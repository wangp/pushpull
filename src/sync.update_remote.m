%-----------------------------------------------------------------------------%

:- module sync.update_remote.
:- interface.

:- import_module io.

    % Update the database's knowledge of the remote mailbox state,
    % since the last known mod-sequence-value.
    %
:- pred update_db_remote_mailbox(prog_config::in, database::in, imap::in,
    mailbox_pair::in, mod_seq_valzer::in, mod_seq_value::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred detect_remote_message_expunges(database::in, imap::in,
    mailbox_pair::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- import_module database.
:- import_module flag_delta.
:- import_module log.
:- import_module message_file.
:- import_module string_util.

:- type remote_message_info
    --->    remote_message_info(
                message_id  :: maybe_message_id,
                flags       :: set(flag)    % does not include \Recent
            ).

%-----------------------------------------------------------------------------%

    % To avoid starting over if we are interrupted when processing a large list
    % of messages, we divide flag updates into two ranges, 1:MidUID and
    % (MidUID+1):*
    %
    % Our snapshot of the remote mailbox is at least as recent as
    % LastModSeqValzer.  Individual messages may be more recently updated.
    %
    % Require that UID_a is updated before UID_b if UID_a < UID_b.
    % Then we can find MidUID, the highest UID for which
    % pairing.last_modseqvalzer > mailbox.last_modseqvalzer.
    %
update_db_remote_mailbox(_Config, Db, IMAP, MailboxPair, LastModSeqValzer,
        HighestModSeqValue, Res, !IO) :-
    % Find MidUID where we were interrupted previously.
    search_max_uid_more_recent_than(Db, MailboxPair, LastModSeqValzer,
        ResMidUID, !IO),
    (
        ResMidUID = ok(yes(MidUID)),
        % Update (MidUID+1):*
        update_uid_range(Db, IMAP, MailboxPair, number(plus_one(MidUID)), star,
            LastModSeqValzer, HighestModSeqValue, ResUpdate0, !IO),
        (
            ResUpdate0 = ok,
            % Update 1:MidUID
            % Changes may have occurred after (multiple) interruptions.
            % The messages are at least as up-to-date as LastModSeqValzer,
            % and also at least as up-to-date as the lowest mod-seq-value
            % of all messages in that range.
            search_min_modseq(Db, MailboxPair, MidUID, ResMinModSeq, !IO),
            (
                ResMinModSeq = ok(MinModSeq),
                SinceModSeq = max(LastModSeqValzer, MinModSeq),
                update_uid_range_then_finalise(Db, IMAP, MailboxPair,
                    number(uid(one)), number(MidUID), SinceModSeq,
                    HighestModSeqValue, Res, !IO)
            ;
                ResMinModSeq = error(Error),
                Res = error(Error)
            )
        ;
            ResUpdate0 = error(Error),
            Res = error(Error)
        )
    ;
        ResMidUID = ok(no),
        % Previous update pass was completed.  Update 1:*
        update_uid_range_then_finalise(Db, IMAP, MailboxPair,
            number(uid(one)), star, LastModSeqValzer, HighestModSeqValue,
            Res, !IO)
    ;
        ResMidUID = error(Error),
        Res = error(Error)
    ).

:- pred update_uid_range_then_finalise(database::in, imap::in,
    mailbox_pair::in, seq_number(uid)::in, seq_number(uid)::in,
    mod_seq_valzer::in, mod_seq_value::in, maybe_error::out, io::di, io::uo)
    is det.

update_uid_range_then_finalise(Db, IMAP, MailboxPair, SeqMin, SeqMax,
        SinceModSeqValzer, HighestModSeqValue, Res, !IO) :-
    update_uid_range(Db, IMAP, MailboxPair, SeqMin, SeqMax,
        SinceModSeqValzer, HighestModSeqValue, Res0, !IO),
    (
        Res0 = ok,
        update_remote_mailbox_modseqvalue(Db, MailboxPair, HighestModSeqValue,
            Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- func plus_one(uid) = uid.

plus_one(uid(N)) = uid(N + one).

:- func max(mod_seq_valzer, mod_seq_valzer) = mod_seq_valzer.

max(mod_seq_valzer(X), mod_seq_valzer(Y)) = mod_seq_valzer(Max) :-
    Max = ( X >= Y -> X ; Y ).

%-----------------------------------------------------------------------------%

:- pred update_uid_range(database::in, imap::in, mailbox_pair::in,
    seq_number(uid)::in, seq_number(uid)::in, mod_seq_valzer::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

update_uid_range(Db, IMAP, MailboxPair, SeqMin, SeqMax, SinceModSeqValzer,
        HighestModSeqValue, Res, !IO) :-
    SequenceSet = last(range(SeqMin, SeqMax)),
    % We only need the Message-ID from the envelope and really only for new
    % messages.
    MessageIdField = header_fields(make_astring("Message-Id"), []),
    Items = atts(flags, [body_peek(msgtext(MessageIdField), no)]),
    % Fetch changes since SinceModSeqValzer.
    SinceModSeqValzer = mod_seq_valzer(N),
    ( N = zero ->
        ChangedSinceModifier = no
    ;
        ChangedSinceModifier = yes(changedsince(mod_seq_value(N)))
    ),
    uid_fetch(IMAP, SequenceSet, Items, ChangedSinceModifier,
        result(ResFetch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResFetch = ok_with_data(FetchResults),
        io.write_string(Text, !IO),
        io.nl(!IO),
        (
            list.foldl(make_remote_message_info, FetchResults,
                map.init, RemoteMessageInfos)
        ->
            update_db_with_remote_message_infos(Db, MailboxPair,
                HighestModSeqValue, RemoteMessageInfos, Res, !IO)
        ;
            Res = error("failed in make_remote_message_info")
        )
    ;
        ( ResFetch = no
        ; ResFetch = bad
        ; ResFetch = bye
        ; ResFetch = continue
        ; ResFetch = error
        ),
        Res = error("unexpected response to UID FETCH: " ++ Text)
    ).

:- pred make_remote_message_info(pair(message_seq_nr, msg_atts)::in,
    map(uid, remote_message_info)::in, map(uid, remote_message_info)::out)
    is semidet.

make_remote_message_info(_MsgSeqNr - Atts, !Map) :-
    solutions((pred(U::out) is nondet :- member(uid(U), Atts)),
        [UID]),

    solutions(
        (pred(MaybeMessageId0::out) is nondet :-
            member(Att, Atts),
            is_message_id_att(Att, MaybeMessageId0)
        ),
        [MaybeMessageId]),

    solutions((pred(F::out) is nondet :- member(flags(F), Atts)),
        [Flags0]),
    list.filter_map(flag_except_recent, Flags0, Flags1),
    set.list_to_set(Flags1, Flags),

    % I guess the server should not send multiple results for the same UID.
    Info = remote_message_info(MaybeMessageId, Flags),
    map.insert(UID, Info, !Map).

:- pred is_message_id_att(msg_att::in, maybe_message_id::out) is semidet.

is_message_id_att(Att, MaybeMessageId) :-
    Att = body(msgtext(header_fields(astring(FieldName), [])), no, NString),
    strcase_equal(FieldName, "Message-Id"),
    (
        (
            NString = yes(quoted(S))
        ;
            NString = yes(literal(S))
        ),
        read_message_id_from_message_crlf(S, ReadMessageId),
        (
            ReadMessageId = yes(MessageId),
            MaybeMessageId = message_id(MessageId)
        ;
            ( ReadMessageId = no
            ; ReadMessageId = format_error(_)
            ; ReadMessageId = error(_)
            ),
            unexpected($module, $pred, "failed to parse Message-Id header")
        )
    ;
        NString = no,
        MaybeMessageId = nil
    ).

:- pred flag_except_recent(flag_fetch::in, flag::out) is semidet.

flag_except_recent(flag(Flag), Flag).
flag_except_recent(recent, _) :- fail.

:- pred update_db_with_remote_message_infos(database::in, mailbox_pair::in,
    mod_seq_value::in, map(uid, remote_message_info)::in, maybe_error::out,
    io::di, io::uo) is det.

update_db_with_remote_message_infos(Db, MailboxPair, HighestModSeqValue,
        RemoteMessageInfos, Res, !IO) :-
    % We require lower UIDs to be updated before higher UIDs in the database.
    map.foldl2(
        update_db_with_remote_message_info(Db, MailboxPair,
            HighestModSeqValue),
        RemoteMessageInfos, ok, Res, !IO).

:- pred update_db_with_remote_message_info(database::in, mailbox_pair::in,
    mod_seq_value::in, uid::in, remote_message_info::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

update_db_with_remote_message_info(Db, MailboxPair, HighestModSeqValue,
        UID, RemoteMessageInfo, MaybeError0, MaybeError, !IO) :-
    (
        MaybeError0 = ok,
        UID = uid(UIDInteger),
        io.format("Updating UID %s\n", [s(to_string(UIDInteger))], !IO),
        do_update_db_with_remote_message_info(Db, MailboxPair,
            HighestModSeqValue, UID, RemoteMessageInfo, MaybeError, !IO)
    ;
        MaybeError0 = error(Error),
        MaybeError = error(Error)
    ).

    % XXX probably want a transaction around this
:- pred do_update_db_with_remote_message_info(database::in, mailbox_pair::in,
    mod_seq_value::in, uid::in, remote_message_info::in, maybe_error::out,
    io::di, io::uo) is det.

do_update_db_with_remote_message_info(Db, MailboxPair, HighestModSeqValue,
        UID, RemoteMessageInfo, MaybeError, !IO) :-
    RemoteMessageInfo = remote_message_info(MessageId, Flags),
    search_pairing_by_remote_message(Db, MailboxPair, UID, MessageId,
        MaybeError0, !IO),
    (
        MaybeError0 = ok(yes({PairingId, FlagDeltas0})),
        update_flags(Flags, FlagDeltas0, FlagDeltas, IsChanged),
        (
            IsChanged = yes,
            update_remote_message_flags(Db, PairingId, FlagDeltas,
                require_attn(FlagDeltas), HighestModSeqValue, MaybeError, !IO)
        ;
            IsChanged = no,
            % Consider bumping remote_modseqvalzer?
            MaybeError = ok
        )
    ;
        MaybeError0 = ok(no),
        insert_new_pairing_only_remote_message(Db, MailboxPair, MessageId,
            UID, Flags, HighestModSeqValue, MaybeError1, !IO),
        (
            MaybeError1 = ok,
            MaybeError = ok
        ;
            MaybeError1 = error(Error),
            MaybeError = error(Error)
        )
    ;
        MaybeError0 = error(Error),
        MaybeError = error(Error)
    ).

%-----------------------------------------------------------------------------%

detect_remote_message_expunges(Db, IMAP, MailboxPair, Res, !IO) :-
    % The search return option forces the server to return UIDs using
    % sequence-set syntax (RFC 4731).
    uid_search(IMAP, all, yes([all]), result(ResSearch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResSearch = ok_with_data(uid_search_result(_UIDs,
            _HighestModSeqValueOfFound, ReturnDatas)),
        ( get_all_uids_set(ReturnDatas, MaybeSequenceSet) ->
            mark_expunged_remote_messages(Db, MailboxPair, MaybeSequenceSet,
                Res, !IO)
        ;
            Res = error("expected UID SEARCH response ALL sequence-set")
        )
    ;
        ( ResSearch = no
        ; ResSearch = bad
        ; ResSearch = bye
        ; ResSearch = continue
        ; ResSearch = error
        ),
        Res = error("unexpected response to UID SEARCH: " ++ Text)
    ).

:- pred get_all_uids_set(list(search_return_data(uid))::in,
    maybe(sequence_set(uid))::out) is semidet.

get_all_uids_set(ReturnDatas, MaybeSequenceSet) :-
    ( ReturnDatas = [] ->
        % Empty mailbox.
        MaybeSequenceSet = no
    ;
        solutions((pred(Set::out) is nondet :- member(all(Set), ReturnDatas)),
            [SequenceSet]),
        MaybeSequenceSet = yes(SequenceSet)
    ).

:- pred mark_expunged_remote_messages(database::in, mailbox_pair::in,
    maybe(sequence_set(uid))::in, maybe_error::out, io::di, io::uo) is det.

mark_expunged_remote_messages(Db, MailboxPair, MaybeSequenceSet, Res, !IO) :-
    create_detect_remote_expunge_temp_table(Db, Res0, !IO),
    (
        Res0 = ok,
        (
            MaybeSequenceSet = yes(SequenceSet),
            insert_into_detect_remote_expunge_table(Db, SequenceSet, Res1,
                !IO)
        ;
            MaybeSequenceSet = no,
            Res1 = ok
        ),
        (
            Res1 = ok,
            mark_expunged_remote_messages(Db, MailboxPair, Res2, !IO),
            (
                Res2 = ok(Count),
                io.format("Detected %d expunged remote messages.\n",
                    [i(Count)], !IO)
            ;
                Res2 = error(_)
            )
        ;
            Res1 = error(Error1),
            Res2 = error(Error1)
        ),
        (
            Res2 = ok(_),
            drop_detect_remote_expunge_temp_table(Db, Res, !IO)
        ;
            Res2 = error(Error),
            Res = error(Error),
            drop_detect_remote_expunge_temp_table(Db, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred insert_into_detect_remote_expunge_table(database::in,
    sequence_set(uid)::in, maybe_error::out, io::di, io::uo) is det.

insert_into_detect_remote_expunge_table(Db, SequenceSet, Res, !IO) :-
    (
        SequenceSet = last(Elem),
        insert_element_into_detect_remote_expunge_table(Db, Elem, Res, !IO)
    ;
        SequenceSet = cons(Head, Tail),
        insert_element_into_detect_remote_expunge_table(Db, Head, Res0, !IO),
        (
            Res0 = ok,
            insert_into_detect_remote_expunge_table(Db, Tail, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred insert_element_into_detect_remote_expunge_table(database::in,
    sequence_set_element(uid)::in, maybe_error::out, io::di, io::uo) is det.

insert_element_into_detect_remote_expunge_table(Db, Elem, Res, !IO) :-
    (
        Elem = element(SeqNumber),
        insert_seqnr_into_detect_remote_expunge_table(Db, SeqNumber, Res, !IO)
    ;
        Elem = range(Low, High),
        (
            Low = number(LowUID),
            High = number(HighUID)
        ->
            insert_range_into_detect_remote_expunge_table(Db, LowUID, HighUID,
                Res, !IO)
        ;
            Res = error("UID range contains *")
        )
    ).

:- pred insert_seqnr_into_detect_remote_expunge_table(database::in,
    seq_number(uid)::in, maybe_error::out, io::di, io::uo) is det.

insert_seqnr_into_detect_remote_expunge_table(Db, SeqNumber, Res, !IO) :-
    (
        SeqNumber = number(UID),
        insert_into_detect_remote_expunge_table(Db, UID, Res, !IO)
    ;
        SeqNumber = star,
        Res = error("UID range contains *")
    ).

:- pred insert_range_into_detect_remote_expunge_table(database::in,
    uid::in, uid::in, maybe_error::out, io::di, io::uo) is det.

insert_range_into_detect_remote_expunge_table(Db, uid(Low), uid(High), Res,
        !IO) :-
    % Low, High are inclusive.
    ( Low =< High ->
        insert_into_detect_remote_expunge_table(Db, uid(Low), Res0, !IO),
        (
            Res0 = ok,
            insert_range_into_detect_remote_expunge_table(Db,
                uid(Low + one), uid(High), Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = ok
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
