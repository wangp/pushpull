%-----------------------------------------------------------------------------%

:- module sync.update_remote.
:- interface.

:- import_module io.

    % Update the database's knowledge of the remote mailbox state,
    % since the last known mod-sequence-value.
    %
:- pred update_db_remote_mailbox(log::in, prog_config::in, database::in,
    imap::in, mailbox_pair::in, mod_seq_valzer::in, mod_seq_value::in,
    maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module enum.
:- import_module diet.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module unit.

:- import_module binary_string.
:- import_module database.
:- import_module flag_delta.
:- import_module log.
:- import_module log_help.
:- import_module message_file.
:- import_module string_util.

:- type make_remote_message_info_result
    --->    yes(uid, remote_message_info)
    ;       no
    ;       error(string).

:- type remote_message_info
    --->    remote_message_info(
                message_id  :: maybe_message_id,
                flags       :: set(flag)    % does not include \Recent
            ).

%-----------------------------------------------------------------------------%

update_db_remote_mailbox(Log, _Config, Db, IMAP, MailboxPair, LastModSeqValzer,
        HighestModSeqValue, Res, !IO) :-
    update_db_remote_mailbox_state(Log, Db, IMAP, MailboxPair,
        LastModSeqValzer, HighestModSeqValue, Res0, !IO),
    (
        Res0 = ok,
        detect_remote_message_expunges(Log, Db, IMAP, MailboxPair, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

    % Update the database's knowledge of the remote mailbox state,
    % since the last known mod-sequence-value.
    %
:- pred update_db_remote_mailbox_state(log::in, database::in, imap::in,
    mailbox_pair::in, mod_seq_valzer::in, mod_seq_value::in, maybe_error::out,
    io::di, io::uo) is det.

update_db_remote_mailbox_state(Log, Db, IMAP, MailboxPair, LastModSeqValzer,
        HighestModSeqValue, Res, !IO) :-
    % Find the UIDs in the mailbox so we can decide how to batch large
    % UID FETCH ranges.
    % The search return option forces the server to return UIDs using
    % sequence-set syntax (RFC 4731).
    uid_search(IMAP, modseq(LastModSeqValzer), yes([all]),
        result(ResSearch, Text, Alerts), !IO),
    report_alerts(Log, Alerts, !IO),
    (
        ResSearch = ok_with_data(uid_search_result(_UIDs,
            _HighestModSeqValueOfFound, ReturnDatas)),
        ( get_all_uids_diet(ReturnDatas, KnownUIDs) ->
            update_db_remote_mailbox_state_2(Log, Db, IMAP, MailboxPair,
                KnownUIDs, LastModSeqValzer, HighestModSeqValue, Res, !IO)
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

:- pred update_db_remote_mailbox_state_2(log::in, database::in, imap::in,
    mailbox_pair::in, diet(uid)::in, mod_seq_valzer::in, mod_seq_value::in,
    maybe_error::out, io::di, io::uo) is det.

update_db_remote_mailbox_state_2(Log, Db, IMAP, MailboxPair, KnownUIDs,
        LastModSeqValzer, HighestModSeqValue, Res, !IO) :-
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
    % Find MidUID where we were interrupted previously.
    search_max_uid_more_recent_than(Db, MailboxPair, LastModSeqValzer,
        ResMidUID, !IO),
    (
        ResMidUID = ok(yes(MidUID)),
        % Update (MidUID+1):*
        update_uid_range(Log, Db, IMAP, MailboxPair, KnownUIDs,
            plus_one(MidUID), no, LastModSeqValzer, HighestModSeqValue,
            ResUpdate0, !IO),
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
                update_uid_range_then_finalise(Log, Db, IMAP, MailboxPair,
                    KnownUIDs, uid(one), yes(MidUID), SinceModSeq,
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
        update_uid_range_then_finalise(Log, Db, IMAP, MailboxPair, KnownUIDs,
            uid(one), no, LastModSeqValzer, HighestModSeqValue, Res, !IO)
    ;
        ResMidUID = error(Error),
        Res = error(Error)
    ).

:- pred update_uid_range_then_finalise(log::in, database::in, imap::in,
    mailbox_pair::in, diet(uid)::in, uid::in, maybe(uid)::in,
    mod_seq_valzer::in, mod_seq_value::in, maybe_error::out, io::di, io::uo)
    is det.

update_uid_range_then_finalise(Log, Db, IMAP, MailboxPair, KnownUIDs,
        RangeMin, RangeMax, SinceModSeqValzer, HighestModSeqValue, Res, !IO) :-
    update_uid_range(Log, Db, IMAP, MailboxPair, KnownUIDs, RangeMin, RangeMax,
        SinceModSeqValzer, HighestModSeqValue, Res0, !IO),
    (
        Res0 = ok,
        update_remote_mailbox_modseqvalue(Db, MailboxPair, HighestModSeqValue,
            Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_uid_range(log::in, database::in, imap::in, mailbox_pair::in,
    diet(uid)::in, uid::in, maybe(uid)::in, mod_seq_valzer::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

update_uid_range(Log, Db, IMAP, MailboxPair, KnownUIDs, BatchMin, RangeMax,
        SinceModSeqValzer, HighestModSeqValue, Res, !IO) :-
    get_batch_max(KnownUIDs, BatchMin, RangeMax, BatchMax, MaybeNextBatchMin),
    update_uid_range_batch(Log, Db, IMAP, MailboxPair, number(BatchMin),
        BatchMax, SinceModSeqValzer, HighestModSeqValue, Res0, !IO),
    (
        Res0 = ok,
        (
            MaybeNextBatchMin = yes(NextBatchMin),
            update_uid_range(Log, Db, IMAP, MailboxPair, KnownUIDs,
                NextBatchMin, RangeMax, SinceModSeqValzer, HighestModSeqValue,
                Res, !IO)
        ;
            MaybeNextBatchMin = no,
            Res = ok
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred get_batch_max(diet(uid)::in, uid::in, maybe(uid)::in,
    seq_number(uid)::out, maybe(uid)::out) is det.

get_batch_max(KnownUIDs, uid(BatchMin), MaybeRangeMax,
        BatchMax, MaybeNextBatchMin) :-
    (
        MaybeRangeMax = yes(RangeMax),
        ResultSet = intersect(KnownUIDs,
            make_interval_set(uid(BatchMin), RangeMax))
    ;
        MaybeRangeMax = no,
        ResultSet = difference(KnownUIDs,
            make_interval_set(uid(zero), uid(BatchMin - one)))
    ),
    ( count(ResultSet) =< max_batch_size ->
        BatchMax = star,
        MaybeNextBatchMin = no
    ;
        BatchMaxUID = BatchMin + integer(max_batch_size - 1),
        NextBatchMinUID = BatchMin + integer(max_batch_size),
        BatchMax = number(uid(BatchMaxUID)),
        MaybeNextBatchMin = yes(uid(NextBatchMinUID))
    ).

:- func max_batch_size = int.

max_batch_size = 4000.

:- pred update_uid_range_batch(log::in, database::in, imap::in,
    mailbox_pair::in, seq_number(uid)::in, seq_number(uid)::in,
    mod_seq_valzer::in, mod_seq_value::in, maybe_error::out, io::di, io::uo)
    is det.

update_uid_range_batch(Log, Db, IMAP, MailboxPair, BatchMin, BatchMax,
        SinceModSeqValzer, HighestModSeqValue, Res, !IO) :-
    log_debug(Log, format("Update UID range %s:%s",
        [s(to_string(BatchMin)), s(to_string(BatchMax))]), !IO),
    SequenceSet = last(range(BatchMin, BatchMax)),
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
    report_alerts(Log, Alerts, !IO),
    (
        ResFetch = ok_with_data(FetchResults),
        log_debug(Log, Text, !IO),
        make_remote_message_infos(FetchResults, ResParse,
            map.init, RemoteMessageInfos),
        (
            ResParse = ok,
            % Transaction around this for faster.
            transaction(
                update_db_with_remote_message_infos(Log, Db, MailboxPair,
                    HighestModSeqValue, RemoteMessageInfos),
                    Db, Res0, !IO),
            (
                Res0 = ok(commit(_ : unit)),
                Res = ok
            ;
                Res0 = ok(rollback(Error)),
                Res = error(Error)
            ;
                Res0 = ok(rollback_exception(Excp)),
                Res = error(string(Excp))
            ;
                Res0 = error(Error),
                Res = error(Error)
            )
        ;
            ResParse = error(Error),
            Res = error(Error)
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

:- func to_string(seq_number(uid)) = string.

to_string(number(uid(N))) = to_string(N).
to_string(star) = "*".

:- pred make_remote_message_infos( list(pair(message_seq_nr, msg_atts))::in,
    maybe_error::out, map(uid, remote_message_info)::in, map(uid,
    remote_message_info)::out) is det.

make_remote_message_infos([], ok, !Map).
make_remote_message_infos([H | T], Res, !Map) :-
    make_remote_message_info(H, ResMake),
    (
        ResMake = yes(UID, Info),
        ( map.insert(UID, Info, !Map) ->
            make_remote_message_infos(T, Res, !Map)
        ;
            % I guess the server should not send multiple results for the same
            % UID.
            Res = error("duplicate UID in FETCH result")
        )
    ;
        ResMake = no,
        make_remote_message_infos(T, Res, !Map)
    ;
        ResMake = error(Error),
        Res = error(Error)
    ).

:- pred make_remote_message_info(pair(message_seq_nr, msg_atts)::in,
    make_remote_message_info_result::out) is det.

make_remote_message_info(_MsgSeqNr - Atts, Res) :-
    % If changes are being made on the remote mailbox concurrently the server
    % (at least Dovecot) may send unsolicited FETCH responses which are not
    % directly in response to our FETCH request, and therefore not matching the
    % items that we asked for.  Ignore those, though we could make use to
    % augment preceding FETCH responses.
    ( solutions((pred(U::out) is nondet :- member(uid(U), Atts)), [UID]) ->
        (
            solutions(
                (pred(NString0::out) is nondet :-
                    member(Att, Atts),
                    is_message_id_att(Att, NString0)
                ),
                [NString])
        ->
            parse_message_id_att(NString, ResMaybeMessageId),
            (
                ResMaybeMessageId = ok(MaybeMessageId),
                (
                    solutions(
                        (pred(F::out) is nondet :- member(flags(F), Atts)),
                        [Flags0]),
                    list.filter_map(flag_except_recent, Flags0, Flags1),
                    set.list_to_set(Flags1, Flags)
                ->
                    Info = remote_message_info(MaybeMessageId, Flags),
                    Res = yes(UID, Info)
                ;
                    Res = error("missing or unexpected flags in FETCH result")
                )
            ;
                ResMaybeMessageId = error(Error),
                Res = error("bad Message-Id in FETCH result: " ++ Error)
            )
        ;
            Res = error("missing Message-Id in FETCH result")
        )
    ;
        Res = no
    ).

:- pred is_message_id_att(msg_att::in, nstring::out) is semidet.

is_message_id_att(Att, NString) :-
    Att = body(msgtext(header_fields(astring(FieldName), [])), no, NString),
    strcase_equal(FieldName, "Message-Id").

:- pred parse_message_id_att(nstring::in, maybe_error(maybe_message_id)::out)
    is det.

parse_message_id_att(NString, Res) :-
    (
        (
            NString = yes(quoted(S)),
            Content = binary_string.from_string(S)
        ;
            NString = yes(literal(Content))
        ),
        read_message_id_from_message_crlf(message(Content), ReadMessageId),
        (
            ReadMessageId = yes(message_id(MessageId)),
            Res = ok(message_id(MessageId))
        ;
            ReadMessageId = no,
            Res = ok(nil)
        ;
            ReadMessageId = format_error(Error),
            Res = error(Error)
        ;
            ReadMessageId = error(Error),
            Res = error(Error)
        )
    ;
        NString = no,
        Res = ok(nil)
    ).

:- pred flag_except_recent(flag_fetch::in, flag::out) is semidet.

flag_except_recent(flag(Flag), Flag).
flag_except_recent(recent, _) :- fail.

:- pred update_db_with_remote_message_infos(log::in, database::in,
    mailbox_pair::in, mod_seq_value::in, map(uid, remote_message_info)::in,
    transaction_result(unit, string)::out, io::di, io::uo) is det.

update_db_with_remote_message_infos(Log, Db, MailboxPair, HighestModSeqValue,
        RemoteMessageInfos, Res, !IO) :-
    map.count(RemoteMessageInfos, Total),
    % We require lower UIDs to be updated before higher UIDs in the database.
    map.foldl3(
        update_db_with_remote_message_info(Log, Db, MailboxPair,
            HighestModSeqValue, Total),
        RemoteMessageInfos, 1, _Count, ok, Res0, !IO),
    (
        Res0 = ok,
        Res = commit(unit)
    ;
        Res0 = error(Error),
        Res = rollback(Error)
    ).

:- pred update_db_with_remote_message_info(log::in, database::in,
    mailbox_pair::in, mod_seq_value::in, int::in,
    uid::in, remote_message_info::in, int::in, int::out,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

update_db_with_remote_message_info(Log, Db, MailboxPair, HighestModSeqValue,
        Total, UID, RemoteMessageInfo, Count, Count + 1,
        MaybeError0, MaybeError, !IO) :-
    (
        MaybeError0 = ok,
        UID = uid(UIDInteger),
        log_debug(Log, format("Updating UID %s (%d of %d)\n",
            [s(to_string(UIDInteger)), i(Count), i(Total)]), !IO),
        do_update_db_with_remote_message_info(Db, MailboxPair,
            HighestModSeqValue, UID, RemoteMessageInfo, MaybeError, !IO)
    ;
        MaybeError0 = error(Error),
        MaybeError = error(Error)
    ).

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

:- pred detect_remote_message_expunges(log::in, database::in, imap::in,
    mailbox_pair::in, maybe_error::out, io::di, io::uo) is det.

detect_remote_message_expunges(Log, Db, IMAP, MailboxPair, Res, !IO) :-
    % The search return option forces the server to return UIDs using
    % sequence-set syntax (RFC 4731).
    % XXX might be able to reuse UID set from update_db_remote_mailbox_state
    uid_search(IMAP, all, yes([all]), result(ResSearch, Text, Alerts), !IO),
    report_alerts(Log, Alerts, !IO),
    (
        ResSearch = ok_with_data(uid_search_result(_UIDs,
            _HighestModSeqValueOfFound, ReturnDatas)),
        ( get_all_uids_set(ReturnDatas, MaybeSequenceSet) ->
            search_min_max_uid(Db, MailboxPair, ResMinMaxUID, !IO),
            (
                ResMinMaxUID = ok(yes({DbMinUID, DbMaxUID})),
                mark_expunged_remote_messages(Log, Db, MailboxPair,
                    DbMinUID, DbMaxUID, MaybeSequenceSet, Res, !IO)
            ;
                ResMinMaxUID = ok(no),
                % Mailbox already empty.
                Res = ok
            ;
                ResMinMaxUID = error(Error),
                Res = error(Error)
            )
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

:- pred mark_expunged_remote_messages(log::in, database::in, mailbox_pair::in,
    uid::in, uid::in, maybe(sequence_set(uid))::in, maybe_error::out,
    io::di, io::uo) is det.

mark_expunged_remote_messages(Log, Db, MailboxPair, DbMinUID, DbMaxUID,
        MaybeSequenceSet, Res, !IO) :-
    begin_detect_expunge(Db, Res0, !IO),
    (
        Res0 = ok(InsertStmt),
        insert_inverse(Db, InsertStmt, MaybeSequenceSet,
            DbMinUID, DbMaxUID, Res1, !IO),
        (
            Res1 = ok,
            mark_expunged_remote_messages(Db, MailboxPair, Res2, !IO),
            (
                Res2 = ok(Count),
                ( Count = 0 ->
                    Level = debug
                ;
                    Level = info
                ),
                log(Log, Level,
                    format("Detected %d expunged remote messages.\n",
                        [i(Count)]), !IO)
            ;
                Res2 = error(_)
            )
        ;
            Res1 = error(Error1),
            Res2 = error(Error1)
        ),
        (
            Res2 = ok(_),
            end_detect_expunge(Db, InsertStmt, Res, !IO)
        ;
            Res2 = error(Error),
            Res = error(Error),
            end_detect_expunge(Db, InsertStmt, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

    % XXX diet uses int internally but UIDs may overflow 32-bit signed ints
:- instance enum(uid) where [
    from_int(Int) = uid(integer(Int)) :- Int > 0,
    to_int(uid(Integer)) = Int :-
    (
        ( string.to_int(to_string(Integer), IntPrime) ->
            Int = IntPrime
        ;
            sorry($module, $pred,
                "UID to int conversion failed (probably overflow)")
        )
    )
].

:- pred get_all_uids_diet(list(search_return_data(uid))::in, diet(uid)::out)
    is semidet.

get_all_uids_diet(ReturnDatas, Diet) :-
    ( ReturnDatas = [] ->
        diet.init(Diet)
    ;
        solutions((pred(Set::out) is nondet :- member(all(Set), ReturnDatas)),
            [SequenceSet]),
        sequence_set_to_diet(SequenceSet, Diet)
    ).

:- pred sequence_set_to_diet(sequence_set(uid)::in, diet(uid)::out) is semidet.

sequence_set_to_diet(last(Elem), Diet) :-
    sequence_set_element_to_diet(Elem, Diet).
sequence_set_to_diet(cons(Elem, Set), Diet) :-
    sequence_set_element_to_diet(Elem, DietA),
    sequence_set_to_diet(Set, DietB),
    diet.union(DietA, DietB, Diet).

:- pred sequence_set_element_to_diet(sequence_set_element(uid)::in,
    diet(uid)::out) is semidet.

sequence_set_element_to_diet(Elem, Diet) :-
    (
        Elem = element(number(UID)),
        Diet = diet.make_singleton_set(UID)
    ;
        Elem = range(number(uid(X)), number(uid(Y))),
        ( X < Y ->
            Diet = diet.make_interval_set(uid(X), uid(Y))
        ;
            Diet = diet.make_interval_set(uid(Y), uid(X))
        )
    ).

    % XXX inefficient, to replace with diet
:- pred get_all_uids_set(list(search_return_data(uid))::in,
    maybe(sequence_set(uid))::out) is semidet.

get_all_uids_set(ReturnDatas, MaybeSequenceSet) :-
    ( ReturnDatas = [] ->
        % Empty mailbox.
        MaybeSequenceSet = no
    ;
        solutions((pred(Set::out) is nondet :- member(all(Set), ReturnDatas)),
            [SequenceSet]),
        MaybeSequenceSet = yes(SequenceSet),
        sequence_set_only_numbers(SequenceSet)
    ).

    % XXX inefficient, to replace with diet
:- pred insert_inverse(database::in, insert_into_detect_expunge_stmt::in,
    maybe(sequence_set(uid))::in, uid::in, uid::in, maybe_error::out,
    io::di, io::uo) is det.

insert_inverse(Db, Stmt, MaybeSequenceSet, UID, DbMaxUID, Res, !IO) :-
    ( UID =< DbMaxUID ->
        (
            MaybeSequenceSet = yes(SequenceSet),
            sequence_set_contains(SequenceSet, UID)
        ->
            Res0 = ok
        ;
            detect_expunge_insert_uid(Db, Stmt, UID, Res0, !IO)
        ),
        (
            Res0 = ok,
            insert_inverse(Db, Stmt, MaybeSequenceSet, plus_one(UID), DbMaxUID,
                Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = ok
    ).

:- pred sequence_set_contains(sequence_set(uid)::in, uid::in) is semidet.

sequence_set_contains(SequenceSet, UID) :-
    (
        SequenceSet = cons(Elem, Tail),
        (
            element_contains(Elem, UID)
        ;
            sequence_set_contains(Tail, UID)
        )
    ;
        SequenceSet = last(Elem),
        element_contains(Elem, UID)
    ).

:- pred element_contains(sequence_set_element(uid)::in, uid::in) is semidet.

element_contains(Element, UID) :-
    (
        Element = element(number(UID))
    ;
        Element = range(number(Low), number(High)),
        Low =< UID, UID =< High
    ).

:- pred uid =< uid.
:- mode in =< in is semidet.

uid(X) =< uid(Y) :-
    X =< Y.

:- func plus_one(uid) = uid.

plus_one(uid(N)) = uid(N + one).

:- func max(mod_seq_valzer, mod_seq_valzer) = mod_seq_valzer.

max(mod_seq_valzer(X), mod_seq_valzer(Y)) = mod_seq_valzer(Max) :-
    Max = ( X >= Y -> X ; Y ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
