%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module time.

:- import_module database.
:- import_module dir_cache.
:- import_module flag_delta.
:- import_module imap.
:- import_module imap.time.
:- import_module imap.types.
:- import_module maildir.
:- import_module message_file.
:- import_module signal.
:- import_module string_util.
:- import_module utime.
:- import_module verify_file.

:- type prog_config
    --->    prog_config(
                db_filename :: string,
                maildir     :: maildir,
                hostport    :: string,
                username    :: username,
                password    :: password,
                mailbox     :: mailbox
            ).

:- type maildir
    --->    maildir(string).

:- type remote_message_info
    --->    remote_message_info(
                message_id  :: maybe_message_id,
                flags       :: set(flag)    % does not include \Recent
            ).

:- type file_data
    --->    file_data(
                content :: string,
                modtime :: time_t
            ).

%-----------------------------------------------------------------------------%

main(!IO) :-
    ignore_sigpipe(yes, !IO),
    io.command_line_arguments(Args, !IO),
    ( Args = [DbFileName, Maildir, HostPort, UserName, Password] ->
        Config = prog_config(DbFileName, maildir(Maildir), HostPort,
            username(UserName), password(Password), mailbox("INBOX")),
        open_database(DbFileName, ResOpenDb, !IO),
        (
            ResOpenDb = ok(Db),
            main_1(Config, Db, !IO),
            close_database(Db, !IO)
        ;
            ResOpenDb = error(Error),
            report_error(Error, !IO)
        )
    ;
        report_error("unexpected arguments", !IO)
    ).

:- pred main_1(prog_config::in, database::in, io::di, io::uo) is det.

main_1(Config, Db, !IO) :-
    Config ^ maildir = maildir(MaildirRoot),
    LocalMailboxPath = local_mailbox_path(MaildirRoot),
    insert_or_ignore_local_mailbox(Db, LocalMailboxPath, ResInsert, !IO),
    (
        ResInsert = ok,
        lookup_local_mailbox(Db, LocalMailboxPath, ResLookup, !IO),
        (
            ResLookup = ok(LocalMailbox),
            main_2(Config, Db, LocalMailbox, !IO)
        ;
            ResLookup = error(Error),
            report_error(Error, !IO)
        )
    ;
        ResInsert = error(Error),
        report_error(Error, !IO)
    ).

:- pred main_2(prog_config::in, database::in, local_mailbox::in,
    io::di, io::uo) is det.

main_2(Config, Db, LocalMailbox, !IO) :-
    HostPort = Config ^ hostport,
    UserName = Config ^ username,
    Password = Config ^ password,
    imap.open(HostPort, ResOpen, OpenAlerts, !IO),
    report_alerts(OpenAlerts, !IO),
    (
        ResOpen = ok(IMAP),
        login(IMAP, UserName, Password,
            result(ResLogin, LoginMessage, LoginAlerts), !IO),
        report_alerts(LoginAlerts, !IO),
        (
            ResLogin = ok,
            io.write_string(LoginMessage, !IO),
            io.nl(!IO),
            logged_in(Config, Db, IMAP, LocalMailbox, !IO)
        ;
            ( ResLogin = no
            ; ResLogin = bad
            ; ResLogin = bye
            ; ResLogin = continue
            ; ResLogin = error
            ),
            report_error(LoginMessage, !IO)
        ),
        logout(IMAP, ResLogout, !IO),
        io.write(ResLogout, !IO),
        io.nl(!IO)
    ;
        ResOpen = error(Error),
        report_error(Error, !IO)
    ).

:- pred logged_in(prog_config::in, database::in, imap::in, local_mailbox::in,
    io::di, io::uo) is det.

logged_in(Config, Db, IMAP, LocalMailbox, !IO) :-
    MailboxName = Config ^ mailbox,
    select(IMAP, MailboxName, result(ResExamine, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResExamine = ok,
        io.write_string(Text, !IO),
        io.nl(!IO),

        get_selected_mailbox_uidvalidity(IMAP, MaybeUIDValidity, !IO),
        get_selected_mailbox_highest_modseqvalue(IMAP,
            MaybeHighestModSeqValue, !IO),
        (
            MaybeUIDValidity = yes(UIDValidity),
            MaybeHighestModSeqValue = yes(highestmodseq(HighestModSeqValue))
        ->
            insert_or_ignore_remote_mailbox(Db, MailboxName, UIDValidity,
                ResInsert, !IO),
            (
                ResInsert = ok,
                lookup_remote_mailbox(Db, MailboxName, UIDValidity, ResLookup,
                    !IO),
                (
                    ResLookup = found(RemoteMailbox, LastModSeqValzer),
                    have_remote_mailbox(Config, Db, IMAP, LocalMailbox,
                        RemoteMailbox, LastModSeqValzer, HighestModSeqValue,
                        Res, !IO),
                    (
                        Res = ok
                    ;
                        Res = error(Error),
                        report_error(Error, !IO)
                    )
                ;
                    ResLookup = error(Error),
                    report_error(Error, !IO)
                )
            ;
                ResInsert = error(Error),
                report_error(Error, !IO)
            )
        ;
            report_error("Cannot support this server.", !IO)
        )
    ;
        ( ResExamine = no
        ; ResExamine = bad
        ; ResExamine = bye
        ; ResExamine = continue
        ; ResExamine = error
        ),
        report_error(Text, !IO)
    ).

:- pred have_remote_mailbox(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, mod_seq_valzer::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

have_remote_mailbox(Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        LastModSeqValzer, HighestModSeqValue, Res, !IO) :-
    % It might be better to get set of valid UIDs first, then use that
    % as part of update_db_remote_mailbox and for detecting expunges.
    update_db_remote_mailbox(Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        LastModSeqValzer, HighestModSeqValue, ResUpdate, !IO),
    (
        ResUpdate = ok,
        detect_remote_message_expunges(Db, IMAP, LocalMailbox, RemoteMailbox,
            ResRemoteExpunges, !IO),
        (
            ResRemoteExpunges = ok,
            update_db_local_mailbox(Db, LocalMailbox, RemoteMailbox,
                ResUpdateLocal, !IO),
            (
                ResUpdateLocal = ok(DirCache),
                % Propagate flags first to allow pairings with
                % previously-expunged messages to be be reset, and thus
                % downloaded in the following steps.
                propagate_flag_deltas_from_remote(Config, Db, LocalMailbox,
                    RemoteMailbox, DirCache, ResPropRemote, !IO),
                (
                    ResPropRemote = ok,
                    propagate_flag_deltas_from_local(Config, Db, IMAP,
                        LocalMailbox, RemoteMailbox, ResPropLocal, !IO),
                    (
                        ResPropLocal = ok,
                        download_unpaired_remote_messages(Config, Db, IMAP,
                            LocalMailbox, RemoteMailbox, DirCache, ResDownload,
                            !IO),
                        % DirCache does not include newly added messages.
                        (
                            ResDownload = ok,
                            upload_unpaired_local_messages(Config, Db, IMAP,
                                LocalMailbox, RemoteMailbox, DirCache,
                                ResUpload, !IO),
                            (
                                ResUpload = ok,
                                delete_expunged_pairings(Db, Res, !IO)
                            ;
                                ResUpload = error(Error),
                                Res = error(Error)
                            )
                        ;
                            ResDownload = error(Error),
                            Res = error(Error)
                        )
                    ;
                        ResPropLocal = error(Error),
                        Res = error(Error)
                    )
                ;
                    ResPropRemote = error(Error),
                    Res = error(Error)
                )
            ;
                ResUpdateLocal = error(Error),
                Res = error(Error)
            )
        ;
            ResRemoteExpunges = error(Error),
            Res = error(Error)
        )
    ;
        ResUpdate = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred update_db_local_mailbox(database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error(dir_cache)::out, io::di, io::uo) is det.

update_db_local_mailbox(Db, LocalMailbox, RemoteMailbox, Res, !IO) :-
    update_file_list(get_local_mailbox_path(LocalMailbox), init, ResCache,
        !IO),
    (
        ResCache = ok(DirCache),
        LocalMailboxPath = get_local_mailbox_path(LocalMailbox),
        list_files(DirCache, LocalMailboxPath, NewFiles, CurFiles),

        LocalMailboxPath = local_mailbox_path(DirName),
        update_db_local_message_files(Db, LocalMailbox, RemoteMailbox,
            DirName / "new", NewFiles, Res1, !IO),
        (
            Res1 = ok,
            update_db_local_message_files(Db, LocalMailbox, RemoteMailbox,
                DirName / "cur", CurFiles, Res2, !IO),
            (
                Res2 = ok,
                mark_expunged_local_messages(Db, LocalMailbox, RemoteMailbox,
                    NewFiles, CurFiles, Res3, !IO),
                (
                    Res3 = ok,
                    Res = ok(DirCache)
                ;
                    Res3 = error(Error),
                    Res = error(Error)
                )
            ;
                Res2 = error(Error),
                Res = error(Error)
            )
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        ResCache = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_files(database::in, local_mailbox::in,
    remote_mailbox::in, string::in, list(string)::in, maybe_error::out,
    io::di, io::uo) is det.

update_db_local_message_files(_Db, _LocalMailbox, _RemoteMailbox, _DirName,
        [], ok, !IO).
update_db_local_message_files(Db, LocalMailbox, RemoteMailbox, DirName,
        [BaseName | BaseNames], Res, !IO) :-
    update_db_local_message_file(Db, LocalMailbox, RemoteMailbox,
        DirName, BaseName, Res0, !IO),
    (
        Res0 = ok,
        update_db_local_message_files(Db, LocalMailbox, RemoteMailbox, DirName,
            BaseNames, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_file(database::in, local_mailbox::in,
    remote_mailbox::in, string::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

update_db_local_message_file(Db, LocalMailbox, RemoteMailbox, DirName,
        BaseName, Res, !IO) :-
    ( parse_basename(BaseName, Unique, Flags) ->
        update_db_local_message_file_2(Db, LocalMailbox, RemoteMailbox,
            DirName, BaseName, Unique, Flags, Res, !IO)
    ;
        % Should just skip.
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

:- pred update_db_local_message_file_2(database::in, local_mailbox::in,
    remote_mailbox::in, string::in, string::in, uniquename::in, set(flag)::in,
    maybe_error::out, io::di, io::uo) is det.

update_db_local_message_file_2(Db, LocalMailbox, RemoteMailbox, DirName,
        BaseName, Unique, Flags, Res, !IO) :-
    search_pairing_by_local_message(Db, LocalMailbox, Unique, ResSearch, !IO),
    (
        ResSearch = ok(yes({PairingId, LocalFlagDeltas0})),
        update_maildir_standard_flags(Flags, LocalFlagDeltas0, LocalFlagDeltas,
            IsChanged),
        (
            IsChanged = yes,
            Unique = uniquename(UniqueString),
            io.format("Updating local message flags %s: %s\n",
                [s(UniqueString), s(string(LocalFlagDeltas))], !IO),
            update_local_message_flags(Db, PairingId, LocalFlagDeltas,
                require_attn(LocalFlagDeltas), Res, !IO)
        ;
            IsChanged = no,
            Res = ok
        )
    ;
        ResSearch = ok(no),
        Path = DirName / BaseName,
        read_message_id_from_file(Path, ResRead, !IO),
        (
            ResRead = yes(MessageId),
            % There may already be an unpaired remote message in the database
            % for this local message.  We will download the remote message to
            % compare, as we don't want to assume that Message-Ids are unique.
            % At that time we will notice that the local and remote message
            % contents match, then delete one of the two pairings (this one) in
            % favour of the other.
            insert_new_pairing_only_local_message(Db, message_id(MessageId),
                LocalMailbox, RemoteMailbox, Unique, Flags, Res, !IO)
        ;
            ResRead = no,
            % XXX we could add this file to the database, but we want to have
            % more indicators that it really is a message file
            io.format("skipping %s: missing Message-Id\n", [s(BaseName)], !IO),
            Res = ok
        ;
            ResRead = format_error(Error),
            io.format("skipping %s: %s\n", [s(BaseName), s(Error)], !IO),
            Res = ok
        ;
            ResRead = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred mark_expunged_local_messages(database::in, local_mailbox::in,
    remote_mailbox::in, list(string)::in, list(string)::in, maybe_error::out,
    io::di, io::uo) is det.

mark_expunged_local_messages(Db, LocalMailbox, RemoteMailbox,
        NewFiles, CurFiles, Res, !IO) :-
    create_detect_local_expunge_temp_table(Db, Res0, !IO),
    (
        Res0 = ok,
        insert_into_detect_local_expunge_table(Db, NewFiles, Res1, !IO),
        (
            Res1 = ok,
            insert_into_detect_local_expunge_table(Db, CurFiles, Res2, !IO),
            (
                Res2 = ok,
                mark_expunged_local_messages(Db, LocalMailbox, RemoteMailbox,
                    Res3, !IO),
                (
                    Res3 = ok(Count),
                    io.format("Detected %d expunged local messages.\n",
                        [i(Count)], !IO)
                ;
                    Res3 = error(_)
                )
            ;
                Res2 = error(Error2),
                Res3 = error(Error2)
            )
        ;
            Res1 = error(Error1),
            Res3 = error(Error1)
        ),
        (
            Res3 = ok(_),
            drop_detect_local_expunge_temp_table(Db, Res, !IO)
        ;
            Res3 = error(Error),
            Res = error(Error),
            drop_detect_local_expunge_temp_table(Db, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred insert_into_detect_local_expunge_table(database::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

insert_into_detect_local_expunge_table(_Db, [], ok, !IO).
insert_into_detect_local_expunge_table(Db, [BaseName | BaseNames], Res, !IO) :-
    ( parse_basename(BaseName, Unique, _Flags) ->
        insert_into_detect_local_expunge_table(Db, Unique, Res0, !IO),
        (
            Res0 = ok,
            insert_into_detect_local_expunge_table(Db, BaseNames, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

%-----------------------------------------------------------------------------%

    % Update the database's knowledge of the remote mailbox state,
    % since the last known mod-sequence-value.
    %
:- pred update_db_remote_mailbox(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, mod_seq_valzer::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

update_db_remote_mailbox(_Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        LastModSeqValzer, HighestModSeqValue, Res, !IO) :-
    SequenceSet = last(range(number(uid(one)), star)),
    % We only need the Message-ID from the envelope and really only for new
    % messages.
    MessageIdField = header_fields(make_astring("Message-Id"), []),
    Items = atts(flags, [body_peek(msgtext(MessageIdField), no)]),
    % Fetch changes since LastModSeqValzer.
    LastModSeqValzer = mod_seq_valzer(N),
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
            update_db_with_remote_message_infos(Db, LocalMailbox,
                RemoteMailbox, RemoteMessageInfos, HighestModSeqValue,
                Res, !IO)
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
    map.insert(UID, remote_message_info(MaybeMessageId, Flags), !Map).

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

:- pred update_db_with_remote_message_infos(database::in, local_mailbox::in,
    remote_mailbox::in, map(uid, remote_message_info)::in, mod_seq_value::in,
    maybe_error::out, io::di, io::uo) is det.

update_db_with_remote_message_infos(Db, LocalMailbox, RemoteMailbox,
        RemoteMessageInfos, ModSeqValue, Res, !IO) :-
    map.foldl2(
        update_db_with_remote_message_info(Db, LocalMailbox, RemoteMailbox),
        RemoteMessageInfos, ok, Res0, !IO),
    (
        Res0 = ok,
        update_remote_mailbox_modseqvalue(Db, RemoteMailbox, ModSeqValue,
            Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_db_with_remote_message_info(database::in, local_mailbox::in,
    remote_mailbox::in, uid::in, remote_message_info::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

update_db_with_remote_message_info(Db, LocalMailbox, RemoteMailbox, UID,
        RemoteMessageInfo, MaybeError0, MaybeError, !IO) :-
    (
        MaybeError0 = ok,
        do_update_db_with_remote_message_info(Db, LocalMailbox, RemoteMailbox,
            UID, RemoteMessageInfo, MaybeError, !IO)
    ;
        MaybeError0 = error(Error),
        MaybeError = error(Error)
    ).

    % XXX probably want a transaction around this
:- pred do_update_db_with_remote_message_info(database::in, local_mailbox::in,
    remote_mailbox::in, uid::in, remote_message_info::in, maybe_error::out,
    io::di, io::uo) is det.

do_update_db_with_remote_message_info(Db, LocalMailbox, RemoteMailbox, UID,
        RemoteMessageInfo, MaybeError, !IO) :-
    RemoteMessageInfo = remote_message_info(MessageId, Flags),
    search_pairing_by_remote_message(Db, RemoteMailbox, UID, MessageId,
        MaybeError0, !IO),
    (
        MaybeError0 = ok(yes({PairingId, FlagDeltas0})),
        update_flags(Flags, FlagDeltas0, FlagDeltas, IsChanged),
        (
            IsChanged = yes,
            update_remote_message_flags(Db, PairingId, FlagDeltas,
                require_attn(FlagDeltas), MaybeError, !IO)
        ;
            IsChanged = no,
            MaybeError = ok
        )
    ;
        MaybeError0 = ok(no),
        insert_new_pairing_only_remote_message(Db, MessageId, LocalMailbox,
            RemoteMailbox, UID, Flags, MaybeError1, !IO),
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

:- pred detect_remote_message_expunges(database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, maybe_error::out, io::di, io::uo)
    is det.

detect_remote_message_expunges(Db, IMAP, LocalMailbox, RemoteMailbox, Res, !IO)
        :-
    % The search return option forces the server to return UIDs using
    % sequence-set syntax (RFC 4731).
    uid_search(IMAP, all, yes([all]), result(ResSearch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResSearch = ok_with_data(uid_search_result(_UIDs,
            _HighestModSeqValueOfFound, ReturnDatas)),
        ( get_all_uids_set(ReturnDatas, MaybeSequenceSet) ->
            mark_expunged_remote_messages(Db, LocalMailbox, RemoteMailbox,
                MaybeSequenceSet, Res, !IO)
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

:- pred mark_expunged_remote_messages(database::in, local_mailbox::in,
    remote_mailbox::in, maybe(sequence_set(uid))::in, maybe_error::out,
    io::di, io::uo) is det.

mark_expunged_remote_messages(Db, LocalMailbox, RemoteMailbox,
        MaybeSequenceSet, Res, !IO) :-
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
            mark_expunged_remote_messages(Db, LocalMailbox, RemoteMailbox,
                Res2, !IO),
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

:- pred download_unpaired_remote_messages(prog_config::in, database::in,
    imap::in, local_mailbox::in, remote_mailbox::in, dir_cache::in,
    maybe_error::out, io::di, io::uo) is det.

download_unpaired_remote_messages(Config, Database, IMAP, LocalMailbox,
        RemoteMailbox, DirCache, Res, !IO) :-
    search_unpaired_remote_messages(Database, RemoteMailbox, ResSearch, !IO),
    (
        ResSearch = ok(Unpaireds),
        download_messages(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
            DirCache, Unpaireds, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred download_messages(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in,
    list(unpaired_remote_message)::in, maybe_error::out, io::di, io::uo)
    is det.

download_messages(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
        DirCache, Unpaireds, Res, !IO) :-
    (
        Unpaireds = [],
        Res = ok
    ;
        Unpaireds = [H | T],
        download_message(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
            DirCache, H, Res0, !IO),
        (
            Res0 = ok,
            download_messages(Config, Database, IMAP, LocalMailbox,
                RemoteMailbox, DirCache, T, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred download_message(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in,
    unpaired_remote_message::in, maybe_error::out, io::di, io::uo) is det.

download_message(Config, Database, IMAP, LocalMailbox, _RemoteMailbox,
        DirCache, UnpairedRemote, Res, !IO) :-
    UnpairedRemote = unpaired_remote_message(_PairingId, UID,
        ExpectedMessageId),
    % Need FLAGS for Maildir filename.
    % INTERNALDATE for setting mtime on new files.
    % MODSEQ could be used to update remote_message row.
    Items = atts(rfc822, [flags, modseq, internaldate]),
    uid_fetch(IMAP, singleton_sequence_set(UID), Items, no,
        result(ResFetch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResFetch = ok_with_data(FetchResults),
        ( find_uid_fetch_result(FetchResults, UID, Atts) ->
            (
                get_rfc822_att(Atts, RawMessageCrLf),
                get_flags(Atts, Flags),
                get_internaldate(Atts, InternalDate)
            ->
                read_message_id_from_message_crlf(RawMessageCrLf, ResMessageId),
                (
                    (
                        ResMessageId = yes(ReadMessageId),
                        HaveMessageId = message_id(ReadMessageId)
                    ;
                        ResMessageId = no,
                        HaveMessageId = nil
                    ),
                    ( ExpectedMessageId = HaveMessageId ->
                        RawMessageLf = crlf_to_lf(RawMessageCrLf),
                        save_message_and_pair(Config, Database, LocalMailbox,
                            UnpairedRemote, RawMessageLf, Flags, InternalDate,
                            DirCache, Res, !IO)
                    ;
                        Res = error("unexpected Message-Id")
                    )
                ;
                    ResMessageId = format_error(Error),
                    Res = error("format error in RFC822 response: " ++ Error)
                ;
                    ResMessageId = error(Error),
                    Res = error(io.error_message(Error))
                )
            ;
                Res = error("problem with UID FETCH response")
            )
        ;
            % Message no longer exists on server?
            Res = ok
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

:- pred find_uid_fetch_result(assoc_list(message_seq_nr, msg_atts)::in,
    uid::in, msg_atts::out) is semidet.

find_uid_fetch_result(FetchResults, UID, MsgAtts) :-
    list.find_first_match(
        (pred(_ - Atts::in) is semidet :- list.member(uid(UID), Atts)),
        FetchResults, _MsgSeqNr - MsgAtts).

:- pred get_rfc822_att(msg_atts::in, string::out) is semidet.

get_rfc822_att(Atts, String) :-
    solutions(pred(X::out) is nondet :- member(rfc822(X), Atts), [NString]),
    NString = yes(IString),
    String = from_imap_string(IString).

:- pred get_flags(msg_atts::in, set(flag)::out) is semidet.

get_flags(Atts, Flags) :-
    solutions(pred(X::out) is nondet :- member(flags(X), Atts), [Flags0]),
    list.filter_map(flag_except_recent, Flags0, Flags1),
    set.list_to_set(Flags1, Flags).

:- pred get_internaldate(msg_atts::in, date_time::out) is semidet.

get_internaldate(Atts, DateTime) :-
    solutions(pred(X::out) is nondet :- member(internaldate(X), Atts),
        [DateTime]).

:- pred save_message_and_pair(prog_config::in, database::in, local_mailbox::in,
    unpaired_remote_message::in, string::in, set(flag)::in, date_time::in,
    dir_cache::in, maybe_error::out, io::di, io::uo) is det.

save_message_and_pair(_Config, Database, LocalMailbox, UnpairedRemote,
        RawMessageLf, Flags, InternalDate, DirCache, Res, !IO) :-
    UnpairedRemote = unpaired_remote_message(PairingId, UID, MessageId),
    % Avoid duplicating an existing local message.
    match_unpaired_local_message(Database, LocalMailbox, DirCache,
        MessageId, RawMessageLf, ResMatch, !IO),
    (
        ResMatch = ok(no),
        save_raw_message(LocalMailbox, UID, RawMessageLf, Flags, InternalDate,
            ResSave, !IO),
        (
            ResSave = ok(Unique),
            set_pairing_local_message(Database, PairingId, Unique,
                init_flags(Flags), Res, !IO)
        ;
            ResSave = error(Error),
            Res = error(Error)
        )
    ;
        ResMatch = ok(yes(UnpairedLocal)),
        UnpairedLocal = unpaired_local_message(OtherPairingId, Unique),
        % XXX use a transaction around this sequence
        lookup_local_message_flags(Database, OtherPairingId, ResLocalFlags,
            !IO),
        delete_pairing(Database, OtherPairingId, ResDeleteOther, !IO),
        (
            ResDeleteOther = ok,
            (
                ResLocalFlags = ok(LocalFlags),
                set_pairing_local_message(Database, PairingId, Unique,
                    LocalFlags, Res, !IO)
            ;
                ResLocalFlags = error(Error),
                Res = error(Error)
            )
        ;
            ResDeleteOther = error(Error),
            Res = error(Error)
        )
    ;
        ResMatch = error(Error),
        Res = error(Error)
    ).

:- pred match_unpaired_local_message(database::in, local_mailbox::in,
    dir_cache::in, maybe_message_id::in, string::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

match_unpaired_local_message(Database, LocalMailbox, DirCache, MessageId,
        RawMessageLf, Res, !IO) :-
    search_unpaired_local_messages_by_message_id(Database, LocalMailbox,
        MessageId, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        verify_unpaired_local_messages(LocalMailbox, DirCache, UnpairedLocals,
            RawMessageLf, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred verify_unpaired_local_messages(local_mailbox::in, dir_cache::in,
    list(unpaired_local_message)::in, string::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

verify_unpaired_local_messages(LocalMailbox, DirCache, UnpairedLocals,
        RawMessageLf, Res, !IO) :-
    (
        UnpairedLocals = [],
        Res = ok(no)
    ;
        UnpairedLocals = [UnpairedLocal | RestUnpairedLocals],
        verify_unpaired_local_message(LocalMailbox, DirCache, UnpairedLocal,
            RawMessageLf, ResVerify, !IO),
        (
            ResVerify = ok(yes),
            Res = ok(yes(UnpairedLocal))
        ;
            ResVerify = ok(no),
            verify_unpaired_local_messages(LocalMailbox, DirCache,
                RestUnpairedLocals, RawMessageLf, Res, !IO)
        ;
            ResVerify = error(Error),
            Res = error(Error)
        )
    ).

:- pred verify_unpaired_local_message(local_mailbox::in, dir_cache::in,
    unpaired_local_message::in, string::in, maybe_error(bool)::out,
    io::di, io::uo) is det.

verify_unpaired_local_message(LocalMailbox, DirCache, UnpairedLocal,
        RawMessageLf, Res, !IO) :-
    UnpairedLocal = unpaired_local_message(_PairingId, Unique),
    find_file(DirCache, get_local_mailbox_path(LocalMailbox), Unique, ResFind),
    (
        ResFind = found(Path, _InfoSuffix),
        verify_file(Path, RawMessageLf, Res, !IO)
    ;
        ResFind = found_but_unexpected(Path),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = not_found,
        Res = ok(no)
    ).

:- pred save_raw_message(local_mailbox::in, uid::in, string::in, set(flag)::in,
    date_time::in,  maybe_error(uniquename)::out, io::di, io::uo) is det.

save_raw_message(LocalMailbox, uid(UID), RawMessageLf, Flags, InternalDate,
        Res, !IO) :-
    LocalMailboxPath = get_local_mailbox_path(LocalMailbox),
    generate_unique_tmp_path(LocalMailboxPath, ResUnique, !IO),
    (
        ResUnique = ok({Unique, TmpPath}),
        InfoSuffix = flags_to_info_suffix(Flags),
        % XXX don't think this condition is right
        ( InfoSuffix = info_suffix(set.init, "") ->
            make_path(LocalMailboxPath, new, Unique, no, DestPath)
        ;
            make_path(LocalMailboxPath, cur, Unique, yes(InfoSuffix), DestPath)
        ),
        io.format("Saving message UID %s to %s\n",
            [s(to_string(UID)), s(DestPath)], !IO),
        io.open_output(TmpPath, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            % XXX catch exceptions during write and fsync
            io.write_string(Stream, RawMessageLf, !IO),
            io.close_output(Stream, !IO),
            set_file_atime_mtime(TmpPath, mktime(InternalDate), ResTime, !IO),
            (
                ResTime = ok,
                io.rename_file(TmpPath, DestPath, ResRename, !IO),
                (
                    ResRename = ok,
                    Res = ok(Unique)
                ;
                    ResRename = error(Error),
                    io.remove_file(TmpPath, _, !IO),
                    Res = error(io.error_message(Error))
                )
            ;
                ResTime = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            ResOpen = error(Error),
            io.remove_file(TmpPath, _, !IO),
            Res = error(io.error_message(Error))
        )
    ;
        ResUnique = error(Error),
        Res = error(Error)
    ).

:- func crlf_to_lf(string) = string.

crlf_to_lf(S) = string.replace_all(S, "\r\n", "\n").

%-----------------------------------------------------------------------------%

:- pred upload_unpaired_local_messages(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in, maybe_error::out,
    io::di, io::uo) is det.

upload_unpaired_local_messages(Config, Database, IMAP, LocalMailbox,
        RemoteMailbox, DirCache, Res, !IO) :-
    % Currently we download unpaired remote messages first and try to pair them
    % with existing local messages, so the remaining unpaired local messages
    % should actually not have remote counterparts.
    search_unpaired_local_messages(Database, LocalMailbox, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        upload_messages(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
            DirCache, UnpairedLocals, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred upload_messages(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in,
    list(unpaired_local_message)::in, maybe_error::out, io::di, io::uo) is det.

upload_messages(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
        DirCache, UnpairedLocals, Res, !IO) :-
    (
        UnpairedLocals = [],
        Res = ok
    ;
        UnpairedLocals = [H | T],
        upload_message(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
            DirCache, H, Res0, !IO),
        (
            Res0 = ok,
            upload_messages(Config, Database, IMAP, LocalMailbox,
                RemoteMailbox, DirCache, T, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred upload_message(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in,
    unpaired_local_message::in, maybe_error::out, io::di, io::uo) is det.

upload_message(_Config, Database, IMAP, LocalMailbox, RemoteMailbox,
        DirCache, UnpairedLocal, Res, !IO) :-
    LocalMailboxPath = get_local_mailbox_path(LocalMailbox),
    UnpairedLocal = unpaired_local_message(PairingId, Unique),
    find_file(DirCache, LocalMailboxPath, Unique, ResFind),
    (
        ResFind = found(Path, _InfoSuffix),
        lookup_local_message_flags(Database, PairingId, ResFlags, !IO),
        (
            ResFlags = ok(LocalFlagDeltas),
            get_file_data(Path, ResFileData, !IO),
            (
                ResFileData = ok(FileData),
                LocalFlags = LocalFlagDeltas ^ cur_set,
                do_upload_message(IMAP, RemoteMailbox, FileData, LocalFlags,
                    ResUpload, !IO),
                (
                    ResUpload = ok(MaybeUID),
                    (
                        MaybeUID = yes(UID),
                        % XXX is it okay to assume RemoteFlags?
                        RemoteFlags = init_flags(LocalFlags),
                        set_pairing_remote_message(Database, PairingId, UID,
                            RemoteFlags, Res, !IO)
                    ;
                        MaybeUID = no,
                        % We don't know the UID of the appended message.
                        % At the next sync we should notice the new message
                        % and, after downloading it, pair it up with the local
                        % message.
                        Res = ok
                    )
                ;
                    ResUpload = error(Error),
                    Res = error(Error)
                )
            ;
                ResFileData = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            ResFlags = error(Error),
            Res = error(Error)
        )
    ;
        ResFind = found_but_unexpected(Path),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = not_found,
        % Maybe deleted since.
        Res = ok
    ).

:- pred get_file_data(string::in, io.res(file_data)::out, io::di, io::uo)
    is det.

get_file_data(Path, Res, !IO) :-
    io.file_modification_time(Path, ResTime, !IO),
    (
        ResTime = ok(ModTime),
        io.open_input(Path, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            io.read_file_as_string(Stream, ResRead, !IO),
            io.close_input(Stream, !IO),
            (
                ResRead = ok(Content),
                Res = ok(file_data(Content, ModTime))
            ;
                ResRead = error(_, Error),
                Res = error(Error)
            )
        ;
            ResOpen = error(Error),
            Res = error(Error)
        )
    ;
        ResTime = error(Error),
        Res = error(Error)
    ).

:- pred do_upload_message(imap::in, remote_mailbox::in, file_data::in,
    set(flag)::in, maybe_error(maybe(uid))::out, io::di, io::uo) is det.

do_upload_message(IMAP, RemoteMailbox, FileData, Flags, Res, !IO) :-
    MailboxName = get_remote_mailbox_name(RemoteMailbox),
    UIDValidity = get_remote_mailbox_uidvalidity(RemoteMailbox),
    % Try to get an older mod-seq-value prior to APPEND if appending to the
    % selected mailbox.
    get_selected_mailbox_uidvalidity(IMAP, SelectedUIDValidity, !IO),
    get_selected_mailbox_highest_modseqvalue(IMAP, HighestModSeqValue, !IO),
    (
        SelectedUIDValidity = yes(UIDValidity),
        HighestModSeqValue = yes(highestmodseq(ModSeqValue))
    ->
        PriorModSeqValue = yes(ModSeqValue)
    ;
        PriorModSeqValue = no
    ),

    FileData = file_data(ContentLf, ModTime),
    DateTime = make_date_time(ModTime),
    append(IMAP, MailboxName, to_sorted_list(Flags), yes(DateTime), ContentLf,
        result(ResAppend, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResAppend = ok_with_data(MaybeAppendUID),
        io.write_string(Text, !IO),
        io.nl(!IO),
        get_appended_uid(IMAP, RemoteMailbox, MaybeAppendUID, ContentLf,
            PriorModSeqValue, Res, !IO)
    ;
        ( ResAppend = no
        ; ResAppend = bad
        ; ResAppend = bye
        ; ResAppend = continue
        ; ResAppend = error
        ),
        Res = error("unexpected response to APPEND: " ++ Text)
    ).

:- pred get_appended_uid(imap::in, remote_mailbox::in, maybe(appenduid)::in,
    string::in, maybe(mod_seq_value)::in, maybe_error(maybe(uid))::out,
    io::di, io::uo) is det.

get_appended_uid(IMAP, RemoteMailbox, MaybeAppendUID, ContentLf,
        PriorModSeqValue, Res, !IO) :-
    % In the best case the server sent an APPENDUID response with the UID.
    % Otherwise we search the mailbox for the Message-Id (if it's unique).
    % Otherwise we give up.
    UIDValidity = get_remote_mailbox_uidvalidity(RemoteMailbox),
    (
        MaybeAppendUID = yes(appenduid(UIDValidity, UIDSet)),
        is_singleton_set(UIDSet, UID)
    ->
        Res = ok(yes(UID))
    ;
        get_appended_uid_fallback(IMAP, ContentLf, PriorModSeqValue, Res, !IO)
    ).

:- pred get_appended_uid_fallback(imap::in, string::in,
    maybe(mod_seq_value)::in, maybe_error(maybe(uid))::out, io::di, io::uo)
    is det.

get_appended_uid_fallback(IMAP, ContentLf, PriorModSeqValue, Res, !IO) :-
    read_message_id_from_message_lf(ContentLf, ReadMessageId),
    (
        ReadMessageId = yes(MessageId),
        SearchKey0 = header(make_astring("Message-Id"),
            make_astring(MessageId)),
        % If we know a mod-seq-value prior to the APPEND then we can restrict
        % the search.
        (
            PriorModSeqValue = yes(mod_seq_value(ModSeqValue)),
            SearchKey = and(modseq(mod_seq_valzer(ModSeqValue)), [SearchKey0])
        ;
            PriorModSeqValue = no,
            SearchKey = SearchKey0
        ),
        uid_search(IMAP, SearchKey, no, result(ResSearch, Text, Alerts), !IO),
        report_alerts(Alerts, !IO),
        (
            ResSearch = ok_with_data(uid_search_result(UIDs,
                _HighestModSeqValueOfFound, _ReturnDatas)),
            io.write_string(Text, !IO),
            io.nl(!IO),
            ( UIDs = [UID] ->
                Res = ok(yes(UID))
            ;
                Res = ok(no)
            )
        ;
            ( ResSearch = no
            ; ResSearch = bad
            ; ResSearch = bye
            ; ResSearch = continue
            ; ResSearch = error
            ),
            Res = error("unexpected response to UID SEARCH: " ++ Text)
        )
    ;
        ( ReadMessageId = no
        ; ReadMessageId = format_error(_)
        ; ReadMessageId = error(_)
        ),
        Res = ok(no)
    ).

:- pred is_singleton_set(uid_set::in, uid::out) is semidet.

is_singleton_set(Set, UID) :-
    solutions(
        (pred(X::out) is nondet :- member(uid_range(X, X), Set)),
        [UID]).

%-----------------------------------------------------------------------------%

:- pred propagate_flag_deltas_from_remote(prog_config::in, database::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in, maybe_error::out,
    io::di, io::uo) is det.

propagate_flag_deltas_from_remote(Config, Db, LocalMailbox, RemoteMailbox,
        DirCache, Res, !IO) :-
    search_pending_flag_deltas_from_remote(Db, LocalMailbox, RemoteMailbox,
        ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        list.foldl2(propagate_flag_deltas_from_remote_2(Config, Db,
            LocalMailbox, RemoteMailbox, DirCache), Pendings, ok, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_remote_2(prog_config::in, database::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in,
    pending_flag_deltas::in, maybe_error::in, maybe_error::out, io::di, io::uo)
    is det.

propagate_flag_deltas_from_remote_2(Config, Db, LocalMailbox, RemoteMailbox,
        DirCache, Pending, Res0, Res, !IO) :-
    (
        Res0 = ok,
        propagate_flag_deltas_from_remote_3(Config, Db, LocalMailbox,
            RemoteMailbox, DirCache, Pending, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_remote_3(prog_config::in, database::in,
    local_mailbox::in, remote_mailbox::in, dir_cache::in,
    pending_flag_deltas::in, maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_from_remote_3(_Config, Db, LocalMailbox, _RemoteMailbox,
        DirCache, Pending, Res, !IO) :-
    Pending = pending_flag_deltas(PairingId,
        MaybeUnique, LocalFlags0, LocalExpunged,
        MaybeUID, RemoteFlags0, RemoteExpunged),
    imply_deleted_flag(LocalExpunged, LocalFlags0, LocalFlags1),
    imply_deleted_flag(RemoteExpunged, RemoteFlags0, RemoteFlags1),
    apply_flag_deltas(LocalFlags1, LocalFlags, RemoteFlags1, RemoteFlags),

    Flags0 = LocalFlags0 ^ cur_set,
    Flags = LocalFlags ^ cur_set,
    AddFlags = Flags `difference` Flags0,
    RemoveFlags = Flags0 `difference` Flags,
    (
        MaybeUnique = yes(Unique),
        expect(unify(LocalExpunged, exists), $module, $pred),
        store_local_flags_add_rm(LocalMailbox, DirCache, Unique,
            AddFlags, RemoveFlags, Res0, !IO),
        (
            Res0 = ok,
            record_remote_flag_deltas_applied_to_local(Db, PairingId,
                LocalFlags, RemoteFlags, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        MaybeUnique = no,
        % If the local message was previously expunged, but the remote message
        % was undeleted then reset the pairing so that the message can be
        % re-added to the local mailbox with a new uniquename.
        % XXX simplify?
        (
            MaybeUID = yes(uid(UID)),
            LocalExpunged = expunged,
            contains(RemoveFlags, system(deleted))
        ->
            io.format("Resurrecting message %s\n", [s(to_string(UID))], !IO),
            reset_pairing_local_message(Db, PairingId, Res, !IO)
        ;
            record_remote_flag_deltas_inapplicable_to_local(Db, PairingId,
                RemoteFlags, Res, !IO)
        )
    ).

:- pred store_local_flags_add_rm(local_mailbox::in, dir_cache::in,
    uniquename::in, set(flag)::in, set(flag)::in, maybe_error::out,
    io::di, io::uo) is det.

store_local_flags_add_rm(LocalMailbox, DirCache, Unique, AddFlags, RemoveFlags,
        Res, !IO) :-
    MailboxPath = get_local_mailbox_path(LocalMailbox),
    find_file(DirCache, MailboxPath, Unique, ResFind),
    (
        ResFind = found(OldPath, MaybeInfoSuffix0),
        (
            MaybeInfoSuffix0 = no,
            InfoSuffix = flags_to_info_suffix(AddFlags)
        ;
            MaybeInfoSuffix0 = yes(InfoSuffix0),
            add_remove_standard_flags(AddFlags, RemoveFlags,
                InfoSuffix0, InfoSuffix)
        ),
        make_path(MailboxPath, cur, Unique, yes(InfoSuffix), NewPath),
        ( OldPath = NewPath ->
            ResRename = ok
        ;
            io.format("Renaming %s to %s\n", [s(OldPath), s(NewPath)], !IO),
            io.rename_file(OldPath, NewPath, ResRename, !IO)
        ),
        (
            ResRename = ok,
            Res = ok
        ;
            ResRename = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        ResFind = not_found,
        Unique = uniquename(UniqueString),
        Res = error("missing uniquename " ++ UniqueString)
    ;
        ResFind = found_but_unexpected(Path),
        Res = error("found unique name but unexpected: " ++ Path)
    ).

%-----------------------------------------------------------------------------%

:- pred propagate_flag_deltas_from_local(prog_config::in, database::in,
    imap::in, local_mailbox::in, remote_mailbox::in, maybe_error::out,
    io::di, io::uo) is det.

propagate_flag_deltas_from_local(_Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        Res, !IO) :-
    search_pending_flag_deltas_from_local(Db, LocalMailbox, RemoteMailbox,
        ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        list.foldl2(propagate_flag_deltas_from_local_2(Db, IMAP),
            Pendings, ok, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_local_2(database::in, imap::in,
    pending_flag_deltas::in, maybe_error::in, maybe_error::out, io::di, io::uo)
    is det.

propagate_flag_deltas_from_local_2(Db, IMAP, Pending, Res0, Res, !IO) :-
    (
        Res0 = ok,
        propagate_flag_deltas_from_local_3(Db, IMAP, Pending, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_local_3(database::in, imap::in,
    pending_flag_deltas::in, maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_from_local_3(Db, IMAP, Pending, Res, !IO) :-
    Pending = pending_flag_deltas(PairingId,
        MaybeUnique, LocalFlags0, LocalExpunged,
        MaybeUID, RemoteFlags0, RemoteExpunged),
    imply_deleted_flag(LocalExpunged, LocalFlags0, LocalFlags1),
    imply_deleted_flag(RemoteExpunged, RemoteFlags0, RemoteFlags1),
    apply_flag_deltas(RemoteFlags1, RemoteFlags, LocalFlags1, LocalFlags),

    Flags0 = RemoteFlags0 ^ cur_set,
    Flags = RemoteFlags ^ cur_set,
    AddFlags = Flags `difference` Flags0,
    RemoveFlags = Flags0 `difference` Flags,
    (
        MaybeUID = yes(UID),
        store_remote_flags_add_rm(IMAP, UID, AddFlags, RemoveFlags, Res0, !IO),
        (
            Res0 = ok,
            record_local_flag_deltas_applied_to_remote(Db, PairingId,
                LocalFlags, RemoteFlags, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        MaybeUID = no,
        % If the remote message was previously expunged, but the local message
        % was undeleted, then reset the pairing so that the message can be
        % re-added to the remote mailbox with a new UID.
        (
            MaybeUnique = yes(uniquename(Unique)),
            RemoteExpunged = expunged,
            contains(RemoveFlags, system(deleted))
        ->
            io.format("Resurrecting message %s\n", [s(Unique)], !IO),
            reset_pairing_remote_message(Db, PairingId, Res, !IO)
        ;
            record_local_flag_deltas_inapplicable_to_remote(Db,
                PairingId, LocalFlags, Res, !IO)
        )
    ).

:- pred store_remote_flags_add_rm(imap::in, uid::in,
    set(flag)::in, set(flag)::in, maybe_error::out, io::di, io::uo) is det.

store_remote_flags_add_rm(IMAP, UID, AddFlags, RemoveFlags, Res, !IO) :-
    % Would it be preferable to read back the actual flags from the server?
    store_remote_flags_change(IMAP, UID, remove, RemoveFlags, Res0, !IO),
    (
        Res0 = ok,
        store_remote_flags_change(IMAP, UID, add, AddFlags, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred store_remote_flags_change(imap::in, uid::in, store_operation::in,
    set(flag)::in, maybe_error::out, io::di, io::uo) is det.

store_remote_flags_change(IMAP, UID, Operation, ChangeFlags, Res, !IO) :-
    ( set.empty(ChangeFlags) ->
        Res = ok
    ;
        uid_store(IMAP, singleton_sequence_set(UID), Operation, silent,
            to_sorted_list(ChangeFlags), result(ResAdd, Text, Alerts), !IO),
        report_alerts(Alerts, !IO),
        (
            ResAdd = ok_with_data(_),
            Res = ok
        ;
            ( ResAdd = no
            ; ResAdd = bad
            ; ResAdd = bye
            ; ResAdd = continue
            ; ResAdd = error
            ),
            Res = error("unexpected response to UID STORE: " ++ Text)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred report_error(string::in, io::di, io::uo) is det.

report_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).

:- pred report_alerts(list(alert)::in, io::di, io::uo) is det.

report_alerts(Alerts, !IO) :-
    list.foldl(report_alert, Alerts, !IO).

:- pred report_alert(alert::in, io::di, io::uo) is det.

report_alert(alert(Alert), !IO) :-
    io.write_string("ALERT: ", !IO),
    io.write_string(Alert, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
