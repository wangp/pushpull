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
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- import_module database.
:- import_module flag_delta.
:- import_module imap.
:- import_module imap.time.
:- import_module imap.types.
:- import_module maildir.
:- import_module message_file.
:- import_module signal.
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
    examine(IMAP, MailboxName, result(ResExamine, Text, Alerts), !IO),
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
    update_db_remote_mailbox(Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        LastModSeqValzer, HighestModSeqValue, ResUpdate, !IO),
    (
        ResUpdate = ok,
        update_db_local_mailbox(Db, LocalMailbox, RemoteMailbox,
            ResUpdateLocal, !IO),
        (
            ResUpdateLocal = ok,
            download_unpaired_remote_messages(Config, Db, IMAP, LocalMailbox,
                RemoteMailbox, ResDownload, !IO),
            (
                ResDownload = ok,
                propagate_flag_deltas(Config, Db, LocalMailbox, RemoteMailbox,
                    ResProp, !IO),
                (
                    ResProp = ok,
                    Res = ok
                ;
                    ResProp = error(Error),
                    Res = error(Error)
                )
            ;
                ResDownload = error(Error),
                Res = error(Error)
            )
        ;
            ResUpdateLocal = error(Error),
            Res = error(Error)
        )
    ;
        ResUpdate = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred update_db_local_mailbox(database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error::out, io::di, io::uo) is det.

update_db_local_mailbox(Db, LocalMailbox, RemoteMailbox, Res, !IO) :-
    list_files(get_local_mailbox_path(LocalMailbox), ResList, !IO),
    (
        ResList = ok(Files),
        update_db_local_message_files(Db, LocalMailbox, RemoteMailbox, Files,
            Res, !IO)
    ;
        ResList = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_files(database::in, local_mailbox::in,
    remote_mailbox::in, list(local_file)::in, maybe_error::out, io::di, io::uo)
    is det.

update_db_local_message_files(_Db, _LocalMailbox, _RemoteMailbox, [], ok, !IO).
update_db_local_message_files(Db, LocalMailbox, RemoteMailbox, [File | Files],
        Res, !IO) :-
    update_db_local_message_file(Db, LocalMailbox, RemoteMailbox, File, Res0,
        !IO),
    (
        Res0 = ok,
        update_db_local_message_files(Db, LocalMailbox, RemoteMailbox, Files,
            Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_file(database::in, local_mailbox::in,
    remote_mailbox::in, local_file::in, maybe_error::out, io::di, io::uo)
    is det.

update_db_local_message_file(Db, LocalMailbox, RemoteMailbox, File, Res, !IO) :-
    File = local_file(_Path, BaseName),
    ( parse_basename(BaseName, Unique, Flags) ->
        update_db_local_message_file_2(Db, LocalMailbox, RemoteMailbox, File,
            Unique, Flags, Res, !IO)
    ;
        % Should just skip.
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

:- pred update_db_local_message_file_2(database::in, local_mailbox::in,
    remote_mailbox::in, local_file::in, uniquename::in, set(flag)::in,
    maybe_error::out, io::di, io::uo) is det.

update_db_local_message_file_2(Db, LocalMailbox, RemoteMailbox, File, Unique,
        Flags, Res, !IO) :-
    File = local_file(Path, BaseName),
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
        read_message_id(Path, ResRead, !IO),
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

    % Update the database's knowledge of the remote mailbox state,
    % since the last known mod-sequence-value.
    %
:- pred update_db_remote_mailbox(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, mod_seq_valzer::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

update_db_remote_mailbox(_Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        LastModSeqValzer, HighestModSeqValue, Res, !IO) :-
    % Search for changes which came *after* LastModSeqValzer.
    LastModSeqValzer = mod_seq_valzer(N),
    SearchKey = modseq(mod_seq_valzer(N + one)),
    uid_search(IMAP, SearchKey, result(ResSearch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResSearch = ok_with_data(UIDs - _HighestModSeqValueOfFound),
        io.write_string(Text, !IO),
        io.nl(!IO),

        fetch_remote_message_infos(IMAP, UIDs, ResFetch, !IO),
        (
            ResFetch = ok(RemoteMessageInfos),
            update_db_with_remote_message_infos(Db, LocalMailbox,
                RemoteMailbox, RemoteMessageInfos, HighestModSeqValue, Res,
                !IO)
        ;
            ResFetch = error(Error),
            Res = error(Error)
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

:- pred fetch_remote_message_infos(imap::in, list(uid)::in,
    maybe_error(map(uid, remote_message_info))::out, io::di, io::uo) is det.

fetch_remote_message_infos(IMAP, UIDs, Res, !IO) :-
    ( make_sequence_set(UIDs, Set) ->
        % We only need the Message-ID from the envelope and really only for new
        % messages.
        Items = atts(flags, [envelope]),
        uid_fetch(IMAP, Set, Items, no, result(ResFetch, Text, Alerts),
            !IO),
        report_alerts(Alerts, !IO),
        (
            ResFetch = ok_with_data(AssocList),
            io.write_string(Text, !IO),
            io.nl(!IO),
            ( list.foldl(make_remote_message_info, AssocList, map.init, Map) ->
                Res = ok(Map)
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
        )
    ;
        % Empty set.
        Res = ok(map.init)
    ).

:- pred make_remote_message_info(pair(message_seq_nr, msg_atts)::in,
    map(uid, remote_message_info)::in, map(uid, remote_message_info)::out)
    is semidet.

make_remote_message_info(_MsgSeqNr - Atts, !Map) :-
    solutions((pred(U::out) is nondet :- member(uid(U), Atts)),
        [UID]),

    solutions((pred(E::out) is nondet :- member(envelope(E), Atts)),
        [Envelope]),
    MessageId = Envelope ^ message_id,

    solutions((pred(F::out) is nondet :- member(flags(F), Atts)),
        [Flags0]),
    list.filter_map(flag_except_recent, Flags0, Flags1),
    set.list_to_set(Flags1, Flags),

    % I guess the server should not send multiple results for the same UID.
    map.insert(UID, remote_message_info(MessageId, Flags), !Map).

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

:- pred download_unpaired_remote_messages(prog_config::in, database::in,
    imap::in, local_mailbox::in, remote_mailbox::in, maybe_error::out,
    io::di, io::uo) is det.

download_unpaired_remote_messages(Config, Database, IMAP, LocalMailbox,
        RemoteMailbox, Res, !IO) :-
    search_unpaired_remote_messages(Database, RemoteMailbox, ResSearch, !IO),
    (
        ResSearch = ok(Unpaireds),
        download_messages(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
            Unpaireds, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred download_messages(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, list(unpaired_remote_message)::in,
    maybe_error::out, io::di, io::uo) is det.

download_messages(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
        Unpaireds, Res, !IO) :-
    (
        Unpaireds = [],
        Res = ok
    ;
        Unpaireds = [H | T],
        download_message(Config, Database, IMAP, LocalMailbox, RemoteMailbox,
            H, Res0, !IO),
        (
            Res0 = ok,
            download_messages(Config, Database, IMAP, LocalMailbox,
                RemoteMailbox, T, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred download_message(prog_config::in, database::in, imap::in,
    local_mailbox::in, remote_mailbox::in, unpaired_remote_message::in,
    maybe_error::out, io::di, io::uo) is det.

download_message(Config, Database, IMAP, LocalMailbox, _RemoteMailbox,
        UnpairedRemote, Res, !IO) :-
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
                RawMessageLf = crlf_to_lf(RawMessageCrLf),
                read_message_id_from_string(RawMessageLf, ResMessageId),
                (
                    (
                        ResMessageId = yes(ReadMessageId),
                        HaveMessageId = message_id(ReadMessageId)
                    ;
                        ResMessageId = no,
                        HaveMessageId = nil
                    ),
                    ( ExpectedMessageId = HaveMessageId ->
                        save_message_and_pair(Config, Database, LocalMailbox,
                            UnpairedRemote, RawMessageLf, Flags, InternalDate,
                            Res, !IO)
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
    maybe_error::out, io::di, io::uo) is det.

save_message_and_pair(_Config, Database, LocalMailbox, UnpairedRemote,
        RawMessageLf, Flags, InternalDate, Res, !IO) :-
    UnpairedRemote = unpaired_remote_message(PairingId, UID, MessageId),
    % Avoid duplicating an existing local mailbox.
    match_unpaired_local_message(Database, LocalMailbox, MessageId,
        RawMessageLf, ResMatch, !IO),
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
    maybe_message_id::in, string::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

match_unpaired_local_message(Database, LocalMailbox, MessageId, RawMessageLf,
        Res, !IO) :-
    search_unpaired_local_messages_by_message_id(Database, LocalMailbox,
        MessageId, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        verify_unpaired_local_messages(LocalMailbox, UnpairedLocals,
            RawMessageLf, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred verify_unpaired_local_messages(local_mailbox::in,
    list(unpaired_local_message)::in, string::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

verify_unpaired_local_messages(LocalMailbox, UnpairedLocals, RawMessageLf,
        Res, !IO) :-
    (
        UnpairedLocals = [],
        Res = ok(no)
    ;
        UnpairedLocals = [UnpairedLocal | RestUnpairedLocals],
        verify_unpaired_local_message(LocalMailbox, UnpairedLocal,
            RawMessageLf, ResVerify, !IO),
        (
            ResVerify = ok(yes),
            Res = ok(yes(UnpairedLocal))
        ;
            ResVerify = ok(no),
            verify_unpaired_local_messages(LocalMailbox, RestUnpairedLocals,
                RawMessageLf, Res, !IO)
        ;
            ResVerify = error(Error),
            Res = error(Error)
        )
    ).

:- pred verify_unpaired_local_message(local_mailbox::in,
    unpaired_local_message::in, string::in, maybe_error(bool)::out,
    io::di, io::uo) is det.

verify_unpaired_local_message(LocalMailbox, UnpairedLocal, RawMessageLf,
        Res, !IO) :-
    UnpairedLocal = unpaired_local_message(_PairingId, Unique),
    find_file(get_local_mailbox_path(LocalMailbox), Unique, ResFind, !IO),
    (
        ResFind = ok(found(_MailboxPath, Path, _InfoSuffix)),
        verify_file(Path, RawMessageLf, Res, !IO)
    ;
        ResFind = ok(found_but_unexpected(Path)),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = ok(not_found),
        Res = ok(no)
    ;
        ResFind = error(Error),
        Res = error(Error)
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

:- pred propagate_flag_deltas(prog_config::in, database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas(Config, Db, LocalMailbox, RemoteMailbox, Res, !IO) :-
    % For now only from remote to local.
    search_pending_flag_deltas_from_remote(Db, LocalMailbox, RemoteMailbox,
        ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        list.foldl2(propagate_flag_deltas_2(Config, Db, LocalMailbox,
            RemoteMailbox), Pendings, ok, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_2(prog_config::in, database::in,
    local_mailbox::in, remote_mailbox::in, pending_flag_deltas::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_2(Config, Db, LocalMailbox, RemoteMailbox, Pending,
        Res0, Res, !IO) :-
    (
        Res0 = ok,
        propagate_flag_deltas_for_message(Config, Db, LocalMailbox,
            RemoteMailbox, Pending, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_for_message(prog_config::in, database::in,
    local_mailbox::in, remote_mailbox::in, pending_flag_deltas::in,
    maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_for_message(_Config, Db, LocalMailbox, _RemoteMailbox,
        Pending, Res, !IO) :-
    Pending = pending_flag_deltas(_PairingId, Unique, _LocalFlags0, _UID,
        _RemoteFlags0),
    find_file(get_local_mailbox_path(LocalMailbox), Unique, ResFind, !IO),
    (
        ResFind = ok(Found),
        Found = found(_, _, _),
        propagate_flag_deltas_for_message_found(Db, Pending, Found, Res, !IO)
    ;
        ResFind = ok(found_but_unexpected(Path)),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = ok(not_found),
        % Can't find the file; the file was probably deleted since we updated
        % our database state.
        % XXX handle this better
        Unique = uniquename(UniqueString),
        Res = error("missing file with unique name " ++ UniqueString)
    ;
        ResFind = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_for_message_found(database::in,
    pending_flag_deltas::in, find_file_result::in(found), maybe_error::out,
    io::di, io::uo) is det.

propagate_flag_deltas_for_message_found(Db, Pending, Found, Res, !IO) :-
    Pending = pending_flag_deltas(PairingId, Unique, LocalFlags0, _UID,
        RemoteFlags0),
    apply_flag_deltas(LocalFlags0, LocalFlags, RemoteFlags0, RemoteFlags),

    % Maybe we can keep in "new" if InfoSuffix still empty?
    Found = found(FolderPath, OldPath, MaybeInfoSuffix0),
    (
        MaybeInfoSuffix0 = no,
        InfoSuffix = flags_to_info_suffix(LocalFlags ^ cur_set)
    ;
        MaybeInfoSuffix0 = yes(InfoSuffix0),
        update_standard_flags(LocalFlags ^ cur_set, InfoSuffix0, InfoSuffix)
    ),
    make_path(FolderPath, cur, Unique, yes(InfoSuffix), NewPath),
    ( OldPath = NewPath ->
        ResRename = ok
    ;
        io.format("Renaming %s to %s\n", [s(OldPath), s(NewPath)], !IO),
        io.rename_file(OldPath, NewPath, ResRename, !IO)
    ),
    (
        ResRename = ok,
        % XXX think more carefully about a crash occurring now
        record_remote_flag_deltas_applied_to_local(Db, PairingId, LocalFlags,
            RemoteFlags, Res, !IO)
    ;
        ResRename = error(Error),
        Res = error(io.error_message(Error))
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
