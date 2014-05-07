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
:- import_module pretty_printer.
:- import_module solutions.
:- import_module string.

:- import_module database.
:- import_module flag_delta.
:- import_module imap.
:- import_module imap.types.
:- import_module maildir.
:- import_module signal.

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
                message_id  :: message_id,  % may be NIL
                flags       :: list(flag)   % does not include \Recent
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
                    update_db_remote_mailbox(Config, Db, IMAP, LocalMailbox,
                        RemoteMailbox, LastModSeqValzer, HighestModSeqValue,
                        ResUpdate, !IO),
                    (
                        ResUpdate = ok,
                        download_unpaired_remote_messages(Config, Db, IMAP,
                            RemoteMailbox, ResDownload, !IO),
                        (
                            ResDownload = ok
                        ;
                            ResDownload = error(Error),
                            report_error(Error, !IO)
                        )
                    ;
                        ResUpdate = error(Error),
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
    list.filter_map(flag_except_recent, Flags0, Flags),

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
        FlagDeltas = update_flag_deltas(FlagDeltas0, Flags),
        update_remote_message_flags(Db, PairingId, FlagDeltas,
            require_attn(FlagDeltas), MaybeError, !IO)
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
    imap::in, remote_mailbox::in, maybe_error::out, io::di, io::uo) is det.

download_unpaired_remote_messages(Config, Database, IMAP, RemoteMailbox,
        Res, !IO) :-
    search_pairings_without_local_message(Database, RemoteMailbox,
        ResSearch, !IO),
    (
        ResSearch = ok(UnpairedUIDs),
        download_messages(Config, Database, IMAP, RemoteMailbox, UnpairedUIDs,
            Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred download_messages(prog_config::in, database::in, imap::in,
    remote_mailbox::in, assoc_list(pairing_id, uid)::in, maybe_error::out,
    io::di, io::uo) is det.

download_messages(Config, Database, IMAP, RemoteMailbox, UnpairedUIDs, Res,
        !IO) :-
    (
        UnpairedUIDs = [],
        Res = ok
    ;
        UnpairedUIDs = [PairingId - UID | Rest],
        download_message(Config, Database, IMAP, RemoteMailbox, PairingId, UID,
            Res0, !IO),
        (
            Res0 = ok,
            download_messages(Config, Database, IMAP, RemoteMailbox, Rest, Res,
                !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred download_message(prog_config::in, database::in, imap::in,
    remote_mailbox::in, pairing_id::in, uid::in, maybe_error::out,
    io::di, io::uo) is det.

download_message(Config, Database, IMAP, _RemoteMailbox, PairingId, UID, Res,
        !IO) :-
    % Need FLAGS for Maildir filename.
    % MODSEQ could be used to update remote_message row.
    Items = atts(rfc822, [flags, modseq]),
    uid_fetch(IMAP, singleton_sequence_set(UID), Items, no,
        result(ResFetch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResFetch = ok_with_data(FetchResults),
        ( find_uid_fetch_result(FetchResults, UID, Atts) ->
            (
                get_rfc822_att(Atts, RawMessage),
                get_flags(Atts, Flags)
            ->
                save_raw_message(Config, UID, RawMessage, Flags, ResSave, !IO),
                (
                    ResSave = ok(Unique),
                    set_pairing_local_message(Database, PairingId, Unique,
                        Flags, Res, !IO)
                ;
                    ResSave = error(Error),
                    Res = error(Error)
                )
            ;
                Res = error("missing RFC822 response")
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

:- pred get_rfc822_att(msg_atts::in, imap_string::out) is semidet.

get_rfc822_att(Atts, String) :-
    solutions(pred(X::out) is nondet :- member(rfc822(X), Atts), [NString]),
    NString = yes(String).

:- pred get_flags(msg_atts::in, list(flag)::out) is semidet.

get_flags(Atts, Flags) :-
    solutions(pred(X::out) is nondet :- member(flags(X), Atts), [Flags0]),
    list.filter_map(flag_except_recent, Flags0, Flags).

:- pred save_raw_message(prog_config::in, uid::in, imap_string::in,
    list(flag)::in, maybe_error(uniquename)::out, io::di, io::uo) is det.

save_raw_message(Config, uid(UID), RawMessage, Flags, Res, !IO) :-
    Config ^ maildir = maildir(Maildir),
    TmpDirName = Maildir / "tmp",
    generate_unique_name(TmpDirName, ResUnique, !IO),
    (
        ResUnique = ok(Unique),
        Unique = uniquename(UniqueString),
        TmpPath = TmpDirName / UniqueString,
        InfoSuffix = info_suffix(Flags),
        ( InfoSuffix = empty_info_suffix ->
            DestPath = Maildir / "new" / UniqueString
        ;
            DestPath = Maildir / "cur" / UniqueString ++ InfoSuffix
        ),
        io.format("Saving message UID %s to %s\n",
            [s(to_string(UID)), s(DestPath)], !IO),
        io.open_output(TmpPath, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            % XXX catch exceptions during write and fsync
            write_imap_string(Stream, RawMessage, !IO),
            io.close_output(Stream, !IO),
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
            ResOpen = error(Error),
            io.remove_file(TmpPath, _, !IO),
            Res = error(io.error_message(Error))
        )
    ;
        ResUnique = error(Error),
        Res = error(Error)
    ).

:- pred write_imap_string(io.output_stream::in, imap_string::in,
    io::di, io::uo) is det.

write_imap_string(Stream, IString, !IO) :-
    (
        IString = quoted(String)
    ;
        IString = literal(String)
    ),
    io.write_string(Stream, String, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred do_uid_search(imap::in, list(uid)::out, maybe(mod_seq_value)::out,
    io::di, io::uo) is det.

do_uid_search(IMAP, UIDs, MaybeModSeqValue, !IO) :-
    uid_search(IMAP, modseq(mod_seq_valzer(det_from_string("10"))),
        result(ResSearch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResSearch = ok_with_data(UIDs - MaybeModSeqValue),
        io.write_string(Text, !IO),
        io.nl(!IO),
        io.write_list(UIDs, ", ", write_uid, !IO),
        (
            MaybeModSeqValue = yes(mod_seq_value(ModSeqValue)),
            io.write_string(" (MODSEQ ", !IO),
            io.write_string(to_string(ModSeqValue), !IO),
            io.write_string(")", !IO)
        ;
            MaybeModSeqValue = no
        ),
        io.nl(!IO)
    ;
        ( ResSearch = no
        ; ResSearch = bad
        ; ResSearch = bye
        ; ResSearch = continue
        ; ResSearch = error
        ),
        report_error(Text, !IO),
        UIDs = [],
        MaybeModSeqValue = no
    ).

:- pred do_uid_fetch(imap::in, bool::in, list(uid)::in, io::di, io::uo) is det.

do_uid_fetch(IMAP, LongItems, UIDs, !IO) :-
    ( make_sequence_set(UIDs, Set) ->
        (
            LongItems = yes,
            Items = atts(rfc822, [flags, envelope, modseq])
        ;
            LongItems = no,
            Items = macro(fast)
        ),
        uid_fetch(IMAP, Set, Items, no, result(ResFetch, Text, Alerts), !IO),
        report_alerts(Alerts, !IO)
    ;
        ResFetch = ok_with_data([]),
        Text = ""
    ),
    (
        ResFetch = ok_with_data(FetchResults),
        list.foldl(write_fetch_result, FetchResults, !IO)
    ;
        ( ResFetch = no
        ; ResFetch = bad
        ; ResFetch = bye
        ; ResFetch = continue
        ; ResFetch = error
        ),
        report_error(Text, !IO)
    ).

:- pred do_idle(imap::in, maybe(mod_seq_value)::in, io::di, io::uo) is det.

do_idle(IMAP, KnownModSeqValue, !IO) :-
    idle(IMAP, result(ResIdle, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResIdle = continue,
        io.write_string(Text, !IO),
        io.nl(!IO),
        select_read(IMAP, 30, SelectResult, !IO),
        idle_done(IMAP, result(ResDone, TextDone, AlertsDone), !IO),
        report_alerts(AlertsDone, !IO),
        (
            ResDone = ok,
            (
                SelectResult = ready,
                search_changes(IMAP, KnownModSeqValue, !IO)
            ;
                SelectResult = timeout
            ;
                SelectResult = error
            )
        ;
            ( ResDone = no
            ; ResDone = bad
            ; ResDone = bye
            ; ResDone = error
            ; ResDone = continue
            ),
            report_error(TextDone, !IO)
        )
    ;
        ( ResIdle = ok
        ; ResIdle = no
        ; ResIdle = bad
        ; ResIdle = bye
        ; ResIdle = error
        ),
        report_error(Text, !IO)
    ).

:- pred search_changes(imap::in, maybe(mod_seq_value)::in, io::di, io::uo)
    is det.

search_changes(IMAP, KnownModSeqValue, !IO) :-
    (
        KnownModSeqValue = yes(mod_seq_value(K)),
        SearchKey = modseq(mod_seq_valzer(K))
    ;
        KnownModSeqValue = no,
        % Fallback - not sure how \Recent works though.
        SearchKey = recent
    ),
    uid_search(IMAP, SearchKey, result(ResSearch, Text, Alerts),
        !IO),
    report_alerts(Alerts, !IO),
    (
        ResSearch = ok_with_data(UIDs - MaybeModSeqValue),
        io.write_string(Text, !IO),
        io.nl(!IO),
        io.write_list(UIDs, ", ", write_uid, !IO),
        (
            MaybeModSeqValue = yes(mod_seq_value(ModSeqValue)),
            io.write_string(" (MODSEQ ", !IO),
            io.write_string(to_string(ModSeqValue), !IO),
            io.write_string(")", !IO)
        ;
            MaybeModSeqValue = no
        ),
        io.nl(!IO),
        do_uid_fetch(IMAP, no, UIDs, !IO),
        do_idle(IMAP, MaybeModSeqValue, !IO)
    ;
        ( ResSearch = no
        ; ResSearch = bad
        ; ResSearch = bye
        ; ResSearch = continue
        ; ResSearch = error
        ),
        report_error(Text, !IO)
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

:- pred write_uid(uid::in, io::di, io::uo) is det.

write_uid(uid(N), !IO) :-
    io.write_string(to_string(N), !IO).

:- pred write_fetch_result(pair(message_seq_nr, msg_atts)::in, io::di, io::uo)
    is det.

write_fetch_result(_MsgSeqNr - Atts, !IO) :-
    io.write_string("Fetched: ", !IO),
    list.foldl(write_msg_att, Atts, !IO),
    io.nl(!IO).

:- pred write_msg_att(msg_att::in, io::di, io::uo) is det.

write_msg_att(Att, !IO) :-
    io.write_string("- ", !IO),
    pretty_printer.write_doc(format(Att), !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
