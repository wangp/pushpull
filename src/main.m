%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module database.
:- import_module gettimeofday.
:- import_module imap.
:- import_module imap.types.
:- import_module log.
:- import_module maildir.
:- import_module prog_config.
:- import_module signal.
:- import_module sync.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ignore_sigpipe(yes, !IO),
    io.command_line_arguments(Args, !IO),
    ( Args = [DbFileName, Maildir, HostPort, UserName, Password, Mailbox] ->
        Config = prog_config(DbFileName, maildir(Maildir), HostPort,
            username(UserName), password(Password), mailbox(Mailbox)),
        open_database(DbFileName, ResOpenDb, !IO),
        (
            ResOpenDb = ok(Db),
            main_2(Config, Db, !IO),
            close_database(Db, !IO)
        ;
            ResOpenDb = error(Error),
            report_error(Error, !IO)
        )
    ;
        report_error("unexpected arguments", !IO)
    ).

:- pred main_2(prog_config::in, database::in, io::di, io::uo) is det.

main_2(Config, Db, !IO) :-
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
            logged_in(Config, Db, IMAP, !IO)
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

:- pred logged_in(prog_config::in, database::in, imap::in, io::di, io::uo)
    is det.

logged_in(Config, Db, IMAP, !IO) :-
    RemoteMailboxName = Config ^ mailbox,
    select(IMAP, RemoteMailboxName, result(ResExamine, Text, Alerts), !IO),
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
            MaybeHighestModSeqValue = yes(highestmodseq(_))
        ->
            % For now.
            Config ^ maildir = maildir(Maildir),
            LocalMailboxPath = local_mailbox_path(Maildir),

            insert_or_ignore_mailbox_pair(Db, LocalMailboxPath,
                RemoteMailboxName, UIDValidity, ResInsert, !IO),
            (
                ResInsert = ok,
                lookup_mailbox_pair(Db, LocalMailboxPath, RemoteMailboxName,
                    UIDValidity, ResLookup, !IO),
                (
                    ResLookup = found(MailboxPair, LastModSeqValzer),
                    sync_and_repeat(Config, Db, IMAP, MailboxPair,
                        LastModSeqValzer, !IO)
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

:- pred sync_and_repeat(prog_config::in, database::in, imap::in,
    mailbox_pair::in, mod_seq_valzer::in, io::di, io::uo) is det.

sync_and_repeat(Config, Db, IMAP, MailboxPair, LastModSeqValzer, !IO) :-
    % This is the highest MODSEQ value that we know of, which could be higher
    % by now.
    get_selected_mailbox_highest_modseqvalue(IMAP, MaybeHighestModSeqValue,
        !IO),
    ( MaybeHighestModSeqValue = yes(highestmodseq(HighestModSeqValue)) ->
        LastModSeqValzer = mod_seq_valzer(Low),
        HighestModSeqValue = mod_seq_value(High),
        io.format("Synchronising from MODSEQ %s (highest MODSEQ >= %s)\n",
            [s(to_string(Low)), s(to_string(High))], !IO),

        sync_mailboxes(Config, Db, IMAP, MailboxPair, LastModSeqValzer,
            HighestModSeqValue, ResSync, !IO),
        (
            ResSync = ok,
            idling(IMAP, ResIdle, !IO),
            (
                ResIdle = ok,
                update_selected_mailbox_highest_modseqvalue_from_fetches(IMAP,
                    !IO),
                sync_and_repeat(Config, Db, IMAP, MailboxPair,
                    mod_seq_valzer(High), !IO)
            ;
                ResIdle = error(Error),
                report_error(Error, !IO)
            )
        ;
            ResSync = error(Error),
            report_error(Error, !IO)
        )
    ;
        report_error("Cannot support this server.", !IO)
    ).

:- pred idling(imap::in, maybe_error::out, io::di, io::uo) is det.

idling(IMAP, Res, !IO) :-
    % Send IDLE command.
    gettimeofday(StartTime, _StartUsec, !IO),
    idle(IMAP, result(Res0, IdleText, IdleAlerts), !IO),
    report_alerts(IdleAlerts, !IO),
    (
        Res0 = continue,
        io.write_string(IdleText, !IO),
        io.nl(!IO),
        idling_loop(IMAP, StartTime, Res1, !IO),
        (
            (
                Res1 = ready
            ;
                Res1 = timeout
            ),
            % Send IDLE DONE.
            idle_done(IMAP, result(ResDone, DoneText, DoneAlerts), !IO),
            report_alerts(DoneAlerts, !IO),
            (
                ResDone = ok,
                (
                    Res1 = ready,
                    Res = ok
                ;
                    Res1 = timeout,
                    % Restart IDLE.
                    idling(IMAP, Res, !IO)
                )
            ;
                ( ResDone = no
                ; ResDone = bad
                ; ResDone = bye
                ; ResDone = continue
                ; ResDone = error
                ),
                Res = error("unexpected response to IDLE DONE: " ++ DoneText)
            )
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        Res0 = no,
        Res = error("IDLE not supported: " ++ IdleText)
    ;
        ( Res0 = ok
        ; Res0 = bad
        ; Res0 = bye
        ; Res0 = error
        ),
        Res = error("unexpected response to IDLE: " ++ IdleText)
    ).

:- pred idling_loop(imap::in, int::in, select_result::out, io::di, io::uo)
    is det.

idling_loop(IMAP, StartTime, Res, !IO) :-
    gettimeofday(Now, _, !IO),
    TimeoutSecs = StartTime - Now + idle_timeout_secs,
    ( TimeoutSecs > 0 ->
        select_read(IMAP, TimeoutSecs, Res0, !IO)
    ;
        Res0 = timeout
    ),
    (
        Res0 = ready,
        read_single_idle_response(IMAP, Res1, !IO),
        (
            Res1 = stop_idling(Alerts),
            report_alerts(Alerts, !IO),
            Res = ready
        ;
            Res1 = continue_idling(Alerts),
            report_alerts(Alerts, !IO),
            idling_loop(IMAP, StartTime, Res, !IO)
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        Res0 = timeout,
        Res = timeout
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- func idle_timeout_secs = int.

idle_timeout_secs = 29 * 60.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
