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
    idle(IMAP, result(ResIdle, IdleText, IdleAlerts), !IO),
    report_alerts(IdleAlerts, !IO),
    (
        ResIdle = continue,
        io.write_string(IdleText, !IO),
        io.nl(!IO),
        % Idle up to 29 minutes.
        TimeoutSeconds = 29 * 60,
        select_read(IMAP, TimeoutSeconds, ResSelect, !IO),
        (
            (
                ResSelect = ready,
                io.write_string("Leaving idle (ready).\n", !IO)
            ;
                ResSelect = timeout,
                io.write_string("Leaving idle (timeout).\n", !IO)
            ),
            idle_done(IMAP, result(ResDone, DoneText, DoneAlerts), !IO),
            report_alerts(DoneAlerts, !IO),
            (
                ResDone = ok,
                (
                    ResSelect = ready,
                    Res = ok
                ;
                    ResSelect = timeout,
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
            ResSelect = error,
            Res = error("connection closed")
        )
    ;
        ResIdle = no,
        Res = error("IDLE not supported: " ++ IdleText)
    ;
        ( ResIdle = ok
        ; ResIdle = bad
        ; ResIdle = bye
        ; ResIdle = error
        ),
        Res = error("unexpected response to IDLE: " ++ IdleText)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
