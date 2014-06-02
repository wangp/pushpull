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
:- import_module dir_cache.
:- import_module gettimeofday.
:- import_module imap.
:- import_module imap.types.
:- import_module inotify.
:- import_module log.
:- import_module path.
:- import_module prog_config.
:- import_module select.
:- import_module signal.
:- import_module sync.

:- type idle_next
    --->    sync
    ;       restart_idle.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ignore_sigpipe(yes, !IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [DbFileName, MaildirRoot, LocalMailboxName, HostPort, UserName,
            Password, RemoteMailbox]
    ->
        Config = prog_config(DbFileName, maildir_root(MaildirRoot),
            local_mailbox_name(LocalMailboxName), HostPort, username(UserName),
            password(Password), mailbox(RemoteMailbox)),
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
    % For now.
    LocalMailboxName = Config ^ local_mailbox_name,
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
            insert_or_ignore_mailbox_pair(Db, LocalMailboxName,
                RemoteMailboxName, UIDValidity, ResInsert, !IO),
            (
                ResInsert = ok,
                lookup_mailbox_pair(Db, LocalMailboxName, RemoteMailboxName,
                    UIDValidity, ResLookup, !IO),
                (
                    ResLookup = found(MailboxPair, LastModSeqValzer),
                    inotify.init(ResInotify, !IO),
                    (
                        ResInotify = ok(Inotify),
                        LocalMailboxPath = make_local_mailbox_path(Config,
                            LocalMailboxName),
                        LocalMailboxPath = local_mailbox_path(DirName),
                        DirCache0 = dir_cache.init(dirname(DirName)),

                        sync_and_repeat(Config, Db, IMAP, Inotify,
                            MailboxPair, LastModSeqValzer,
                            DirCache0, _DirCache, !IO)
                    ;
                        ResInotify = error(Error),
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

:- pred sync_and_repeat(prog_config::in, database::in, imap::in,
    inotify(S)::in, mailbox_pair::in, mod_seq_valzer::in,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

sync_and_repeat(Config, Db, IMAP, Inotify, MailboxPair, LastModSeqValzer,
        !DirCache, !IO) :-
    % This is the highest MODSEQ value that we know of, which could be higher
    % by now.
    get_selected_mailbox_highest_modseqvalue(IMAP, MaybeHighestModSeqValue,
        !IO),
    ( MaybeHighestModSeqValue = yes(highestmodseq(HighestModSeqValue)) ->
        LastModSeqValzer = mod_seq_valzer(Low),
        HighestModSeqValue = mod_seq_value(High),
        io.format("Synchronising from MODSEQ %s (highest MODSEQ >= %s)\n",
            [s(to_string(Low)), s(to_string(High))], !IO),

        % Clear inotify buffer as of now.
        % Events which come in between now and the end of the sync cycle can
        % cause a potentially unnecessary sync cycle.
        read_all(Inotify, ResClearInotify, !IO),
        (
            ResClearInotify = ok,
            sync_mailboxes(Config, Db, IMAP, MailboxPair, LastModSeqValzer,
                HighestModSeqValue, ResSync, !DirCache, !IO),
            (
                ResSync = ok,
                update_watches(Inotify, !.DirCache, ResWatch, !IO),
                (
                    ResWatch = ok,
                    idle_until_sync(IMAP, Inotify, ResIdle, !IO),
                    (
                        ResIdle = ok,
                        update_selected_mailbox_highest_modseqvalue_from_fetches(
                            IMAP, !IO),
                        sync_and_repeat(Config, Db, IMAP, Inotify, MailboxPair,
                            mod_seq_valzer(High), !DirCache, !IO)
                    ;
                        ResIdle = error(Error),
                        report_error(Error, !IO)
                    )
                ;
                    ResWatch = error(Error),
                    report_error(Error, !IO)
                )
            ;
                ResSync = error(Error),
                report_error(Error, !IO)
            )
        ;
            ResClearInotify = error(Error),
            report_error(Error, !IO)
        )
    ;
        report_error("Cannot support this server.", !IO)
    ).

:- pred idle_until_sync(imap::in, inotify(S)::in, maybe_error::out,
    io::di, io::uo) is det.

idle_until_sync(IMAP, Inotify, Res, !IO) :-
    % Send IDLE command.
    gettimeofday(StartTime, _StartUsec, !IO),
    idle(IMAP, result(Res0, IdleText, IdleAlerts), !IO),
    report_alerts(IdleAlerts, !IO),
    (
        Res0 = continue,
        io.write_string(IdleText, !IO),
        io.nl(!IO),
        idle_until_done(IMAP, Inotify, StartTime, Res1, !IO),
        (
            Res1 = ok(sync),
            sleep(1, !IO),
            Res = ok
        ;
            Res1 = ok(restart_idle),
            idle_until_sync(IMAP, Inotify, Res, !IO)
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

:- pred idle_until_done(imap::in, inotify(S)::in, int::in,
    maybe_error(idle_next)::out, io::di, io::uo) is det.

idle_until_done(IMAP, Inotify, StartTime, Res, !IO) :-
    idle_loop(IMAP, Inotify, StartTime, Res0, !IO),
    (
        Res0 = ok(Next),
        % Send IDLE DONE.
        idle_done(IMAP, result(ResDone, DoneText, DoneAlerts), !IO),
        report_alerts(DoneAlerts, !IO),
        (
            ResDone = ok,
            Res = ok(Next)
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
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred idle_loop(imap::in, inotify(S)::in, int::in,
    maybe_error(idle_next)::out, io::di, io::uo) is det.

idle_loop(IMAP, Inotify, StartTime, Res, !IO) :-
    gettimeofday(Now, _, !IO),
    TimeoutSecs = StartTime - Now + idle_timeout_secs,
    get_filedes(Inotify, InotifyFd),
    get_read_filedes(IMAP, ResIMAP_Fd, !IO),
    (
        ResIMAP_Fd = ok(IMAP_Fd),
        select_read([IMAP_Fd, InotifyFd], TimeoutSecs, Res0, !IO),
        (
            Res0 = ready(_NumReady, FdSet),
            ( fd_isset(FdSet, InotifyFd) ->
                Sync1 = yes
            ;
                Sync1 = no
            ),
            ( fd_isset(FdSet, IMAP_Fd) ->
                do_read_idle_response(IMAP, Res2, Sync2, !IO)
            ;
                Res2 = ok,
                Sync2 = no
            ),
            (
                Res2 = ok,
                ( Sync1 `or` Sync2 = yes ->
                    Res = ok(sync)
                ;
                    idle_loop(IMAP, Inotify, StartTime, Res, !IO)
                )
            ;
                Res2 = error(Error),
                Res = error(Error)
            )
        ;
            Res0 = timeout,
            Res = ok(restart_idle)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        ResIMAP_Fd = error(Error),
        Res = error(Error)
    ).

:- pred do_read_idle_response(imap::in, maybe_error::out, bool::out,
    io::di, io::uo) is det.

do_read_idle_response(IMAP, Res, Sync, !IO) :-
    read_single_idle_response(IMAP, ResRead, !IO),
    (
        (
            ResRead = stop_idling(Alerts),
            Sync = yes
        ;
            ResRead = continue_idling(Alerts),
            Sync = no
        ),
        report_alerts(Alerts, !IO),
        Res = ok
    ;
        ResRead = error(Error),
        Res = error(Error),
        Sync = no
    ).

:- func idle_timeout_secs = int.

idle_timeout_secs = 29 * 60.

:- pred sleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    sleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    sleep(N);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
