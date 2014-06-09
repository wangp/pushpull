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
:- import_module log_help.
:- import_module path.
:- import_module prog_config.
:- import_module select.
:- import_module signal.
:- import_module sync.

:- type idle_next_outer
    --->    quit
    ;       sync(shortcut).

:- type idle_next_inner
    --->    quit
    ;       sync(shortcut)
    ;       restart_idle.

%-----------------------------------------------------------------------------%

main(!IO) :-
    open_log(no, info, ResLog, !IO),
    (
        ResLog = ok(Log),
        main_1(Log, !IO),
        close_log(Log, !IO)
    ;
        ResLog = error(Error),
        io.stderr_stream(Stream, !IO),
        io.write_string(Stream, Error, !IO),
        io.nl(Stream, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_1(log::in, io::di, io::uo) is det.

main_1(Log, !IO) :-
    ( debugging_grade ->
        true
    ;
        ignore_sigint(no, !IO)
    ),
    ignore_sigpipe(yes, !IO),
    io.command_line_arguments(Args, !IO),
    (
        Args = [DbFileName, MaildirRoot, LocalMailboxName, HostPort, UserName,
            Password, RemoteMailbox]
    ->
        Config = prog_config(DbFileName, maildir_root(MaildirRoot),
            local_mailbox_name(LocalMailboxName), HostPort, username(UserName),
            password(Password), mailbox(RemoteMailbox)),
        log_info(Log, "Opening database " ++ DbFileName, !IO),
        open_database(DbFileName, ResOpenDb, !IO),
        (
            ResOpenDb = ok(Db),
            main_2(Log, Config, Db, !IO),
            close_database(Db, !IO)
        ;
            ResOpenDb = error(Error),
            report_error(Log, Error, !IO)
        )
    ;
        report_error(Log, "unexpected arguments", !IO)
    ).

:- pred debugging_grade is semidet.

debugging_grade :-
    string.sub_string_search($grade, ".debug", _).

%-----------------------------------------------------------------------------%

:- pred main_2(log::in, prog_config::in, database::in, io::di, io::uo) is det.

main_2(Log, Config, Db, !IO) :-
    HostPort = Config ^ hostport,
    UserName = Config ^ username,
    Password = Config ^ password,
    imap.open(HostPort, ResOpen, OpenAlerts, !IO),
    report_alerts(Log, OpenAlerts, !IO),
    (
        ResOpen = ok(IMAP),
        login(IMAP, UserName, Password,
            result(ResLogin, LoginMessage, LoginAlerts), !IO),
        report_alerts(Log, LoginAlerts, !IO),
        (
            ResLogin = ok,
            log_info(Log, LoginMessage, !IO),
            logged_in(Log, Config, Db, IMAP, !IO)
        ;
            ( ResLogin = no
            ; ResLogin = bad
            ; ResLogin = bye
            ; ResLogin = continue
            ; ResLogin = error
            ),
            report_error(Log, LoginMessage, !IO)
        ),
        log_notice(Log, "Logging out.\n", !IO),
        logout(IMAP, ResLogout, !IO),
        log_debug(Log, string(ResLogout), !IO)
    ;
        ResOpen = error(Error),
        report_error(Log, Error, !IO)
    ).

:- pred logged_in(log::in, prog_config::in, database::in, imap::in,
    io::di, io::uo) is det.

logged_in(Log, Config, Db, IMAP, !IO) :-
    % For now.
    LocalMailboxName = Config ^ local_mailbox_name,
    RemoteMailboxName = Config ^ mailbox,
    select(IMAP, RemoteMailboxName, result(ResExamine, Text, Alerts), !IO),
    report_alerts(Log, Alerts, !IO),
    (
        ResExamine = ok,
        log_debug(Log, Text, !IO),

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

                        sync_and_repeat(Log, Config, Db, IMAP, Inotify,
                            MailboxPair, LastModSeqValzer,
                            shortcut(check, check), scan_all,
                            DirCache0, _DirCache, !IO)
                    ;
                        ResInotify = error(Error),
                        report_error(Log, Error, !IO)
                    )
                ;
                    ResLookup = error(Error),
                    report_error(Log, Error, !IO)
                )
            ;
                ResInsert = error(Error),
                report_error(Log, Error, !IO)
            )
        ;
            report_error(Log, "Cannot support this server.", !IO)
        )
    ;
        ( ResExamine = no
        ; ResExamine = bad
        ; ResExamine = bye
        ; ResExamine = continue
        ; ResExamine = error
        ),
        report_error(Log, Text, !IO)
    ).

:- pred sync_and_repeat(log::in, prog_config::in, database::in, imap::in,
    inotify(S)::in, mailbox_pair::in, mod_seq_valzer::in,
    shortcut::in, update_method::in, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

sync_and_repeat(Log, Config, Db, IMAP, Inotify, MailboxPair, LastModSeqValzer,
        Shortcut0, DirCacheUpdate0, !DirCache, !IO) :-
    % This is the highest MODSEQ value that we know of, which could be higher
    % by now.
    get_selected_mailbox_highest_modseqvalue(IMAP, MaybeHighestModSeqValue,
        !IO),
    ( MaybeHighestModSeqValue = yes(highestmodseq(HighestModSeqValue)) ->
        LastModSeqValzer = mod_seq_valzer(Low),
        HighestModSeqValue = mod_seq_value(High),
        log_info(Log,
            format("Synchronising from MODSEQ %s (highest MODSEQ >= %s)\n",
                [s(to_string(Low)), s(to_string(High))]), !IO),

        sync_mailboxes(Log, Config, Db, IMAP, Inotify, MailboxPair,
            LastModSeqValzer, HighestModSeqValue, Shortcut0, DirCacheUpdate0,
            ResSync, !DirCache, !IO),
        (
            ResSync = ok,
            get_sigint_count(SigInt, !IO),
            ( SigInt > 0 ->
                true
            ;
                % Clear inotify events prior to idling.  There may be events in
                % the queue, particularly those induced by renamings performed
                % during the sync cycle, which would cause us to wake
                % immediately.
                update_dir_cache(Log, Inotify, scan_from_inotify_events(no),
                    ResCache, !DirCache, !IO),
                (
                    ResCache = ok(yes),
                    ResIdle = ok(sync(shortcut(check, skip))),
                    DirCacheUpdate1 = scan_from_inotify_events(yes)
                ;
                    ResCache = ok(no),
                    idle_until_sync(Log, IMAP, Inotify, ResIdle, !IO),
                    DirCacheUpdate1 = scan_from_inotify_events(no)
                ;
                    ResCache = error(Error0),
                    ResIdle = error(Error0),
                    DirCacheUpdate1 = scan_from_inotify_events(no)
                ),
                (
                    ResIdle = ok(sync(Shortcut1)),
                    update_selected_mailbox_highest_modseqvalue_from_fetches(
                        IMAP, !IO),
                    sync_and_repeat(Log, Config, Db, IMAP, Inotify,
                        MailboxPair, mod_seq_valzer(High), Shortcut1,
                        DirCacheUpdate1, !DirCache, !IO)
                ;
                    ResIdle = ok(quit)
                ;
                    ResIdle = error(Error),
                    report_error(Log, Error, !IO)
                )
            )
        ;
            ResSync = error(Error),
            report_error(Log, Error, !IO)
        )
    ;
        report_error(Log, "Cannot support this server.", !IO)
    ).

:- pred idle_until_sync(log::in, imap::in, inotify(S)::in,
    maybe_error(idle_next_outer)::out, io::di, io::uo) is det.

idle_until_sync(Log, IMAP, Inotify, Res, !IO) :-
    % Send IDLE command.
    log_info(Log, "Idling", !IO),
    gettimeofday(StartTime, _StartUsec, !IO),
    idle(IMAP, result(Res0, IdleText, IdleAlerts), !IO),
    report_alerts(Log, IdleAlerts, !IO),
    (
        Res0 = continue,
        log_debug(Log, IdleText, !IO),
        idle_until_done(Log, IMAP, Inotify, StartTime, Res1, !IO),
        (
            Res1 = ok(sync(Shortcut)),
            sleep(1, !IO),
            Res = ok(sync(Shortcut))
        ;
            Res1 = ok(restart_idle),
            idle_until_sync(Log, IMAP, Inotify, Res, !IO)
        ;
            Res1 = ok(quit),
            Res = ok(quit)
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

:- pred idle_until_done(log::in, imap::in, inotify(S)::in, int::in,
    maybe_error(idle_next_inner)::out, io::di, io::uo) is det.

idle_until_done(Log, IMAP, Inotify, StartTime, Res, !IO) :-
    idle_loop(Log, IMAP, Inotify, StartTime, Res0, !IO),
    (
        Res0 = ok(Next),
        % Send IDLE DONE.
        idle_done(IMAP, result(ResDone, DoneText, DoneAlerts), !IO),
        report_alerts(Log, DoneAlerts, !IO),
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

:- pred idle_loop(log::in, imap::in, inotify(S)::in, int::in,
    maybe_error(idle_next_inner)::out, io::di, io::uo) is det.

idle_loop(Log, IMAP, Inotify, StartTime, Res, !IO) :-
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
                CheckLocal = check
            ;
                CheckLocal = skip
            ),
            ( fd_isset(FdSet, IMAP_Fd) ->
                do_read_idle_response(Log, IMAP, Res2, CheckRemote, !IO)
            ;
                Res2 = ok,
                CheckRemote = skip
            ),
            (
                Res2 = ok,
                (
                    CheckLocal = skip,
                    CheckRemote = skip
                ->
                    idle_loop(Log, IMAP, Inotify, StartTime, Res, !IO)
                ;
                    Res = ok(sync(shortcut(CheckLocal, CheckRemote)))
                )
            ;
                Res2 = error(Error),
                Res = error(Error)
            )
        ;
            Res0 = timeout,
            Res = ok(restart_idle)
        ;
            Res0 = interrupt,
            log_notice(Log, "Interrupted.\n", !IO),
            Res = ok(quit)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        ResIMAP_Fd = error(Error),
        Res = error(Error)
    ).

:- pred do_read_idle_response(log::in, imap::in, maybe_error::out, check::out,
    io::di, io::uo) is det.

do_read_idle_response(Log, IMAP, Res, CheckRemote, !IO) :-
    read_single_idle_response(IMAP, ResRead, !IO),
    (
        (
            ResRead = stop_idling(Alerts),
            CheckRemote = check
        ;
            ResRead = continue_idling(Alerts),
            CheckRemote = skip
        ),
        report_alerts(Log, Alerts, !IO),
        Res = ok
    ;
        ResRead = error(Error),
        Res = error(Error),
        CheckRemote = skip
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
