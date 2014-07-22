%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module database.
:- import_module dir_cache.
:- import_module gettimeofday.
:- import_module imap.
:- import_module imap.types.
:- import_module inotify.
:- import_module log.
:- import_module log_help.
:- import_module maybe_result.
:- import_module openssl.
:- import_module path.
:- import_module prog_config.
:- import_module select.
:- import_module signal.
:- import_module sync.
:- import_module terminal_attr.

:- type idle_next
    --->    quit
    ;       sync(shortcut)
    ;       restart_idle.

:- inst idle_next_outer
    --->    quit
    ;       sync(ground).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = [ConfigFileName, PairingName] ->
        load_prog_config(ConfigFileName, PairingName, LoadRes, !IO),
        (
            LoadRes = ok(Config),
            main_1(Config, !IO)
        ;
            LoadRes = errors(Errors),
            print_error("Errors in configuration file:", !IO),
            list.foldl(print_error, Errors, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        io.progname_base("plugsink", ProgName, !IO),
        print_error(format("Usage: %s config-file pairing", [s(ProgName)]),
            !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_1(prog_config::in, io::di, io::uo) is det.

main_1(Config, !IO) :-
    MaybeLogFileName = Config ^ maybe_log_filename,
    Level = Config ^ log_level,
    open_log(MaybeLogFileName, Level, ResLog, !IO),
    (
        ResLog = ok(Log),
        main_2(Log, Config, !IO),
        close_log(Log, !IO)
    ;
        ResLog = error(Error),
        io.write_string(Error, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_2(log::in, prog_config::in, io::di, io::uo) is det.

main_2(Log, Config, !IO) :-
    ( debugging_grade ->
        true
    ;
        ignore_sigint(no, !IO)
    ),
    ignore_sigpipe(yes, !IO),
    DbFileName = Config ^ db_filename,
    log_info(Log, "Opening database " ++ DbFileName, !IO),
    open_database(DbFileName, ResOpenDb, !IO),
    (
        ResOpenDb = ok(Db),
        inotify.init(ResInotify, !IO),
        (
            ResInotify = ok(Inotify),
            maybe_prompt_password(Config, ResPassword, !IO),
            (
                ResPassword = ok(Password),
                main_3(Log, Config, Password, Db, Inotify, !IO)
            ;
                ResPassword = error(Error),
                report_error(Log, Error, !IO)
            ),
            inotify.close(Inotify, !IO)
        ;
            ResInotify = error(Error),
            report_error(Log, Error, !IO)
        ),
        close_database(Db, !IO)
    ;
        ResOpenDb = error(Error),
        report_error(Log, Error, !IO)
    ).

:- pred main_3(log::in, prog_config::in, password::in, database::in,
    inotify(S)::in, io::di, io::uo) is det.

main_3(Log, Config, Password, Db, Inotify, !IO) :-
    open_connection(Log, Config, ResBio, !IO),
    (
        ResBio = ok(Bio),
        imap.open(Bio, ResOpen, OpenAlerts, !IO),
        report_alerts(Log, OpenAlerts, !IO),
        (
            ResOpen = ok(IMAP),
            do_login(Log, Config, Password, IMAP, ResLogin, !IO),
            (
                ResLogin = yes,
                logged_in(Log, Config, Db, IMAP, Inotify, Res, !IO),
                (
                    Res = ok,
                    Restart = no
                ;
                    Res = eof,
                    report_error(Log, "unexpected eof", !IO),
                    Restart = yes
                ;
                    Res = error(Error),
                    report_error(Log, Error, !IO),
                    Restart = no
                )
            ;
                ResLogin = no,
                Restart = no
            ),
            % XXX skip logout if connection already closed
            log_notice(Log, "Logging out.\n", !IO),
            logout(IMAP, ResLogout, !IO),
            log_debug(Log, string(ResLogout), !IO),
            (
                Restart = yes,
                log_notice(Log, "Restarting.\n", !IO),
                main_3(Log, Config, Password, Db, Inotify, !IO)
            ;
                Restart = no
            )
        ;
            ResOpen = error(Error),
            report_error(Log, Error, !IO)
        )
        % Bio freed already.
    ;
        ResBio = error(Error),
        report_error(Log, Error, !IO)
    ).

:- pred debugging_grade is semidet.

debugging_grade :-
    string.sub_string_search($grade, ".debug", _).

:- pred print_error(string::in, io::di, io::uo) is det.

print_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_prompt_password(prog_config::in, maybe_error(password)::out,
    io::di, io::uo) is det.

maybe_prompt_password(Config, Res, !IO) :-
    MaybePassword = Config ^ password,
    (
        MaybePassword = yes(Password),
        Res = ok(Password)
    ;
        MaybePassword = no,
        UserName = Config ^ username,
        HostPort = Config ^ hostport,
        prompt_password(UserName, HostPort, Res, !IO)
    ).

:- pred prompt_password(username::in, string::in, maybe_error(password)::out,
    io::di, io::uo) is det.

prompt_password(username(UserName), HostPort, Res, !IO) :-
    string.format("Enter password for '%s' to '%s': ",
        [s(UserName), s(HostPort)], Prompt),
    io.write_string(Prompt, !IO),
    io.flush_output(!IO),
    set_echo(no, Res0, !IO),
    (
        Res0 = ok,
        io.read_line_as_string(ResRead, !IO),
        io.nl(!IO),
        set_echo(yes, _, !IO),
        (
            ResRead = ok(Line),
            Password = password(chomp(Line)),
            Res = ok(Password)
        ;
            ResRead = eof,
            Res = error("unexpected eof")
        ;
            ResRead = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred open_connection(log::in, prog_config::in, maybe_error(bio)::out,
    io::di, io::uo) is det.

open_connection(Log, Config, Res, !IO) :-
    HostPort = Config ^ hostport,
    MaybeCertificateFile = Config ^ certificate_file,

    log_info(Log, "Connecting to " ++ HostPort, !IO),
    openssl.library_init(!IO),
    connect_handshake(tlsv1_client_method, HostPort, MaybeCertificateFile,
        ResBio, !IO),
    (
        ResBio = ok(Bio - CertificateNames),
        log_debug(Log, "Verified peer certificate", !IO),

        Expected = Config ^ certificate_match_name,
        ( match_certificate_name(Expected, CertificateNames) ->
            Res = ok(Bio),
            log_info(Log, "Peer certificate matches name '" ++
                Expected ++ "'", !IO)
        ;
            bio_free_all(Bio, !IO),
            log_certificate_names(Log, CertificateNames, !IO),
            Res = error("Peer certificate does not match name '" ++
                Expected ++ "'")
        )
    ;
        ResBio = error(Error),
        Res = error(Error),
        % XXX ugly
        openssl.print_errors(io.stderr_stream, !IO)
    ).

:- pred connect_handshake(method::in, string::in, maybe(string)::in,
    maybe_error(pair(bio, certificate_names))::out, io::di, io::uo) is det.

connect_handshake(Method, HostPort, MaybeCertificateFile, Res, !IO) :-
    setup(Method, HostPort, MaybeCertificateFile, ResBio, !IO),
    (
        ResBio = ok(Bio),
        bio_do_connect(Bio, ResConnect, !IO),
        (
            ResConnect = ok,
            bio_do_handshake(Bio, ResHandshake, !IO),
            (
                ResHandshake = ok(CertificateNames),
                Res = ok(Bio - CertificateNames)
            ;
                ResHandshake = error(Error),
                Res = error(Error)
            )
        ;
            ResConnect = error(Error),
            Res = error(Error)
        ),
        (
            Res = ok(_)
        ;
            Res = error(_),
            bio_free_all(Bio, !IO)
        )
    ;
        ResBio = error(Error),
        Res = error(Error)
    ).

:- pred match_certificate_name(string::in, certificate_names::in) is semidet.

match_certificate_name(Expected, certificate_names(CommonName, DnsNames)) :-
    (
        member(Expected, DnsNames)
    ;
        CommonName = Expected
    ).

:- pred log_certificate_names(log::in, certificate_names::in, io::di, io::uo)
    is det.

log_certificate_names(Log, certificate_names(CommonName, DnsNames), !IO) :-
    log_notice(Log, "Peer certificate has common name (CN): " ++ CommonName,
        !IO),
    (
        DnsNames = []
    ;
        DnsNames = [_ | _],
        log_notice(Log, "Peer certificate has DNS names: "
            ++ join_list(", ", DnsNames), !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred do_login(log::in, prog_config::in, password::in, imap::in,
    bool::out, io::di, io::uo) is det.

do_login(Log, Config, Password, IMAP, Res, !IO) :-
    UserName = Config ^ username,
    imap.login(IMAP, UserName, Password, Res0, !IO),
    (
        Res0 = ok(result(ResLogin, LoginMessage, LoginAlerts)),
        report_alerts(Log, LoginAlerts, !IO),
        (
            ResLogin = ok,
            log_info(Log, LoginMessage, !IO),
            Res = yes
        ;
            ( ResLogin = no
            ; ResLogin = bad
            ; ResLogin = bye
            ; ResLogin = continue
            ),
            report_error(Log, LoginMessage, !IO),
            Res = no
        )
    ;
        Res0 = eof,
        report_error(Log, "unexpected eof", !IO),
        Res = no
    ;
        Res0 = error(Error),
        report_error(Log, Error, !IO),
        Res = no
    ).

%-----------------------------------------------------------------------------%


:- pred logged_in(log::in, prog_config::in, database::in, imap::in,
    inotify(S)::in, maybe_result::out, io::di, io::uo) is det.

logged_in(Log, Config, Db, IMAP, Inotify, Res, !IO) :-
    % For now.
    LocalMailboxName = Config ^ local_mailbox_name,
    RemoteMailboxName = Config ^ mailbox,
    select(IMAP, RemoteMailboxName, Res0, !IO),
    (
        Res0 = ok(result(Status, Text, Alerts)),
        report_alerts(Log, Alerts, !IO),
        (
            Status = ok,
            log_debug(Log, Text, !IO),

            % Assume that expunges may have occurred since the last run.
            set_expunge_seen_flag(IMAP, yes, !IO),

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
                    lookup_mailbox_pair(Db, LocalMailboxName,
                        RemoteMailboxName, UIDValidity, ResLookup, !IO),
                    (
                        ResLookup = ok(MailboxPair),
                        LocalMailboxPath = make_local_mailbox_path(Config,
                            LocalMailboxName),
                        LocalMailboxPath = local_mailbox_path(DirName),
                        DirCache0 = dir_cache.init(dirname(DirName)),
                        sync_and_repeat(Log, Config, Db, IMAP, Inotify,
                            MailboxPair, shortcut(check, check), scan_all,
                            Res, DirCache0, _DirCache, !IO)
                    ;
                        ResLookup = error(Error),
                        Res = error(Error)
                    )
                ;
                    ResInsert = error(Error),
                    Res = error(Error)
                )
            ;
                Res = error("Cannot support this server.")
            )
        ;
            ( Status = no
            ; Status = bad
            ; Status = bye
            ; Status = continue
            ),
            Res = error(Text)
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred sync_and_repeat(log::in, prog_config::in, database::in, imap::in,
    inotify(S)::in, mailbox_pair::in, shortcut::in, update_method::in,
    maybe_result::out, dir_cache::in, dir_cache::out, io::di, io::uo) is det.

sync_and_repeat(Log, Config, Db, IMAP, Inotify, MailboxPair, Shortcut0,
        DirCacheUpdate0, Res, !DirCache, !IO) :-
    lookup_remote_mailbox_modseqvalzer(Db, MailboxPair, ResLastModSeqValzer,
        !IO),
    (
        ResLastModSeqValzer = ok(LastModSeqValzer @ mod_seq_valzer(Low)),
        log_info(Log,
            format("Synchronising from MODSEQ %s", [s(to_string(Low))]), !IO),
        sync_mailboxes(Log, Config, Db, IMAP, Inotify, MailboxPair,
            LastModSeqValzer, Shortcut0, DirCacheUpdate0, ResSync,
            !DirCache, !IO),
        (
            ResSync = ok,
            get_sigint_count(SigInt, !IO),
            ( SigInt > 0 ->
                Res = ok
            ; Config ^ idle = no ->
                Res = ok
            ;
                sync_and_repeat_2(Log, Config, IMAP, Inotify, Res1,
                    DirCacheUpdate1, !DirCache, !IO),
                (
                    Res1 = ok(sync(Shortcut1)),
                    sync_and_repeat(Log, Config, Db, IMAP, Inotify,
                        MailboxPair, Shortcut1, DirCacheUpdate1,
                        Res, !DirCache, !IO)
                ;
                    Res1 = ok(quit),
                    Res = ok
                ;
                    ( Res1 = eof
                    ; Res1 = error(_)
                    ),
                    Res = convert(Res1)
                )
            )
        ;
            ( ResSync = eof
            ; ResSync = error(_)
            ),
            Res = ResSync
        )
    ;
        ResLastModSeqValzer = error(Error),
        Res = error(Error)
    ).

:- pred sync_and_repeat_2(log::in, prog_config::in, imap::in, inotify(S)::in,
    maybe_result(idle_next)::out(maybe_result(idle_next_outer)),
    dir_cache.update_method::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

sync_and_repeat_2(Log, Config, IMAP, Inotify, Res, DirCacheUpdate1, !DirCache,
        !IO) :-
    % Clear inotify events prior to idling.  There may be events in the queue,
    % particularly those induced by renamings performed during the sync cycle,
    % which would cause us to wake immediately.
    AddNewWatches = Config ^ idle,
    update_dir_cache(Log, Inotify, scan_from_inotify_events(no), AddNewWatches,
        ResCache, !DirCache, !IO),
    (
        ResCache = ok(Changed),
        (
            Changed = yes,
            CheckLocal = check,
            DirCacheUpdate1 = scan_from_inotify_events(yes)
        ;
            Changed = no,
            CheckLocal = skip,
            DirCacheUpdate1 = scan_from_inotify_events(no)
        ),
        % NOOP lets the server update us before we IDLE.
        noop(Log, IMAP, ResNoop, !IO),
        (
            ResNoop = ok,
            should_check_remote(IMAP, CheckRemote, !IO),
            (
                CheckLocal = skip,
                CheckRemote = skip
            ->
                idle_until_sync(Log, Config, IMAP, Inotify, Res, !IO)
            ;
                Res = ok(sync(shortcut(CheckLocal, CheckRemote)))
            )
        ;
            ResNoop = eof,
            Res = eof
        ;
            ResNoop = error(Error),
            Res = error(Error)
        )
    ;
        ResCache = error(Error),
        Res = error(Error),
        DirCacheUpdate1 = scan_from_inotify_events(no)
    ).

:- pred noop(log::in, imap::in, maybe_result::out, io::di, io::uo) is det.

noop(Log, IMAP, Res, !IO) :-
    noop(IMAP, Res0, !IO),
    (
        Res0 = ok(result(Status, Text, Alerts)),
        report_alerts(Log, Alerts, !IO),
        (
            Status = ok,
            log_debug(Log, Text, !IO),
            Res = ok
        ;
            ( Status = no
            ; Status = bad
            ; Status = bye
            ; Status = continue
            ),
            Res = error("unexpected response to NOOP: " ++ Text)
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred should_check_remote(imap::in, check::out, io::di, io::uo) is det.

should_check_remote(IMAP, CheckRemote, !IO) :-
    % If either EXISTS or EXPUNGE responses were seen from the server then
    % immediately sync again instead of entering IDLE.  The EXPUNGE seen flag
    % must be left intact for expunge detection to be run.
    clear_exists_seen_flag(IMAP, ExistsSeen, !IO),
    read_expunge_seen_flag(IMAP, ExpungeSeen, !IO),
    (
        ExistsSeen = no,
        ExpungeSeen = no
    ->
        CheckRemote = skip
    ;
        CheckRemote = check
    ).

:- pred idle_until_sync(log::in, prog_config::in, imap::in, inotify(S)::in,
    maybe_result(idle_next)::out(maybe_result(idle_next_outer)),
    io::di, io::uo) is det.

idle_until_sync(Log, Config, IMAP, Inotify, Res, !IO) :-
    % Send IDLE command.
    log_info(Log, "Idling", !IO),
    gettimeofday(StartTime, _StartUsec, !IO),
    idle(IMAP, Res0, !IO),
    (
        Res0 = ok(result(Status, IdleText, IdleAlerts)),
        report_alerts(Log, IdleAlerts, !IO),
        (
            Status = continue,
            log_debug(Log, IdleText, !IO),
            idle_until_done(Log, Config, IMAP, Inotify, StartTime, Res1, !IO),
            (
                Res1 = ok(sync(Shortcut)),
                sleep(1, !IO),
                Res = ok(sync(Shortcut))
            ;
                Res1 = ok(restart_idle),
                idle_until_sync(Log, Config, IMAP, Inotify, Res, !IO)
            ;
                Res1 = ok(quit),
                Res = ok(quit)
            ;
                Res1 = eof,
                Res = eof
            ;
                Res1 = error(Error),
                Res = error(Error)
            )
        ;
            Status = no,
            Res = error("IDLE not supported: " ++ IdleText)
        ;
            ( Status = ok
            ; Status = bad
            ; Status = bye
            ),
            Res = error("unexpected response to IDLE: " ++ IdleText)
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred idle_until_done(log::in, prog_config::in, imap::in, inotify(S)::in,
    int::in, maybe_result(idle_next)::out, io::di, io::uo) is det.

idle_until_done(Log, Config, IMAP, Inotify, StartTime, Res, !IO) :-
    idle_loop(Log, Config, IMAP, Inotify, StartTime, Res0, !IO),
    (
        Res0 = ok(_),
        % Send IDLE DONE.
        idle_done(IMAP, Res1, !IO),
        (
            Res1 = ok(result(Status, DoneText, DoneAlerts)),
            report_alerts(Log, DoneAlerts, !IO),
            (
                Status = ok,
                Res = Res0
           ;
                ( Status = no
                ; Status = bad
                ; Status = bye
                ; Status = continue
                ),
                Res = error("unexpected response to IDLE DONE: " ++ DoneText)
            )
        ;
            Res1 = eof,
            Res = eof
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred idle_loop(log::in, prog_config::in, imap::in, inotify(S)::in, int::in,
    maybe_result(idle_next)::out, io::di, io::uo) is det.

idle_loop(Log, Config, IMAP, Inotify, StartTime, Res, !IO) :-
    gettimeofday(Now, _, !IO),
    IdleTimeoutSecs = Config ^ idle_timeout_secs,
    TimeoutSecs = StartTime - Now + IdleTimeoutSecs,
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
                    idle_loop(Log, Config, IMAP, Inotify, StartTime, Res, !IO)
                ;
                    Res = ok(sync(shortcut(CheckLocal, CheckRemote)))
                )
            ;
                Res2 = eof,
                Res = eof
            ;
                Res2 = error(Error),
                Res = error(Error)
            )
        ;
            Res0 = timeout,
            SyncOnIdleTimeout = Config ^ sync_on_idle_timeout,
            (
                SyncOnIdleTimeout = yes,
                Res = ok(sync(shortcut(skip, check)))
            ;
                SyncOnIdleTimeout = no,
                Res = ok(restart_idle)
            )
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

:- pred do_read_idle_response(log::in, imap::in, maybe_result::out, check::out,
    io::di, io::uo) is det.

do_read_idle_response(Log, IMAP, Res, CheckRemote, !IO) :-
    read_single_idle_response(IMAP, ResRead, !IO),
    (
        ResRead = ok(Stop - Alerts),
        (
            Stop = stop_idling,
            CheckRemote = check
        ;
            Stop = continue_idling,
            CheckRemote = skip
        ),
        report_alerts(Log, Alerts, !IO),
        Res = ok
    ;
        ResRead = eof,
        Res = eof,
        CheckRemote = skip
    ;
        ResRead = error(Error),
        Res = error(Error),
        CheckRemote = skip
    ).

:- pred sleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    sleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    sleep(N);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
