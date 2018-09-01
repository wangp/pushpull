% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module main.
:- interface.

:- import_module io.

:- pred real_main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module benchmarking.
:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module database.
:- import_module dir_cache.
:- import_module file_util.
:- import_module gettimeofday.
:- import_module imap.
:- import_module imap.types.
:- import_module inotify.
:- import_module log.
:- import_module log_help.
:- import_module maildir.
:- import_module maybe_result.
:- import_module openssl.
:- import_module path.
:- import_module prog_config.
:- import_module select.
:- import_module setsockopt.
:- import_module signal.
:- import_module sync.
:- import_module terminal_attr.

:- type result_restart == pair(maybe_result, restart).

:- type restart
    --->    stop
    ;       immediate_restart
    ;       delayed_restart.

:- type idle_next
    --->    quit
    ;       sync(requires_check)
    ;       restart_idle.

:- type idle_next_outer
    --->    quit
    ;       sync(requires_check).

%-----------------------------------------------------------------------------%

real_main(!IO) :-
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
        install_signal_handler(sigint, count, !IO)
    ),
    install_signal_handler(sigterm, count, !IO),
    install_signal_handler(sigpipe, ignore, !IO),
    install_signal_handler(sigusr1, count, !IO),
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
                % Initialise OpenSSL once only.
                openssl.library_init(!IO),
                get_environment_info(EnvInfo, !IO),
                main_3(Log, Config, Password, EnvInfo, Db, Inotify, !IO)
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

:- pred main_3(log::in, prog_config::in, password::in, env_info::in,
    database::in, inotify(S)::in, io::di, io::uo) is det.

main_3(Log, Config, Password, EnvInfo, Db, Inotify, !IO) :-
    LocalMailboxName = Config ^ local_mailbox_name,
    LocalMailboxPath = make_local_mailbox_path(Config, LocalMailboxName),
    LocalMailboxPath = local_mailbox_path(DirName),
    DirCache0 = dir_cache.init(dirname(DirName)),
    main_4(Log, Config, Password, EnvInfo, Db, Inotify, DirCache0, _, !IO).

:- pred main_4(log::in, prog_config::in, password::in, env_info::in,
    database::in, inotify(S)::in, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

main_4(Log, Config, Password, EnvInfo, Db, Inotify, !DirCache, !IO) :-
    main_5(Log, Config, Password, EnvInfo, Db, Inotify, Restart,
        !DirCache, !IO),
    get_sigint_or_sigterm_count(InterruptCount0, !IO),
    ( InterruptCount0 > 0 ->
        log_notice(Log, "Stopping.\n", !IO)
    ;
        (
            Restart = stop
        ;
            Restart = immediate_restart,
            log_notice(Log, "Restarting.\n", !IO),
            main_4(Log, Config, Password, EnvInfo, Db, Inotify, !DirCache, !IO)
        ;
            Restart = delayed_restart,
            MaybeDelay = Config ^ restart_after_error_seconds,
            (
                MaybeDelay = yes(Delay),
                log_notice(Log,
                    format("Restarting after %d second delay.\n", [i(Delay)]),
                    !IO),
                sleep(Delay, !IO),
                get_sigint_or_sigterm_count(InterruptCount1, !IO),
                ( InterruptCount1 > 0 ->
                    log_notice(Log, "Stopping.\n", !IO)
                ;
                    log_notice(Log, "Restarting.\n", !IO),
                    main_4(Log, Config, Password, EnvInfo, Db, Inotify,
                        !DirCache, !IO)
                )
            ;
                MaybeDelay = no
            )
        )
    ).

:- pred main_5(log::in, prog_config::in, password::in, env_info::in,
    database::in, inotify(S)::in, restart::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

main_5(Log, Config, Password, EnvInfo, Db, Inotify, Restart, !DirCache, !IO) :-
    open_connection(Log, Config, ResBio, !IO),
    (
        ResBio = ok(Bio),
        imap.open(Bio, ResOpen, OpenAlerts, !IO),
        report_alerts(Log, OpenAlerts, !IO),
        (
            ResOpen = ok(IMAP),
            do_login(Log, Config, Password, IMAP, ResLogin, !IO),
            % ResLogin already logged.
            (
                ResLogin = ok,
                logged_in(Log, Config, EnvInfo, Db, IMAP, Inotify,
                    ResPostLogin - Restart, !DirCache, !IO),
                (
                    ResPostLogin = ok
                ;
                    ResPostLogin = eof,
                    log_notice(Log, "Connection was closed prematurely.", !IO)
                ;
                    ResPostLogin = error(Error),
                    report_error(Log, Error, !IO)
                )
            ;
                ResLogin = eof,
                ResPostLogin = eof,
                Restart = delayed_restart
            ;
                ResLogin = error(LoginError),
                ResPostLogin = error(LoginError),
                % XXX separate permanent and transient login failures
                Restart = delayed_restart
            ),
            (
                ( ResPostLogin = ok
                ; ResPostLogin = error(_)
                ),
                logout(IMAP, ResLogout, !IO),
                (
                    ResLogout = ok(result(Status, LogoutText, LogoutAlerts)),
                    report_alerts(Log, LogoutAlerts, !IO),
                    (
                        Status = ok,
                        log_debug(Log, LogoutText, !IO),
                        log_notice(Log, "Logged out.", !IO)
                    ;
                        ( Status = no
                        ; Status = bad
                        ; Status = bye
                        ; Status = continue
                        ),
                        log_error(Log, LogoutText, !IO)
                    )
                ;
                    ResLogout = eof,
                    log_warning(Log, "Connection closed before logout.", !IO)
                ;
                    ResLogout = error(LogoutError),
                    report_error(Log, LogoutError, !IO)
                )
            ;
                ResPostLogin = eof
            ),
            close(IMAP, !IO)
        ;
            ResOpen = error(Error),
            report_error(Log, Error, !IO),
            Restart = delayed_restart
        )
        % Bio freed already.
    ;
        ResBio = error(Error),
        report_error(Log, Error, !IO),
        Restart = delayed_restart
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
            Res = error("unexpected eof in " ++ $pred)
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

    log_notice(Log, "Connecting to " ++ HostPort, !IO),
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
            bio_destroy(Bio, !IO),
            log_certificate_names(Log, CertificateNames, !IO),
            Res = error("Peer certificate does not match name '" ++
                Expected ++ "'")
        )
    ;
        ResBio = error(Error),
        Res = error(Error)
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
                set_timeouts(Bio, ResSetup, !IO),
                (
                    ResSetup = ok,
                    Res = ok(Bio - CertificateNames)
                ;
                    ResSetup = error(Error),
                    Res = error(Error)
                )
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
            bio_destroy(Bio, !IO)
        )
    ;
        ResBio = error(Error),
        Res = error(Error)
    ).

:- pred set_timeouts(bio::in, maybe_error::out, io::di, io::uo) is det.

set_timeouts(Bio, Res, !IO) :-
    bio_get_fd(Bio, ResFd, !IO),
    (
        ResFd = ok(Fd),
        % Some server operations really do take a long time.
        RecvTimeoutSecs = 300,
        SendTimeoutSecs = 60,
        set_timeout(Fd, recv_timeout, RecvTimeoutSecs, Res0, !IO),
        (
            Res0 = ok,
            set_timeout(Fd, send_timeout, SendTimeoutSecs, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        ResFd = error(Error),
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
    maybe_result::out, io::di, io::uo) is det.

do_login(Log, Config, Password, IMAP, Res, !IO) :-
    UserName = Config ^ username,
    imap.login(IMAP, UserName, Password, Res0, !IO),
    (
        Res0 = ok(result(ResLogin, LoginMessage, LoginAlerts)),
        report_alerts(Log, LoginAlerts, !IO),
        (
            ResLogin = ok,
            log_info(Log, LoginMessage, !IO),
            Res = ok
        ;
            ( ResLogin = no
            ; ResLogin = bad
            ; ResLogin = bye
            ; ResLogin = continue
            ),
            log_error(Log, LoginMessage, !IO),
            Res = error(LoginMessage)
        )
    ;
        Res0 = eof,
        report_error(Log, "unexpected eof in" ++ $pred, !IO),
        Res = eof
    ;
        Res0 = error(Error),
        report_error(Log, Error, !IO),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred logged_in(log::in, prog_config::in, env_info::in, database::in,
    imap::in, inotify(S)::in, result_restart::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

logged_in(Log, Config, EnvInfo, Db, IMAP, Inotify, Res, !DirCache, !IO) :-
    RemoteMailboxName = Config ^ mailbox,
    % [RFC 7162] HIGHESTMODSEQ is only required once a CONDSTORE enabling
    % command is issued.
    select(IMAP, RemoteMailboxName, [condstore], Res0, !IO),
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
                logged_in_selected(Log, Config, EnvInfo, Db, IMAP, Inotify,
                    UIDValidity, Res, !DirCache, !IO)
            ;
                Res = error("Cannot support this server.") - stop
            )
        ;
            ( Status = no
            ; Status = bad
            ; Status = bye
            ; Status = continue
            ),
            Res = error(Text) - stop % perhaps restartable
        )
    ;
        Res0 = eof,
        Res = eof - delayed_restart
    ;
        Res0 = error(Error),
        Res = error(Error) - delayed_restart
    ).

:- pred logged_in_selected(log::in, prog_config::in, env_info::in,
    database::in, imap::in, inotify(S)::in, uidvalidity::in,
    result_restart::out, dir_cache::in, dir_cache::out, io::di, io::uo) is det.

logged_in_selected(Log, Config, EnvInfo, Db, IMAP, Inotify, UIDValidity, Res,
        !DirCache, !IO) :-
    LocalMailboxName = Config ^ local_mailbox_name,
    LocalMailboxPath = make_local_mailbox_path(Config, LocalMailboxName),
    LocalMailboxPath = local_mailbox_path(DirName),
    RemoteMailboxName = Config ^ mailbox,
    RemoteMailboxString = Config ^ mailbox_string,
    % XXX maybe avoid interactive prompt after idling
    check_sane_pairing(Db, LocalMailboxName, LocalMailboxPath,
        RemoteMailboxName - RemoteMailboxString, UIDValidity, ResSane, !IO),
    (
        ResSane = ok,
        insert_or_ignore_mailbox_pair(Db, LocalMailboxName, RemoteMailboxName,
            UIDValidity, ResInsert, !IO),
        (
            ResInsert = ok,
            lookup_mailbox_pair(Db, LocalMailboxName, RemoteMailboxName,
                UIDValidity, ResLookup, !IO),
            (
                ResLookup = ok(MailboxPair),
                log_notice(Log,
                    format("Will synchronise '%s' with IMAP folder '%s'",
                        [s(DirName), s(RemoteMailboxString)]), !IO),
                sync_and_repeat(Log, Config, EnvInfo, Db, IMAP, Inotify,
                    MailboxPair, requires_check(check, check), scan_all, Res,
                    !DirCache, !IO)
            ;
                ResLookup = not_found,
                Res = error("mailbox_pair not found") - stop
            ;
                ResLookup = error(Error),
                Res = error(Error) - stop
            )
        ;
            ResInsert = error(Error),
            Res = error(Error) - stop
        )
    ;
        ResSane = error(Error),
        Res = error(Error) - stop
    ).

%-----------------------------------------------------------------------------%

:- pred check_sane_pairing(database::in, local_mailbox_name::in,
    local_mailbox_path::in, pair(mailbox, string)::in, uidvalidity::in,
    maybe_error::out, io::di, io::uo) is det.

check_sane_pairing(Db, LocalMailboxName, LocalMailboxPath,
        RemoteMailboxName - RemoteMailboxString, UIDValidity, Res, !IO) :-
    lookup_mailbox_pair(Db, LocalMailboxName, RemoteMailboxName, UIDValidity,
        ResLookup, !IO),
    (
        (
            ResLookup = ok(_),
            KnownInDb = yes
        ;
            ResLookup = not_found,
            KnownInDb = no
        ),
        check_sane_pairing_2(LocalMailboxPath, RemoteMailboxString,
            UIDValidity, KnownInDb, Res, !IO)
    ;
        ResLookup = error(Error),
        Res = error(Error)
    ).

:- pred check_sane_pairing_2(local_mailbox_path::in, string::in,
    uidvalidity::in, bool::in, maybe_error::out, io::di, io::uo) is det.

check_sane_pairing_2(LocalMailboxPath, RemoteMailboxString, UIDValidity,
        KnownInDb, Res, !IO) :-
    LocalMailboxPath = local_mailbox_path(DirName),
    file_exists(DirName, ResExists, !IO),
    (
        ResExists = yes(IsDir),
        (
            IsDir = yes,
            DirExists = yes,
            check_sane_pairing_3(LocalMailboxPath, DirExists,
                RemoteMailboxString, UIDValidity, KnownInDb, Res, !IO)
        ;
            IsDir = no,
            Res = error(DirName ++ " is not a directory")
        )
    ;
        ResExists = no,
        DirExists = no,
        check_sane_pairing_3(LocalMailboxPath, DirExists,
            RemoteMailboxString, UIDValidity, KnownInDb, Res, !IO)
    ;
        ResExists = error(Error),
        Res = error(Error)
    ).

:- pred check_sane_pairing_3(local_mailbox_path::in, bool::in, string::in,
    uidvalidity::in, bool::in, maybe_error::out, io::di, io::uo) is det.

check_sane_pairing_3(LocalMailboxPath, DirExists, RemoteMailboxString,
        UIDValidity, KnownInDb, Res, !IO) :-
    (
        KnownInDb = yes,
        DirExists = yes,
        Res = ok
    ;
        KnownInDb = yes,
        DirExists = no,
        % Every message in the remote mailbox would be marked as deleted if we
        % continued.
        LocalMailboxPath = local_mailbox_path(DirName),
        Res = error(string.format(
            "The directory '%s' does not exist but the pairing " ++
            "is already known to the database.",
            [s(DirName)]))
    ;
        KnownInDb = no,
        DirExists = yes,
        prompt_first_synchronisation(LocalMailboxPath, RemoteMailboxString,
            UIDValidity, ResAnswer, !IO),
        (
            ResAnswer = ok(yes),
            Res = ok
        ;
            ResAnswer = ok(no),
            Res = error("stopped")
        ;
            ResAnswer = error(Error),
            Res = error(Error)
        )
    ;
        KnownInDb = no,
        DirExists = no,
        LocalMailboxPath = local_mailbox_path(DirName),
        prompt_make_directory(DirName, ResAnswer, !IO),
        (
            ResAnswer = ok(yes),
            dir.make_directory(DirName, ResMakeDir, !IO),
            (
                ResMakeDir = ok,
                Res = ok
            ;
                ResMakeDir = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            ResAnswer = ok(no),
            Res = error("stopped")
        ;
            ResAnswer = error(Error),
            Res = error(Error)
        )
    ).

:- pred prompt_first_synchronisation(local_mailbox_path::in, string::in,
    uidvalidity::in, maybe_error(bool)::out, io::di, io::uo) is det.

prompt_first_synchronisation(LocalMailboxPath, RemoteMailboxString,
        UIDValidity, Res, !IO) :-
    LocalMailboxPath = local_mailbox_path(DirName),
    UIDValidity = uidvalidity(UIDValidity0),
    io.format(
        "This appears to be the first time synchronising '%s'\n" ++
        "with IMAP folder '%s' (UIDVALIDITY %s).\n",
        [s(DirName), s(RemoteMailboxString), s(to_string(UIDValidity0))], !IO),
    prompt_bool("Continue? [y,n] ", Res, !IO).

:- pred prompt_make_directory(string::in, maybe_error(bool)::out,
    io::di, io::uo) is det.

prompt_make_directory(DirName, Res, !IO) :-
    prompt_bool(
        string.format("Create the directory '%s'? [y,n] ", [s(DirName)]),
        Res, !IO).

:- pred prompt_bool(string::in, maybe_error(bool)::out, io::di, io::uo) is det.

prompt_bool(Prompt, Res, !IO) :-
    io.write_string(Prompt, !IO),
    io.flush_output(!IO),
    io.read_line_as_string(ResRead, !IO),
    (
        ResRead = ok(String),
        ( yes_no(to_lower(strip(String)), Bool) ->
            Res = ok(Bool)
        ;
            prompt_bool(Prompt, Res, !IO)
        )
    ;
        ResRead = eof,
        Res = error("eof received")
    ;
        ResRead = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred yes_no(string::in, bool::out) is semidet.

yes_no("y", yes).
yes_no("yes", yes).
yes_no("n", no).
yes_no("no", no).

%-----------------------------------------------------------------------------%

:- pred sync_and_repeat(log::in, prog_config::in, env_info::in, database::in,
    imap::in, inotify(S)::in, mailbox_pair::in, requires_check::in,
    update_method::in, result_restart::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

sync_and_repeat(Log, Config, EnvInfo, Db, IMAP, Inotify, MailboxPair,
        Shortcut0, DirCacheUpdate0, Res, !DirCache, !IO) :-
    lookup_remote_mailbox_modseqvalzer(Db, MailboxPair, ResLastModSeqValzer,
        !IO),
    (
        ResLastModSeqValzer = ok(LastModSeqValzer @ mod_seq_valzer(Low)),
        log_notice(Log,
            format("Synchronising from MODSEQ %s", [s(to_string(Low))]), !IO),
        sync_mailboxes(Log, Config, EnvInfo, Db, IMAP, Inotify, MailboxPair,
            LastModSeqValzer, Shortcut0, DirCacheUpdate0, ResSync,
            !DirCache, !IO),
        (
            ResSync = ok(ok),
            get_sigint_or_sigterm_count(InterruptCount, !IO),
            ( InterruptCount > 0 ->
                Res = ok - stop
            ; Config ^ idle = no ->
                Res = ok - stop
            ;
                % Go into IDLE.
                sync_and_repeat_2(Log, Config, IMAP, Inotify, Res1,
                    DirCacheUpdate1, !DirCache, !IO),
                (
                    Res1 = ok(sync(RequiresCheck1)),
                    sync_and_repeat(Log, Config, EnvInfo, Db, IMAP, Inotify,
                        MailboxPair, RequiresCheck1, DirCacheUpdate1,
                        Res, !DirCache, !IO)
                ;
                    Res1 = ok(quit),
                    Res = ok - stop
                ;
                    Res1 = eof,
                    Res = eof - immediate_restart
                ;
                    Res1 = error(Error),
                    Res = error(Error) - delayed_restart % sometimes
                )
            )
        ;
            ResSync = ok(error(PostSyncError)),
            Res = error(PostSyncError) - stop
        ;
            ResSync = eof,
            Res = eof - immediate_restart
        ;
            ResSync = error(Error),
            Res = error(Error) - delayed_restart % sometimes
        )
    ;
        ResLastModSeqValzer = error(Error),
        Res = error(Error) - stop
    ).

:- pred sync_and_repeat_2(log::in, prog_config::in, imap::in, inotify(S)::in,
    maybe_result(idle_next_outer)::out, dir_cache.update_method::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

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
                report_memory_attribution("before idle", !IO),
                idle_until_sync(Log, Config, IMAP, Inotify, Res,
                    !DirCache, !IO)
            ;
                Res = ok(sync(requires_check(CheckLocal, CheckRemote)))
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
    maybe_result(idle_next_outer)::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

idle_until_sync(Log, Config, IMAP, Inotify, Res, !DirCache, !IO) :-
    IdleTimeoutSecs = Config ^ idle_timeout_seconds,
    gettimeofday(StartTime, _StartUsec, !IO),
    EndTime = StartTime + IdleTimeoutSecs,

    % Send IDLE command.
    log_notice(Log, "Idling", !IO),
    idle(IMAP, Res0, !IO),
    (
        Res0 = ok(result(Status, IdleText, IdleAlerts)),
        report_alerts(Log, IdleAlerts, !IO),
        (
            Status = continue,
            log_debug(Log, IdleText, !IO),
            idle_until_done(Log, Config, IMAP, Inotify, EndTime, Res1,
                !DirCache, !IO),
            (
                Res1 = ok(restart_idle),
                idle_until_sync(Log, Config, IMAP, Inotify, Res,
                    !DirCache, !IO)
            ;
                Res1 = ok(sync(RequiresCheck)),
                Res = ok(sync(RequiresCheck))
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
    int::in, maybe_result(idle_next)::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

idle_until_done(Log, Config, IMAP, Inotify, EndTime, Res, !DirCache, !IO) :-
    RequiresCheck0 = requires_check(skip, skip),
    idle_loop(Log, Config, IMAP, Inotify, EndTime, EndTime, RequiresCheck0,
        Res0, !DirCache, !IO),
    (
        Res0 = ok(IdleNext),
        % Send IDLE DONE.
        idle_done(IMAP, Res1, !IO),
        (
            Res1 = ok(result(Status, DoneText, DoneAlerts)),
            report_alerts(Log, DoneAlerts, !IO),
            (
                Status = ok,
                Res = ok(IdleNext)
           ;
                ( Status = no
                ; Status = bad
                ; Status = bye
                ; Status = continue
                ),
                Res = error("unexpected response to IDLE DONE: " ++
                    DoneText)
            )
        ;
            Res1 = eof,
            Res = eof
        ;
            Res1 = error(Error),
            log_error(Log, "failed to send DONE: " ++ Error, !IO),
            % Restart in case we failed to write because the connection was
            % closed.
            Res = eof
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred idle_loop(log::in, prog_config::in, imap::in, inotify(S)::in, int::in,
    int::in, requires_check::in, maybe_result(idle_next)::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

idle_loop(Log, Config, IMAP, Inotify, EndTimeA, EndTimeB0, !.RequiresCheck,
        Res, !DirCache, !IO) :-
    get_filedes(Inotify, InotifyFd),
    get_read_filedes(IMAP, ResIMAP_Fd, !IO),
    (
        ResIMAP_Fd = ok(IMAP_Fd),
        get_sigusr1_count(SIGUSR1_Count0, !IO),
        gettimeofday(T0, _, !IO),
        TimeoutSecs = min(EndTimeA, EndTimeB0) - T0,
        select_read([IMAP_Fd, InotifyFd], TimeoutSecs, Res0, !IO),
        (
            Res0 = ready(_NumReady, FdSet),
            ( fd_isset(FdSet, InotifyFd) ->
                % Buffer events for later.
                sleep(1, !IO),
                buffer_events(Inotify, Res1, !IO),
                (
                    Res1 = ok,
                    log_debug(Log, "Waiting for local changes to settle", !IO),
                    gettimeofday(T1, _, !IO),
                    EndTimeB = T1 + Config ^ quiesce_seconds,
                    !RequiresCheck ^ check_local := check
                ;
                    Res1 = error(_),
                    EndTimeB = EndTimeB0
                )
            ;
                Res1 = ok,
                EndTimeB = EndTimeB0
            ),
            (
                Res1 = ok,
                fd_isset(FdSet, IMAP_Fd)
            ->
                do_read_idle_response(Log, IMAP, Res2, !RequiresCheck, !IO)
            ;
                Res2 = ok
            ),
            (
                Res2 = ok,
                ( skip_all(!.RequiresCheck) ->
                    idle_loop(Log, Config, IMAP, Inotify, EndTimeA, EndTimeB,
                        !.RequiresCheck, Res, !DirCache, !IO)
                ; !.RequiresCheck = requires_check(check, skip) ->
                    idle_loop(Log, Config, IMAP, Inotify, EndTimeA, EndTimeB,
                        !.RequiresCheck, Res, !DirCache, !IO)
                ;
                    Res = ok(sync(!.RequiresCheck))
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
                !RequiresCheck ^ check_remote := check
            ;
                SyncOnIdleTimeout = no
            ),
            ( skip_all(!.RequiresCheck) ->
                Res = ok(restart_idle)
            ;
                Res = ok(sync(!.RequiresCheck))
            )
        ;
            Res0 = interrupt,
            get_sigusr1_count(SIGUSR1_Count, !IO),
            ( SIGUSR1_Count > SIGUSR1_Count0 ->
                log_notice(Log, "Received SIGUSR1, forcing sync.\n", !IO),
                Res = ok(sync(requires_check(check, check)))
            ;
                log_notice(Log, "Interrupted.\n", !IO),
                Res = ok(quit)
            )
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        ResIMAP_Fd = error(Error),
        Res = error(Error)
    ).

:- pred do_read_idle_response(log::in, imap::in, maybe_result::out,
    requires_check::in, requires_check::out, io::di, io::uo) is det.

do_read_idle_response(Log, IMAP, Res, !RequiresCheck, !IO) :-
    read_single_idle_response(IMAP, ResRead, !IO),
    (
        ResRead = ok(stop_idling - Alerts),
        report_alerts(Log, Alerts, !IO),
        Res = ok,
        !RequiresCheck ^ check_remote := check
    ;
        ResRead = ok(continue_idling - Alerts),
        report_alerts(Log, Alerts, !IO),
        Res = ok
    ;
        ResRead = ok(bye - Alerts),
        log_notice(Log, "Received BYE from server.", !IO),
        report_alerts(Log, Alerts, !IO),
        Res = eof
    ;
        ResRead = eof,
        Res = eof
    ;
        ResRead = error(Error),
        Res = error(Error)
    ).

:- pred skip_all(requires_check::in) is semidet.

skip_all(requires_check(skip, skip)).

:- pred sleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    sleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    sleep(N);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
