% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module prog_config.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module log.
:- import_module imap.
:- import_module imap.types.
:- import_module shell_word.

%-----------------------------------------------------------------------------%

:- type prog_config
    --->    prog_config(
                restart_after_error_seconds :: maybe(int),
                maybe_log_filename :: maybe(string),
                log_level :: level,
                db_filename :: string,
                maildir_root :: maildir_root,
                fsync :: fsync,
                buckets :: buckets,
                quiesce_seconds :: int,
                local_mailbox_name :: local_mailbox_name,
                host_name_only :: string,
                port :: int,
                auth_method :: auth_method,
                mailbox :: mailbox,
                mailbox_string :: string,
                idle :: bool,
                idle_timeout_seconds :: int,
                sync_on_idle_timeout :: bool,
                certificate_file :: maybe(string),
                command_post_sync_local_change :: maybe(list(word))
            ).

:- type maildir_root
    --->    maildir_root(string).

:- type fsync
    --->    do_fsync
    ;       do_not_fsync.

:- type buckets
    --->    use_buckets
    ;       no_buckets.

:- type local_mailbox_name
    --->    local_mailbox_name(string).

:- type local_mailbox_path
    --->    local_mailbox_path(string).

:- func make_local_mailbox_path(prog_config, local_mailbox_name)
    = local_mailbox_path.

:- type auth_method
    --->    auth_plain(
                username :: username,
                password :: maybe(password)
            )
    ;       auth_oauth2(
                oauth2_command :: list(word),
                oauth2_string :: maybe(oauth2_base64_string)
            ).

%-----------------------------------------------------------------------------%

:- type load_prog_config_result
    --->    ok(prog_config)
    ;       errors(list(string)).

:- pred load_prog_config(string::in, string::in, load_prog_config_result::out,
    io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module parsing_utils.
:- import_module string.

:- import_module config.

%-----------------------------------------------------------------------------%

make_local_mailbox_path(Config, local_mailbox_name(MailboxName)) = Path :-
    MaildirRoot = Config ^ maildir_root,
    MaildirRoot = maildir_root(DirName),
    Path = local_mailbox_path(DirName / MailboxName).

%-----------------------------------------------------------------------------%

load_prog_config(FileName, PairingName, Res, !IO) :-
    load_config_file(FileName, LoadRes, !IO),
    (
        LoadRes = ok(Config),
        make_prog_config(Config, PairingName, ProgConfig, [], RevErrors, !IO),
        (
            RevErrors = [],
            Res = ok(ProgConfig)
        ;
            RevErrors = [_ | _],
            Res = errors(reverse(RevErrors))
        )
    ;
        LoadRes = error(Error),
        Res = errors([io.error_message(Error)])
    ).

:- pred make_prog_config(config::in, string::in, prog_config::out,
    list(string)::in, list(string)::out, io::di, io::uo) is cc_multi.

make_prog_config(Config, PairingName, ProgConfig, !Errors, !IO) :-
    ( nonempty(Config, "general", "restart_after_error_seconds", Seconds0) ->
        ( nonnegative_int(Seconds0, Seconds) ->
            RestartAfterErrorSeconds = yes(Seconds)
        ;
            RestartAfterErrorSeconds = no,
            cons("general.restart_after_error_seconds invalid: " ++ Seconds0, !Errors)
        )
    ;
        RestartAfterErrorSeconds = no
    ),

    ( nonempty(Config, "log", "file", LogFileName) ->
        MaybeLogFileName = yes(LogFileName)
    ;
        MaybeLogFileName = no
    ),

    ( nonempty(Config, "log", "level", Level0) ->
        ( log_level(Level0, Level1) ->
            LogLevel = Level1
        ;
            LogLevel = default_log_level,
            cons("log.level invalid: " ++ Level0, !Errors)
        )
    ;
        LogLevel = default_log_level
    ),

    ( nonempty(Config, "local", "state", DbFileName0) ->
        DbFileName = DbFileName0
    ;
        DbFileName = "",
        cons("missing local.state", !Errors)
    ),

    ( nonempty(Config, "local", "maildir", MaildirRoot0) ->
        MaildirRoot = maildir_root(MaildirRoot0)
    ;
        MaildirRoot = maildir_root(""),
        cons("missing maildir.path", !Errors)
    ),

    ( nonempty(Config, "local", "fsync", Fsync0) ->
        ( bool(Fsync0, FsyncBool) ->
            (
                FsyncBool = yes,
                Fsync = do_fsync
            ;
                FsyncBool = no,
                Fsync = do_not_fsync
            )
        ;
            Fsync = do_fsync,
            cons("invalid local.fsync: " ++ Fsync0, !Errors)
        )
    ;
        Fsync = do_fsync
    ),

    ( nonempty(Config, "local", "buckets", Buckets0) ->
        ( bool(Buckets0, BucketsBool) ->
            (
                BucketsBool = yes,
                Buckets = use_buckets
            ;
                BucketsBool = no,
                Buckets = no_buckets
            )
        ;
            Buckets = no_buckets,
            cons("invalid local.buckets: " ++ Buckets0, !Errors)
        )
    ;
        Buckets = no_buckets
    ),

    ( nonempty(Config, "local", "quiesce_seconds", Quiesce0) ->
        ( nonnegative_int(Quiesce0, QuiesceInt) ->
            Quiesce = QuiesceInt
        ;
            cons("invalid local.quiesce_seconds: " ++ Quiesce0, !Errors),
            Quiesce = 0
        )
    ;
        Quiesce = 10
    ),

    ( nonempty(Config, "imap", "host", HostNameOnly0) ->
        HostNameOnly = HostNameOnly0
    ;
        HostNameOnly = "",
        cons("missing imap.host", !Errors)
    ),

    ( nonempty(Config, "imap", "port", PortString) ->
        (
            string.to_int(PortString, PortInt),
            PortInt > 0,
            PortInt =< 0xffff
        ->
            Port = PortInt
        ;
            Port = 0,
            cons("invalid imap.port: " ++ PortString, !Errors)
        )
    ;
        Port = 993
    ),

    ( nonempty(Config, "imap", "auth", AuthMethod0) ->
        string.to_lower(AuthMethod0, AuthMethodLower0),
        ( AuthMethodLower0 = "plain" ->
            ( nonempty(Config, "imap", "auth_plain_username", UserName0) ->
                UserName = username(UserName0)
            ;
                UserName = username(""),
                cons("missing imap.auth_plain_username", !Errors)
            ),

            ( nonempty(Config, "imap", "auth_plain_password", Password) ->
                MaybePassword = yes(password(Password))
            ;
                MaybePassword = no
            ),

            AuthMethod = auth_plain(UserName, MaybePassword)
        ; AuthMethodLower0 = "oauth2" ->
            (
                nonempty(Config, "imap", "auth_oauth2_command",
                    OAuthCommandString)
            ->
                parse_command(OAuthCommandString, MaybeOAuthCommand, !Errors),
                (
                    MaybeOAuthCommand = yes(OAuthCommand)
                ;
                    MaybeOAuthCommand = no,
                    % Error already logged.
                    OAuthCommand = []
                ),
                AuthMethod = auth_oauth2(OAuthCommand, no)
            ;
                AuthMethod = auth_plain(username(""), no),
                cons("missing imap.auth_oauth2_command", !Errors)
            )
        ;
            AuthMethod = auth_plain(username(""), no),
            cons("unknown imap.auth: " ++ AuthMethod0, !Errors)
        )
    ;
        AuthMethod = auth_plain(username(""), no),
        cons("missing imap.auth", !Errors)
    ),

    ( nonempty(Config, "imap", "idle", Idle0) ->
        ( bool(Idle0, Idle1) ->
            Idle = Idle1
        ;
            Idle = no,
            cons("imap.idle invalid: " ++ Idle0, !Errors)
        )
    ;
        Idle = no
    ),

    ( nonempty(Config, "imap", "idle_timeout_minutes", Mins0) ->
        ( positive_int(Mins0, Mins) ->
            IdleTimeoutSecs = min(Mins * 60, max_idle_timeout_seconds)
        ;
            IdleTimeoutSecs = 0,
            cons("imap.idle_timeout_minutes invalid: " ++ Mins0, !Errors)
        )
    ;
        IdleTimeoutSecs = max_idle_timeout_seconds
    ),

    ( nonempty(Config, "imap", "sync_on_idle_timeout", Value) ->
        ( bool(Value, Bool) ->
            SyncOnIdleTimeout = Bool
        ;
            SyncOnIdleTimeout = no,
            cons("imap.sync_on_idle_timeout invalid: " ++ Value, !Errors)
        )
    ;
        SyncOnIdleTimeout = no
    ),

    ( nonempty(Config, "ssl", "certificate_file", CertificateFile) ->
        MaybeCertificateFile = yes(CertificateFile)
    ;
        MaybeCertificateFile = no
    ),

    PairingSectionName = "pairing " ++ PairingName,
    ( search(Config, PairingSectionName, PairingSection) ->
        ( nonempty(PairingSection, "local", LocalMailboxName0) ->
            LocalMailboxName = local_mailbox_name(LocalMailboxName0)
        ;
            LocalMailboxName = local_mailbox_name(""),
            cons("missing local in section: " ++ PairingSectionName, !Errors)
        ),

        ( nonempty(PairingSection, "remote", RemoteMailboxName0) ->
            RemoteMailboxName = mailbox(RemoteMailboxName0),
            RemoteMailboxString = RemoteMailboxName0
        ;
            RemoteMailboxName = mailbox(""),
            RemoteMailboxString = "",
            cons("missing remote in section: " ++ PairingSectionName, !Errors)
        )
    ;
        LocalMailboxName = local_mailbox_name(""),
        RemoteMailboxName = mailbox(""),
        RemoteMailboxString = "",
        cons("missing section: " ++ PairingSectionName, !Errors)
    ),

    ( nonempty(Config, "command", "post_sync_local_change", Command0) ->
        parse_command(Command0, CommandPostSyncLocal, !Errors)
    ;
        CommandPostSyncLocal = maybe.no
    ),

    ProgConfig = prog_config(RestartAfterErrorSeconds,
        MaybeLogFileName, LogLevel, DbFileName,
        MaildirRoot, Fsync, Buckets, Quiesce, LocalMailboxName,
        HostNameOnly, Port, AuthMethod,
        RemoteMailboxName, RemoteMailboxString,
        Idle, IdleTimeoutSecs, SyncOnIdleTimeout, MaybeCertificateFile,
        CommandPostSyncLocal).

:- pred nonempty(config::in, config.section::in, string::in,
    string::out) is semidet.

nonempty(Config, Section, Key, Value) :-
    search_config_section(Config, Section, SectionMap),
    nonempty(SectionMap, Key, Value).

:- pred nonempty(section_map::in, string::in, string::out) is semidet.

nonempty(SectionMap, Key, Value) :-
    search_section(SectionMap, Key, Value),
    Value \= "".

:- pred positive_int(string::in, int::out) is semidet.

positive_int(String, Int) :-
    string.to_int(String, Int),
    Int > 0.

:- pred nonnegative_int(string::in, int::out) is semidet.

nonnegative_int(String, Int) :-
    string.to_int(String, Int),
    Int >= 0.

:- pred bool(string::in, bool::out) is semidet.

bool(String, Bool) :-
    bool_lower(to_lower(String), Bool).

:- pred bool_lower(string::in, bool::out) is semidet.

bool_lower("yes", yes).
bool_lower("no", no).
bool_lower("true", yes).
bool_lower("false", no).

:- pred log_level(string::in, log.level::out) is semidet.

log_level(String, Level) :-
    log_level_lower(to_lower(String), Level).

:- pred log_level_lower(string::in, log.level::out) is semidet.

log_level_lower("error", error).
log_level_lower("warning", warning).
log_level_lower("notice", notice).
log_level_lower("info", info).
log_level_lower("debug", debug).

:- func default_log_level = log.level.

default_log_level = info.

:- func max_idle_timeout_seconds = int.

max_idle_timeout_seconds = 29 * 60.

:- pred parse_command(string::in, maybe(list(word))::out,
    list(string)::in, list(string)::out) is cc_multi.

parse_command(S0, MaybeWords, !Errors) :-
    shell_word.split(S0, ParseResult),
    (
        ParseResult = ok(Words),
        MaybeWords = yes(Words)
    ;
        (
            ParseResult = error(yes(Message), _Line, _Column)
        ;
            ParseResult = error(no, _Line, _Column),
            Message = "parse error"
        ),
        Error = string.format("%s in value: %s", [s(Message), s(S0)]),
        cons(Error, !Errors),
        MaybeWords = no
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
