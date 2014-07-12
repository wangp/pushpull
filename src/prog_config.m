%-----------------------------------------------------------------------------%

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
                maybe_log_filename :: maybe(string),
                log_level :: level,
                db_filename :: string,
                maildir_root :: maildir_root,
                fsync :: fsync,
                buckets :: buckets,
                local_mailbox_name :: local_mailbox_name,
                hostport :: string,
                username :: username,
                password :: maybe(password),
                mailbox :: mailbox,
                idle :: bool,
                idle_timeout_secs :: int,
                sync_on_idle_timeout :: bool,
                command_post_sync :: maybe(list(word))
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

%-----------------------------------------------------------------------------%

:- type load_prog_config_result
    --->    ok(prog_config)
    ;       errors(list(string)).

:- pred load_prog_config(string::in, load_prog_config_result::out,
    io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module int.
:- import_module parsing_utils.
:- import_module string.

:- import_module config.

%-----------------------------------------------------------------------------%

make_local_mailbox_path(Config, local_mailbox_name(MailboxName)) = Path :-
    MaildirRoot = Config ^ maildir_root,
    MaildirRoot = maildir_root(DirName),
    Path = local_mailbox_path(DirName / MailboxName).

%-----------------------------------------------------------------------------%

load_prog_config(FileName, Res, !IO) :-
    load_config_file(FileName, LoadRes, !IO),
    (
        LoadRes = ok(Config),
        load_prog_config_2(Config, Res, !IO)
    ;
        LoadRes = error(Error),
        Res = errors([io.error_message(Error)])
    ).

:- pred load_prog_config_2(config::in, load_prog_config_result::out,
    io::di, io::uo) is cc_multi.

load_prog_config_2(Config, Res, !IO) :-
    make_prog_config(Config, ProgConfig, [], RevErrors, !IO),
    (
        RevErrors = [],
        Res = ok(ProgConfig)
    ;
        RevErrors = [_ | _],
        Res = errors(reverse(RevErrors))
    ).

:- pred make_prog_config(config::in, prog_config::out,
    list(string)::in, list(string)::out, io::di, io::uo) is cc_multi.

make_prog_config(Config, ProgConfig, !Errors, !IO) :-
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

    ( nonempty(Config, "imap", "host", Host0) ->
        Host = Host0
    ;
        Host = "",
        cons("missing imap.host", !Errors)
    ),

    ( nonempty(Config, "imap", "username", UserName0) ->
        UserName = username(UserName0)
    ;
        UserName = username(""),
        cons("missing imap.username", !Errors)
    ),

    ( nonempty(Config, "imap", "password", Password) ->
        MaybePassword = yes(password(Password))
    ;
        MaybePassword = no
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
            IdleTimeoutSecs = min(Mins * 60, max_idle_timeout_secs)
        ;
            IdleTimeoutSecs = 0,
            cons("imap.idle_timeout_minutes invalid: " ++ Mins0, !Errors)
        )
    ;
        IdleTimeoutSecs = max_idle_timeout_secs
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

    ( nonempty(Config, "pairing", "local", LocalMailboxName0) ->
        LocalMailboxName = local_mailbox_name(LocalMailboxName0)
    ;
        LocalMailboxName = local_mailbox_name(""),
        cons("missing pairing.local", !Errors)
    ),

    ( nonempty(Config, "pairing", "remote", RemoteMailboxName0) ->
        RemoteMailboxName = mailbox(RemoteMailboxName0)
    ;
        RemoteMailboxName = mailbox(""),
        cons("missing pairing.remote", !Errors)
    ),

    ( nonempty(Config, "command", "post_sync", Command0) ->
        parse_command(Command0, CommandPostSync, !Errors)
    ;
        CommandPostSync = maybe.no
    ),

    ProgConfig = prog_config(MaybeLogFileName, LogLevel, DbFileName,
        MaildirRoot, Fsync, Buckets, LocalMailboxName,
        Host, UserName, MaybePassword, RemoteMailboxName,
        Idle, IdleTimeoutSecs, SyncOnIdleTimeout, CommandPostSync).

:- pred nonempty(config::in, config.section::in, string::in,
    string::out) is semidet.

nonempty(Config, Section, Key, Value) :-
    search_config(Config, Section, Key, Value),
    Value \= "".

:- pred positive_int(string::in, int::out) is semidet.

positive_int(String, Int) :-
    string.to_int(String, Int),
    Int > 0.

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

:- func max_idle_timeout_secs = int.

max_idle_timeout_secs = 29 * 60.

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
