%-----------------------------------------------------------------------------%

:- module prog_config.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type prog_config
    --->    prog_config(
                db_filename :: string,
                maildir_root :: maildir_root,
                fsync :: fsync,
                local_mailbox_name :: local_mailbox_name,
                hostport    :: string,
                username    :: username,
                password    :: password,
                mailbox     :: mailbox,
                idle_timeout_secs :: int,
                sync_on_idle_timeout :: bool
            ).

:- type maildir_root
    --->    maildir_root(string).

:- type fsync
    --->    do_fsync
    ;       do_not_fsync.

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
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module int.
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
    io::di, io::uo) is det.

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
    list(string)::in, list(string)::out, io::di, io::uo) is det.

make_prog_config(Config, ProgConfig, !Errors, !IO) :-
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

    ( nonempty(Config, "imap", "password", Password0) ->
        Password = password(Password0)
    ;
        Password = password(""),
        cons("missing imap.password", !Errors)
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

    ProgConfig = prog_config(DbFileName, MaildirRoot, Fsync, LocalMailboxName,
        Host, UserName, Password, RemoteMailboxName,
        IdleTimeoutSecs, SyncOnIdleTimeout).

:- pred nonempty(config::in, config.section::in, string::in,
    string::out) is semidet.

nonempty(Config, Section, Key, Value) :-
    search_config(Config, Section, Key, Value),
    Value \= "".

:- pred bool(string::in, bool::out) is semidet.

bool(String, Bool) :-
    bool_lower(to_lower(String), Bool).

:- pred bool_lower(string::in, bool::out) is semidet.

bool_lower("yes", yes).
bool_lower("no", no).
bool_lower("true", yes).
bool_lower("false", no).

:- pred positive_int(string::in, int::out) is semidet.

positive_int(String, Int) :-
    string.to_int(String, Int),
    Int > 0.

:- func max_idle_timeout_secs = int.

max_idle_timeout_secs = 29 * 60.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
