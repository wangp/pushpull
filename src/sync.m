%-----------------------------------------------------------------------------%

:- module sync.
:- interface.

:- import_module io.

:- import_module database.
:- import_module dir_cache.
:- import_module imap.
:- import_module imap.types.
:- import_module inotify.
:- import_module log.
:- import_module maybe_result.
:- import_module prog_config.

:- type shortcut
    --->    shortcut(
                check_local :: check,
                check_remote :: check
            ).

:- type check
    --->    check
    ;       skip.

:- pred sync_mailboxes(log::in, prog_config::in, database::in, imap::in,
    inotify(S)::in, mailbox_pair::in, mod_seq_valzer::in, shortcut::in,
    update_method::in, maybe_result::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module std_util.
:- import_module string.

:- include_module sync.download.
:- include_module sync.flags_local.
:- include_module sync.flags_remote.
:- include_module sync.update_local.
:- include_module sync.update_remote.
:- include_module sync.upload.

:- import_module sync.download.
:- import_module sync.flags_local.
:- import_module sync.flags_remote.
:- import_module sync.update_local.
:- import_module sync.update_remote.
:- import_module sync.upload.

:- import_module dir_cache.
:- import_module quote_arg.
:- import_module shell_word.

%-----------------------------------------------------------------------------%

sync_mailboxes(Log, Config, Db, IMAP, Inotify, MailboxPair, LastModSeqValzer,
        Shortcut, DirCacheUpdate, !:Res, !DirCache, !IO) :-
    Shortcut = shortcut(CheckLocal0, CheckRemote),
    % It might be better to get set of valid UIDs first, then use that
    % as part of update_db_remote_mailbox and for detecting expunges.
    (
        CheckRemote = check,
        log_debug(Log, "Update remote mailbox state", !IO),
        update_db_remote_mailbox(Log, Config, Db, IMAP, MailboxPair,
            LastModSeqValzer, !:Res, !IO)
    ;
        CheckRemote = skip,
        !:Res = ok
    ),
    force_check_local(Inotify, CheckLocal0, CheckLocal, !Res, !IO),
    (
        !.Res = ok,
        CheckLocal = check
    ->
        log_debug(Log, "Update local mailbox state", !IO),
        update_db_local_mailbox(Log, Config, Db, Inotify, MailboxPair,
            DirCacheUpdate, ResUpdateLocal, !DirCache, !IO),
        !:Res = from_maybe_error(ResUpdateLocal)
    ;
        true
    ),
    (
        !.Res = ok,
        CheckRemote = check
    ->
        % Propagate flags first to allow pairings with previously-expunged
        % messages to be be reset, and thus downloaded in the following steps.
        log_debug(Log, "Propagate flag deltas from remote mailbox", !IO),
        propagate_flag_deltas_from_remote(Log, Config, Db, MailboxPair,
            ResPropFlagsFromRemote, !DirCache, !IO),
        !:Res = from_maybe_error(ResPropFlagsFromRemote)
    ;
        true
    ),
    (
        !.Res = ok,
        CheckLocal = check
    ->
        log_debug(Log, "Propagate flag deltas from local mailbox", !IO),
        propagate_flag_deltas_from_local(Log, Config, Db, IMAP, MailboxPair,
            !:Res, !IO)
    ;
        true
    ),
    ( !.Res = ok ->
        log_debug(Log, "Download remote messages", !IO),
        download_unpaired_remote_messages(Log, Config, Db, IMAP, MailboxPair,
            !:Res, !DirCache, !IO),
        ( !.Res = ok ->
            % When a remote message is paired with an existing local message
            % then their flags may need to be propagated either way.
            propagate_flag_deltas(Log, Config, Db, IMAP, MailboxPair, !:Res,
                !DirCache, !IO)
        ;
            true
        )
    ;
        true
    ),
    ( !.Res = ok ->
        log_debug(Log, "Upload local messages", !IO),
        upload_unpaired_local_messages(Log, Config, Db, IMAP, MailboxPair,
            !.DirCache, !:Res, !IO)
    ;
        true
    ),
    ( !.Res = ok ->
        log_debug(Log, "Delete expunged pairings", !IO),
        delete_expunged_pairings(Db, ResDelete, !IO),
        !:Res = from_maybe_error(ResDelete)
    ;
        true
    ),
    ( !.Res = ok ->
        call_command_post_sync(Log, Config, !:Res, !IO)
    ;
        true
    ).

:- pred force_check_local(inotify(S)::in, check::in, check::out,
    maybe_result::in, maybe_result::out, io::di, io::uo) is det.

force_check_local(Inotify, CheckLocal0, CheckLocal, Res0, Res, !IO) :-
    (
        Res0 = ok,
        (
            CheckLocal0 = skip,
            get_queue_length(Inotify, ResQueue, !IO),
            (
                ResQueue = ok(Length),
                Res = ok,
                CheckLocal = ( Length > 0 -> check ; skip )
            ;
                ResQueue = error(Error),
                Res = error(Error),
                CheckLocal = skip
            )
        ;
            CheckLocal0 = check,
            CheckLocal = check,
            Res = ok
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = Res0,
        CheckLocal = CheckLocal0
    ).

:- pred propagate_flag_deltas(log::in, prog_config::in, database::in,
    imap::in, mailbox_pair::in, maybe_result::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

propagate_flag_deltas(Log, Config, Db, IMAP, MailboxPair, Res,
        !DirCache, !IO) :-
    propagate_flag_deltas_from_remote(Log, Config, Db, MailboxPair,
        Res0, !DirCache, !IO),
    (
        Res0 = ok,
        propagate_flag_deltas_from_local(Log, Config, Db, IMAP, MailboxPair,
            Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred call_command_post_sync(log::in, prog_config::in,
    maybe_result::out, io::di, io::uo) is det.

call_command_post_sync(Log, Config, Res, !IO) :-
    MaybeCommand = Config ^ command_post_sync,
    (
        MaybeCommand = yes(Words),
        Command = join_list(" ", map(compose(quote_arg, word_string), Words)),
        log_info(Log, "Calling command: " ++ Command, !IO),
        io.call_system(Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                Res = ok
            ;
                Res = error("post_sync command returned exit status " ++
                    string.from_int(ExitStatus))
            )
        ;
            CallRes = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        MaybeCommand = no,
        log_debug(Log, "No post_sync command configured.", !IO),
        Res = ok
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
