% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module sync.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module database.
:- import_module dir_cache.
:- import_module imap.
:- import_module imap.types.
:- import_module inotify.
:- import_module log.
:- import_module maybe_result.
:- import_module prog_config.

:- type requires_check
    --->    requires_check(
                check_local :: check,
                check_remote :: check
            ).

:- type check
    --->    check
    ;       skip.

:- pred sync_mailboxes(log::in, prog_config::in, database::in, imap::in,
    inotify(S)::in, mailbox_pair::in, mod_seq_valzer::in, requires_check::in,
    update_method::in, maybe_result(maybe_error)::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
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

:- import_module quote_arg.
:- import_module shell_word.

%-----------------------------------------------------------------------------%

sync_mailboxes(Log, Config, Db, IMAP, Inotify, MailboxPair, LastModSeqValzer,
        RequiresCheck, DirCacheUpdate, Res, !DirCache, !IO) :-
    sync_mailboxes_2(Log, Config, Db, IMAP, Inotify, MailboxPair,
        LastModSeqValzer, RequiresCheck, DirCacheUpdate, Res, !DirCache,
        no, _LocalChanges, !IO).

:- pred sync_mailboxes_2(log::in, prog_config::in, database::in, imap::in,
    inotify(S)::in, mailbox_pair::in, mod_seq_valzer::in, requires_check::in,
    update_method::in, maybe_result(maybe_error)::out,
    dir_cache::in, dir_cache::out, bool::in, bool::out, io::di, io::uo) is det.

sync_mailboxes_2(Log, Config, Db, IMAP, Inotify, MailboxPair, LastModSeqValzer,
        RequiresCheck, DirCacheUpdate, !:Res, !DirCache, !LocalChanges, !IO) :-
    RequiresCheck = requires_check(CheckLocal0, CheckRemote),
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
            ResPropFlagsFromRemote, !DirCache, !LocalChanges, !IO),
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
            !:Res, !DirCache, !LocalChanges, !IO),
        ( !.Res = ok ->
            % When a remote message is paired with an existing local message
            % then their flags may need to be propagated either way.
            propagate_flag_deltas(Log, Config, Db, IMAP, MailboxPair, !:Res,
                !DirCache, !LocalChanges, !IO)
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
        delete_expunged_pairings(Db, MailboxPair, ResDelete, !IO),
        !:Res = from_maybe_error(ResDelete)
    ;
        true
    ),
    (
        !.Res = ok,
        maybe_call_command_post_sync(Log, Config, !.LocalChanges, PostSync,
            !IO),
        !:Res = ok(PostSync)
    ;
        !.Res = eof,
        !:Res = eof
    ;
        !.Res = error(Error),
        !:Res = error(Error)
    ).

:- pred force_check_local(inotify(S)::in, check::in, check::out,
    maybe_result::in, maybe_result::out, io::di, io::uo) is det.

force_check_local(Inotify, CheckLocal0, CheckLocal, Res0, Res, !IO) :-
    (
        Res0 = ok,
        (
            CheckLocal0 = skip,
            check_queue(Inotify, ResQueue, !IO),
            (
                ResQueue = ok(empty),
                Res = ok,
                CheckLocal = skip
            ;
                ResQueue = ok(nonempty),
                Res = ok,
                CheckLocal = check
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
    dir_cache::in, dir_cache::out, bool::in, bool::out, io::di, io::uo) is det.

propagate_flag_deltas(Log, Config, Db, IMAP, MailboxPair, Res,
        !DirCache, !LocalChanges, !IO) :-
    propagate_flag_deltas_from_remote(Log, Config, Db, MailboxPair,
        Res0, !DirCache, !LocalChanges, !IO),
    (
        Res0 = ok,
        propagate_flag_deltas_from_local(Log, Config, Db, IMAP, MailboxPair,
            Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred maybe_call_command_post_sync(log::in, prog_config::in, bool::in,
    maybe_error::out, io::di, io::uo) is det.

maybe_call_command_post_sync(Log, Config, LocalChanges, Res, !IO) :-
    (
        LocalChanges = yes,
        call_command_post_sync(Log, Config, Res, !IO)
    ;
        LocalChanges = no,
        log_debug(Log, "No local file changes.", !IO),
        Res = ok
    ).

:- pred call_command_post_sync(log::in, prog_config::in,
    maybe_error::out, io::di, io::uo) is det.

call_command_post_sync(Log, Config, Res, !IO) :-
    MaybeCommand = Config ^ command_post_sync_local_change,
    (
        MaybeCommand = yes(Words),
        Command = join_list(" ", map(compose(quote_arg, word_string), Words)),
        log_notice(Log, "Calling command: " ++ Command, !IO),
        io.call_system(Command, CallRes, !IO),
        (
            CallRes = ok(ExitStatus),
            ( ExitStatus = 0 ->
                Res = ok
            ;
                Res = error(Command ++ " returned exit status " ++
                    string.from_int(ExitStatus))
            )
        ;
            CallRes = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        MaybeCommand = no,
        log_debug(Log, "No post_sync_local_change command.", !IO),
        Res = ok
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
