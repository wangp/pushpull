% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module sync.flags_local.
:- interface.

:- import_module maybe.

:- import_module dir_cache.

:- pred propagate_flag_deltas_from_remote(log::in, prog_config::in,
    database::in, mailbox_pair::in, maybe_error::out,
    dir_cache::in, dir_cache::out, bool::in, bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module integer.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module flag_delta.
:- import_module lowio.
:- import_module maildir.
:- import_module path.

%-----------------------------------------------------------------------------%

propagate_flag_deltas_from_remote(Log, Config, Db, MailboxPair, Res,
        !DirCache, !LocalChanges, !IO) :-
    search_pending_flag_deltas_from_remote(Db, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        propagate_flag_deltas_from_remote_2(Log, Config, Db, Pendings, Res,
            !DirCache, !LocalChanges, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_remote_2(log::in, prog_config::in,
    database::in, list(pending_flag_deltas)::in, maybe_error::out,
    dir_cache::in, dir_cache::out, bool::in, bool::out, io::di, io::uo) is det.

propagate_flag_deltas_from_remote_2(Log, Config, Db, Pendings, Res,
        !DirCache, !LocalChanges, !IO) :-
    (
        Pendings = [],
        Res = ok
    ;
        Pendings = [Head | Tail],
        propagate_flag_deltas_from_remote_3(Log, Config, Db, Head, Res0,
            !DirCache, !LocalChanges, !IO),
        (
            Res0 = ok,
            propagate_flag_deltas_from_remote_2(Log, Config, Db, Tail, Res,
                !DirCache, !LocalChanges, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred propagate_flag_deltas_from_remote_3(log::in, prog_config::in,
    database::in, pending_flag_deltas::in, maybe_error::out,
    dir_cache::in, dir_cache::out, bool::in, bool::out, io::di, io::uo) is det.

propagate_flag_deltas_from_remote_3(Log, Config, Db, Pending, Res,
        !DirCache, !LocalChanges, !IO) :-
    Pending = pending_flag_deltas(PairingId,
        MaybeUnique, LocalFlags0, LocalExpunged,
        MaybeUID, RemoteFlags0, _RemoteExpunged),
    apply_flag_deltas(LocalFlags0, LocalFlags, RemoteFlags0, RemoteFlags),

    Flags0 = LocalFlags0 ^ cur_set,
    Flags = LocalFlags ^ cur_set,
    AddFlags = Flags `difference` Flags0,
    RemoveFlags = Flags0 `difference` Flags,
    (
        MaybeUnique = yes(Unique),
        expect(unify(LocalExpunged, exists), $module, $pred),
        store_local_flags_add_rm(Log, Config, Unique, AddFlags, RemoveFlags,
            Res0, !DirCache, !LocalChanges, !IO),
        (
            Res0 = ok,
            record_remote_flag_deltas_applied_to_local(Db, PairingId,
                LocalFlags, RemoteFlags, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        MaybeUnique = no,
        % If the local message was previously expunged, but the remote message
        % was undeleted then reset the pairing so that the message can be
        % re-added to the local mailbox with a new uniquename.
        % XXX simplify?
        (
            MaybeUID = yes(uid(UID)),
            LocalExpunged = expunged,
            contains(RemoveFlags, system(deleted))
        ->
            log_notice(Log, "Resurrecting message " ++ to_string(UID), !IO),
            reset_pairing_local_message(Db, PairingId, Res, !IO)
        ;
            record_remote_flag_deltas_inapplicable_to_local(Db, PairingId,
                RemoteFlags, Res, !IO)
        )
    ).

:- pred store_local_flags_add_rm(log::in, prog_config::in, uniquename::in,
    set(flag)::in, set(flag)::in, maybe_error::out,
    dir_cache::in, dir_cache::out, bool::in, bool::out, io::di, io::uo) is det.

store_local_flags_add_rm(Log, Config, Unique, AddFlags, RemoveFlags, Res,
        !DirCache, !LocalChanges, !IO) :-
    find_file(!.DirCache, Unique, ResFind),
    (
        ResFind = found(OldDirName, OldBaseName, MaybeInfoSuffix0),
        (
            MaybeInfoSuffix0 = no,
            InfoSuffix = flags_to_info_suffix(AddFlags)
        ;
            MaybeInfoSuffix0 = yes(InfoSuffix0),
            add_remove_standard_flags(AddFlags, RemoveFlags,
                InfoSuffix0, InfoSuffix)
        ),
        ( change_to_cur(OldDirName, NewDirName) ->
            make_message_basename(Unique, yes(InfoSuffix), NewBaseName),
            (
                OldDirName = NewDirName,
                OldBaseName = NewBaseName
            ->
                Res = ok
            ;
                OldPath = OldDirName / OldBaseName,
                NewPath = NewDirName / NewBaseName,
                ( OldDirName = NewDirName ->
                    log_notice(Log,
                        format("Renaming %s to %s\n",
                            [s(OldPath ^ path), s(NewBaseName ^ bn)]), !IO)
                ;
                    log_notice(Log,
                        format("Renaming %s to %s\n",
                            [s(OldPath ^ path), s(NewPath ^ path)]), !IO)
                ),
                io.rename_file(OldPath ^ path, NewPath ^ path, ResRename, !IO),
                (
                    ResRename = ok,
                    !:LocalChanges = yes,
                    maybe_fsync_dir(Config, NewDirName, ResFsync, !IO),
                    (
                        ResFsync = ok,
                        (
                            update_for_rename(OldDirName, OldBaseName,
                                NewDirName, NewBaseName, !DirCache)
                        ->
                            Res = ok
                        ;
                            Res = error("update_for_rename failed")
                        )
                    ;
                        ResFsync = error(Error),
                        Res = error(Error)
                    )
                ;
                    ResRename = error(Error),
                    Res = error(io.error_message(Error))
                )
            )
        ;
            Res = error("change_to_cur failed")
        )
    ;
        ResFind = not_found,
        Unique = uniquename(UniqueString),
        Res = error("missing uniquename " ++ UniqueString)
    ;
        ResFind = found_but_unexpected(path(Path)),
        Res = error("found unique name but unexpected: " ++ Path)
    ).

:- pred maybe_fsync_dir(prog_config::in, dirname::in, maybe_error::out,
    io::di, io::uo) is det.

maybe_fsync_dir(Config, dirname(DirName), Res, !IO) :-
    Fsync = Config ^ fsync,
    (
        Fsync = do_fsync,
        lowio.fsync_dir(DirName, Res, !IO)
    ;
        Fsync = do_not_fsync,
        Res = ok
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
