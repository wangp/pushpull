%-----------------------------------------------------------------------------%

:- module sync.flags_local.
:- interface.

:- import_module dir_cache.

:- pred propagate_flag_deltas_from_remote(prog_config::in, database::in,
    mailbox_pair::in, dir_cache::in, maybe_error::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module integer.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module flag_delta.
:- import_module maildir.

%-----------------------------------------------------------------------------%

propagate_flag_deltas_from_remote(Config, Db, MailboxPair, DirCache, Res, !IO)
        :-
    search_pending_flag_deltas_from_remote(Db, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        LocalMailboxName = get_local_mailbox_name(MailboxPair),
        LocalMailboxPath = make_local_mailbox_path(Config, LocalMailboxName),
        list.foldl2(propagate_flag_deltas_from_remote_2(Config, Db,
            LocalMailboxPath, DirCache), Pendings, ok, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_remote_2(prog_config::in, database::in,
    local_mailbox_path::in, dir_cache::in, pending_flag_deltas::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_from_remote_2(Config, Db, LocalMailboxPath, DirCache,
        Pending, Res0, Res, !IO) :-
    (
        Res0 = ok,
        propagate_flag_deltas_from_remote_3(Config, Db, LocalMailboxPath,
            DirCache, Pending, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_remote_3(prog_config::in, database::in,
    local_mailbox_path::in, dir_cache::in, pending_flag_deltas::in,
    maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_from_remote_3(_Config, Db, LocalMailboxPath, DirCache,
        Pending, Res, !IO) :-
    Pending = pending_flag_deltas(PairingId,
        MaybeUnique, LocalFlags0, LocalExpunged,
        MaybeUID, RemoteFlags0, RemoteExpunged),
    imply_deleted_flag(LocalExpunged, LocalFlags0, LocalFlags1),
    imply_deleted_flag(RemoteExpunged, RemoteFlags0, RemoteFlags1),
    apply_flag_deltas(LocalFlags1, LocalFlags, RemoteFlags1, RemoteFlags),

    Flags0 = LocalFlags0 ^ cur_set,
    Flags = LocalFlags ^ cur_set,
    AddFlags = Flags `difference` Flags0,
    RemoveFlags = Flags0 `difference` Flags,
    (
        MaybeUnique = yes(Unique),
        expect(unify(LocalExpunged, exists), $module, $pred),
        store_local_flags_add_rm(LocalMailboxPath, DirCache, Unique,
            AddFlags, RemoveFlags, Res0, !IO),
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
            io.format("Resurrecting message %s\n", [s(to_string(UID))], !IO),
            reset_pairing_local_message(Db, PairingId, Res, !IO)
        ;
            record_remote_flag_deltas_inapplicable_to_local(Db, PairingId,
                RemoteFlags, Res, !IO)
        )
    ).

:- pred store_local_flags_add_rm(local_mailbox_path::in, dir_cache::in,
    uniquename::in, set(flag)::in, set(flag)::in, maybe_error::out,
    io::di, io::uo) is det.

store_local_flags_add_rm(MailboxPath, DirCache, Unique, AddFlags, RemoveFlags,
        Res, !IO) :-
    find_file(DirCache, MailboxPath, Unique, ResFind),
    (
        ResFind = found(OldPath, MaybeInfoSuffix0),
        (
            MaybeInfoSuffix0 = no,
            InfoSuffix = flags_to_info_suffix(AddFlags)
        ;
            MaybeInfoSuffix0 = yes(InfoSuffix0),
            add_remove_standard_flags(AddFlags, RemoveFlags,
                InfoSuffix0, InfoSuffix)
        ),
        make_path(MailboxPath, cur, Unique, yes(InfoSuffix), NewPath),
        ( OldPath = NewPath ->
            ResRename = ok
        ;
            io.format("Renaming %s to %s\n", [s(OldPath), s(NewPath)], !IO),
            io.rename_file(OldPath, NewPath, ResRename, !IO)
        ),
        (
            ResRename = ok,
            Res = ok
        ;
            ResRename = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        ResFind = not_found,
        Unique = uniquename(UniqueString),
        Res = error("missing uniquename " ++ UniqueString)
    ;
        ResFind = found_but_unexpected(Path),
        Res = error("found unique name but unexpected: " ++ Path)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
