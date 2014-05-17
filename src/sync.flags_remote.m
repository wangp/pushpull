%-----------------------------------------------------------------------------%

:- module sync.flags_remote.
:- interface.

:- pred propagate_flag_deltas_from_local(prog_config::in, database::in,
    imap::in, local_mailbox::in, remote_mailbox::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module set.
:- import_module string.

:- import_module flag_delta.
:- import_module log.
:- import_module maildir.

%-----------------------------------------------------------------------------%

propagate_flag_deltas_from_local(_Config, Db, IMAP, LocalMailbox, RemoteMailbox,
        Res, !IO) :-
    search_pending_flag_deltas_from_local(Db, LocalMailbox, RemoteMailbox,
        ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        list.foldl2(propagate_flag_deltas_from_local_2(Db, IMAP),
            Pendings, ok, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_local_2(database::in, imap::in,
    pending_flag_deltas::in, maybe_error::in, maybe_error::out, io::di, io::uo)
    is det.

propagate_flag_deltas_from_local_2(Db, IMAP, Pending, Res0, Res, !IO) :-
    (
        Res0 = ok,
        propagate_flag_deltas_from_local_3(Db, IMAP, Pending, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred propagate_flag_deltas_from_local_3(database::in, imap::in,
    pending_flag_deltas::in, maybe_error::out, io::di, io::uo) is det.

propagate_flag_deltas_from_local_3(Db, IMAP, Pending, Res, !IO) :-
    Pending = pending_flag_deltas(PairingId,
        MaybeUnique, LocalFlags0, LocalExpunged,
        MaybeUID, RemoteFlags0, RemoteExpunged),
    imply_deleted_flag(LocalExpunged, LocalFlags0, LocalFlags1),
    imply_deleted_flag(RemoteExpunged, RemoteFlags0, RemoteFlags1),
    apply_flag_deltas(RemoteFlags1, RemoteFlags, LocalFlags1, LocalFlags),

    Flags0 = RemoteFlags0 ^ cur_set,
    Flags = RemoteFlags ^ cur_set,
    AddFlags = Flags `difference` Flags0,
    RemoveFlags = Flags0 `difference` Flags,
    (
        MaybeUID = yes(UID),
        store_remote_flags_add_rm(IMAP, UID, AddFlags, RemoveFlags, Res0, !IO),
        (
            Res0 = ok,
            record_local_flag_deltas_applied_to_remote(Db, PairingId,
                LocalFlags, RemoteFlags, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        MaybeUID = no,
        % If the remote message was previously expunged, but the local message
        % was undeleted, then reset the pairing so that the message can be
        % re-added to the remote mailbox with a new UID.
        (
            MaybeUnique = yes(uniquename(Unique)),
            RemoteExpunged = expunged,
            contains(RemoveFlags, system(deleted))
        ->
            io.format("Resurrecting message %s\n", [s(Unique)], !IO),
            reset_pairing_remote_message(Db, PairingId, Res, !IO)
        ;
            record_local_flag_deltas_inapplicable_to_remote(Db,
                PairingId, LocalFlags, Res, !IO)
        )
    ).

:- pred store_remote_flags_add_rm(imap::in, uid::in,
    set(flag)::in, set(flag)::in, maybe_error::out, io::di, io::uo) is det.

store_remote_flags_add_rm(IMAP, UID, AddFlags, RemoveFlags, Res, !IO) :-
    % Would it be preferable to read back the actual flags from the server?
    store_remote_flags_change(IMAP, UID, remove, RemoveFlags, Res0, !IO),
    (
        Res0 = ok,
        store_remote_flags_change(IMAP, UID, add, AddFlags, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred store_remote_flags_change(imap::in, uid::in, store_operation::in,
    set(flag)::in, maybe_error::out, io::di, io::uo) is det.

store_remote_flags_change(IMAP, UID, Operation, ChangeFlags, Res, !IO) :-
    ( set.empty(ChangeFlags) ->
        Res = ok
    ;
        uid_store(IMAP, singleton_sequence_set(UID), Operation, silent,
            to_sorted_list(ChangeFlags), result(ResAdd, Text, Alerts), !IO),
        report_alerts(Alerts, !IO),
        (
            ResAdd = ok_with_data(_),
            Res = ok
        ;
            ( ResAdd = no
            ; ResAdd = bad
            ; ResAdd = bye
            ; ResAdd = continue
            ; ResAdd = error
            ),
            Res = error("unexpected response to UID STORE: " ++ Text)
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
