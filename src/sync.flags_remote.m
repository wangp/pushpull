% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module sync.flags_remote.
:- interface.

:- import_module log.
:- import_module maybe_result.

:- pred propagate_flag_deltas_from_local(log::in, prog_config::in,
    database::in, imap::in, mailbox_pair::in, maybe_result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module diet.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

:- import_module flag_delta.
:- import_module log_help.
:- import_module maildir.

    % Group UIDs which require the same set of flag modifications.
    %
:- type groups == map(changes, group).

:- type changes
    --->    changes(
                add_flags   :: set(flag),
                rm_flags    :: set(flag)
            ).

:- type group
    --->    group(
                uids        :: diet(uid),
                pairing_ids :: list(pairing_id)
            ).

%-----------------------------------------------------------------------------%

propagate_flag_deltas_from_local(Log, _Config, Db, IMAP, MailboxPair, Res, !IO)
        :-
    search_pending_flag_deltas_from_local(Db, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(Pendings),
        % Separate into local messages with and without a corresponding remote
        % UID.
        % XXX ugly
        list.filter(has_uid, Pendings, WithUIDs, WithoutUIDs),
        handle_with_uids(Log, Db, IMAP, MailboxPair, WithUIDs, Res1, !IO),
        (
            Res1 = ok,
            handle_without_uids(Log, Db, WithoutUIDs, Res, !IO)
        ;
            ( Res1 = eof
            ; Res1 = error(_)
            ),
            Res = Res1
        )
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred has_uid(pending_flag_deltas::in) is semidet.

has_uid(Pending) :-
    Pending ^ remote_uid = yes(_).

:- pred get_flag_changes(pending_flag_deltas::in, changes::out,
    flag_deltas(local_mailbox)::out) is det.

get_flag_changes(Pending, Changes, LocalFlags) :-
    Pending = pending_flag_deltas(_PairingId,
        _MaybeUnique, LocalFlags0, _LocalExpunged,
        _MaybeUID, RemoteFlags0, _RemoteExpunged),
    apply_flag_deltas(RemoteFlags0, RemoteFlags, LocalFlags0, LocalFlags),

    Flags0 = RemoteFlags0 ^ cur_set,
    Flags = RemoteFlags ^ cur_set,
    AddFlags = Flags `difference` Flags0,
    RemoveFlags = Flags0 `difference` Flags,
    Changes = changes(AddFlags, RemoveFlags).

%-----------------------------------------------------------------------------%

:- pred handle_with_uids(log::in, database::in, imap::in, mailbox_pair::in,
    list(pending_flag_deltas)::in, maybe_result::out, io::di, io::uo) is det.

handle_with_uids(Log, Db, IMAP, _MailboxPair, Pendings, Res, !IO) :-
    list.foldl(group_changes, Pendings, map.init, Groups),
    map.count(Groups, NumGroups),
    map.foldl3(propagate_flag_deltas_by_group(Log, Db, IMAP, NumGroups),
        Groups, ok, Res, 1, _Count, !IO).

:- pred group_changes(pending_flag_deltas::in, groups::in, groups::out)
    is det.

group_changes(Pending, !Groups) :-
    MaybeUID = Pending ^ remote_uid,
    (
        MaybeUID = yes(UID),
        PairingId = Pending ^ pairing_id,
        get_flag_changes(Pending, Changes, _LocalFlags),
        add_group(Changes, UID, PairingId, !Groups)
    ;
        MaybeUID = no,
        unexpected($module, $pred, "MaybeUID = no")
    ).

:- pred add_group(changes::in, uid::in, pairing_id::in, groups::in, groups::out)
    is det.

add_group(Changes, UID, PairingId, !Groups) :-
    UpdateGroup = (pred(G0::in, G::out) is det :-
        G0 = group(Set0, PairingIds0),
        insert(UID, Set0, Set),
        cons(PairingId, PairingIds0, PairingIds),
        G = group(Set, PairingIds)
    ),
    ( map.transform_value(UpdateGroup, Changes, !Groups) ->
        true
    ;
        Group = group(make_singleton_set(UID), [PairingId]),
        map.det_insert(Changes, Group, !Groups)
    ).

:- pred propagate_flag_deltas_by_group(log::in, database::in, imap::in,
    int::in, changes::in, group::in, maybe_result::in, maybe_result::out,
    int::in, int::out, io::di, io::uo) is det.

propagate_flag_deltas_by_group(Log, Db, IMAP, NumGroups, Changes, Group,
        Res0, Res, Count, Count + 1, !IO) :-
    (
        Res0 = ok,
        propagate_flag_deltas_by_group_2(Log, Db, IMAP, Changes, Group,
            Count, NumGroups, Res, !IO)
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = Res0
    ).

:- pred propagate_flag_deltas_by_group_2(log::in, database::in, imap::in,
    changes::in, group::in, int::in, int::in, maybe_result::out,
    io::di, io::uo) is det.

propagate_flag_deltas_by_group_2(Log, Db, IMAP, Changes, Group,
        Count, NumGroups, Res, !IO) :-
    Group = group(UIDs, PairingIds),

    log_notice(Log,
        format("Applying changes to %s remote messages (%d of %d): %s",
            [s(to_string(count(UIDs))), i(Count), i(NumGroups),
            s(show_changes(Changes))]), !IO),

    ( diet_to_sequence_set(UIDs, SequenceSet) ->
        store_remote_flags_add_rm(Log, IMAP, SequenceSet, Changes, Res0, !IO)
    ;
        unexpected($module, $pred, "empty UIDs")
    ),
    ( Res0 = ok ->
        transaction(
            record_changes_in_database(Db, Changes, PairingIds),
            Db, Res1, !IO),
        (
            Res1 = ok(commit(_ : unit)),
            Res = ok
        ;
            Res1 = ok(rollback(Error)),
            Res = error(Error)
        ;
            Res1 = ok(rollback_exception(Univ)),
            Res = error("exception thrown: " ++ string(Univ))
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        Res = Res0
    ).

:- func show_changes(changes) = string.

show_changes(changes(AddFlags, RemoveFlags)) =
    (
        set.is_empty(AddFlags),
        set.is_empty(RemoveFlags)
    ->
        "(none)"
    ;
        join_list(" ",
            map(show_change("+"), to_sorted_list(AddFlags)) ++
            map(show_change("-"), to_sorted_list(RemoveFlags)))
    ).

:- func show_change(string, flag) = string.

show_change(Prefix, Flag) = Prefix ++ flag_to_imap_syntax(Flag).

:- pred store_remote_flags_add_rm(log::in, imap::in, sequence_set(uid)::in,
    changes::in, maybe_result::out, io::di, io::uo) is det.

store_remote_flags_add_rm(Log, IMAP, UIDs, changes(AddFlags, RemoveFlags), Res,
        !IO) :-
    % Would it be preferable to read back the actual flags from the server?
    store_remote_flags_change(Log, IMAP, UIDs, remove, RemoveFlags, Res0, !IO),
    ( Res0 = ok ->
        store_remote_flags_change(Log, IMAP, UIDs, add, AddFlags, Res, !IO)
    ;
        Res = Res0
    ).

:- pred store_remote_flags_change(log::in, imap::in, sequence_set(uid)::in,
    store_operation::in, set(flag)::in, maybe_result::out, io::di, io::uo)
    is det.

store_remote_flags_change(Log, IMAP, UIDs, Operation, ChangeFlags, Res, !IO) :-
    ( set.is_empty(ChangeFlags) ->
        Res = ok
    ;
        uid_store(IMAP, UIDs, Operation, silent, to_sorted_list(ChangeFlags),
            Res0, !IO),
        (
            Res0 = ok(result(Status, Text, Alerts)),
            report_alerts(Log, Alerts, !IO),
            (
                Status = ok_with_data(_),
                Res = ok
            ;
                ( Status = no
                ; Status = bad
                ; Status = bye
                ; Status = continue
                ),
                Res = error("unexpected response to UID STORE: " ++ Text)
            )
        ;
            Res0 = eof,
            Res = eof
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred record_changes_in_database(database::in, changes::in,
    list(pairing_id)::in, transaction_result(unit, string)::out,
    io::di, io::uo) is det.

record_changes_in_database(Db, Changes, PairingIds, Res, !IO) :-
    (
        PairingIds = [],
        Res = commit(unit)
    ;
        PairingIds = [PairingId | RestPairingIds],
        record_changes_in_database_2(Db, Changes, PairingId, Res0, !IO),
        (
            Res0 = ok,
            record_changes_in_database(Db, Changes, RestPairingIds, Res, !IO)
        ;
            Res0 = error(Error),
            Res = rollback(Error)
        )
    ).

:- pred record_changes_in_database_2(database::in, changes::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

record_changes_in_database_2(Db, changes(AddFlags, RemoveFlags), PairingId,
        Res, !IO) :-
    % Be conservative; look up the row again within this transaction.
    lookup_both_message_flags(Db, PairingId, ResLookup, !IO),
    (
        ResLookup = ok(LocalFlags0 - RemoteFlags0),

        LocalFlags0 = sets(Lcur, Lplus0, Lminus0),
        RemoteFlags0 = sets(Rcur0, Rplus0, Rminus0),

        % We applied +AddFlags -RemoveFlags to remote side.
        Rcur = difference(Rcur0, RemoveFlags) `union` AddFlags,
        Rplus = intersect(Rplus0, Rcur),
        Rminus = difference(Rminus0, Rcur),
        RemoteFlags = sets(Rcur, Rplus, Rminus),

        % AddFlags, RemoveFlags can be removed from the respective
        % sets on the local side.
        Lplus = difference(Lplus0, AddFlags) `difference` Rcur,
        Lminus = difference(Lminus0, RemoveFlags) `intersect` Rcur,
        LocalFlags = sets(Lcur, Lplus, Lminus),

        record_local_flag_deltas_applied_to_remote(Db, PairingId,
            LocalFlags, require_attn(LocalFlags), RemoteFlags, Res, !IO)
    ;
        ResLookup = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred handle_without_uids(log::in, database::in,
    list(pending_flag_deltas)::in, maybe_result::out, io::di, io::uo) is det.

handle_without_uids(Log, Db, Pendings, Res, !IO) :-
    (
        Pendings = [],
        Res = ok
    ;
        Pendings = [H | T],
        handle_without_uid(Log, Db, H, Res0, !IO),
        (
            Res0 = ok,
            handle_without_uids(Log, Db, T, Res, !IO)
        ;
            ( Res0 = eof
            ; Res0 = error(_)
            ),
            Res = Res0
        )
    ).

:- pred handle_without_uid(log::in, database::in, pending_flag_deltas::in,
    maybe_result::out, io::di, io::uo) is det.

handle_without_uid(Log, Db, Pending, Res, !IO) :-
    Pending = pending_flag_deltas(PairingId,
        MaybeUnique, _LocalFlags0, _LocalExpunged,
        _MaybeUID, _RemoteFlags0, RemoteExpunged),

    get_flag_changes(Pending, changes(_AddFlags, RemoveFlags), LocalFlags),

    % If the remote message was previously expunged, but the local message was
    % undeleted then reset the pairing so that the message can be re-uploaded
    % to the the remote mailbox with a new UID.
    (
        MaybeUnique = yes(uniquename(Unique)),
        RemoteExpunged = expunged,
        contains(RemoveFlags, system(deleted))
    ->
        log_notice(Log, "Resurrecting message " ++ Unique, !IO),
        reset_pairing_remote_message(Db, PairingId, Res0, !IO)
    ;
        record_local_flag_deltas_inapplicable_to_remote(Db, PairingId,
            LocalFlags, Res0, !IO)
    ),
    Res = from_maybe_error(Res0).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
