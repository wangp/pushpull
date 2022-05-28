% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module sync.update_local.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module dir_cache.

:- pred update_db_local_mailbox(log::in, prog_config::in, database::in,
    inotify(S)::in, mailbox_pair::in, update_method::in, maybe_error::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module set.
:- import_module set_tree234.
:- import_module string.

:- import_module flag_delta.
:- import_module maildir.
:- import_module message_file.
:- import_module path.

:- type change
    --->    change(pairing_id, flag_deltas(local_mailbox)).

    % diet might be good.
:- type pairing_id_set == set_tree234(pairing_id).

%-----------------------------------------------------------------------------%

update_db_local_mailbox(Log, Config, Db, Inotify, MailboxPair, UpdateMethod,
        Res, !DirCache, !IO) :-
    % Only need to watch directories if we will IDLE.
    AddNewWatches = Config ^ idle,
    update_dir_cache(Log, Inotify, UpdateMethod, AddNewWatches, ResCache,
        !DirCache, !IO),
    (
        ResCache = ok(yes),
        log_debug(Log, "Detecting local mailbox changes", !IO),
        % Avoid loading the entire database query result in memory.  The
        % directory cache is already in memory so we try to share with that.
        ( all_files(!.DirCache, AllFiles) ->
            fold_unexpunged_pairings_with_uniquename(detect_local_changes(Log),
                Db, MailboxPair, ResFold,
                [], Changes, init, UnseenPairingIds,
                AllFiles, NewFiles, !IO),
            (
                ResFold = ok,
                % If many previously known messages are missing from the local
                % mailbox then something may have gone wrong.
                NumUnseenPairingIds = set_tree234.count(UnseenPairingIds),
                check_mass_deletion(Log, Config, NumUnseenPairingIds, Res0, !IO),
                (
                    Res0 = ok,
                    log_debug(Log, "Update database local message state", !IO),
                    update_pairings(Db, Changes, ResUpdate, !IO),
                    (
                        ResUpdate = ok,
                        insert_pairings(Log, Db, MailboxPair, NewFiles, ResInsert,
                            !IO),
                        (
                            ResInsert = ok,
                            mark_expunged_local_messages(Log, Db, MailboxPair,
                               UnseenPairingIds, Res, !IO)
                        ;
                            ResInsert = error(Error),
                            Res = error(Error)
                        )
                    ;
                        ResUpdate = error(Error),
                        Res = error(Error)
                    )
                ;
                    Res0 = error(Error),
                    Res = error(Error)
                )
            ;
                ResFold = error(Error),
                Res = error(Error)
            )
        ;
            Res = error("non-unique file name")
        )
    ;
        ResCache = ok(no),
        Res = ok
    ;
        ResCache = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred detect_local_changes(log::in,
    pairing_id::in, uniquename::in, flag_deltas(local_mailbox)::in,
    list(change)::in, list(change)::out,
    pairing_id_set::in, pairing_id_set::out,
    files::array_di, files::array_uo, io::di, io::uo) is det.

detect_local_changes(Log, PairingId, Unique, LocalFlagDeltas0,
        !Changes, !UnseenPairingIds, !Files, !IO) :-
    ( remove_uniquename(Unique, BaseName, !Files) ->
        % Unique name exists found in directory.
        ( parse_basename(BaseName, Unique, MaildirStandardFlags) ->
            update_maildir_standard_flags(MaildirStandardFlags,
                LocalFlagDeltas0, LocalFlagDeltas, IsChanged),
            (
                IsChanged = yes,
                cons(change(PairingId, LocalFlagDeltas), !Changes)
            ;
                IsChanged = no
            )
        ;
            log_warning(Log,
                "cannot parse message filename: " ++ BaseName ^ bn, !IO)
        )
    ;
        % No longer in directory.
        insert(PairingId, !UnseenPairingIds)
    ).

%-----------------------------------------------------------------------------%

:- pred check_mass_deletion(log::in, prog_config::in, int::in,
    maybe_error::out, io::di, io::uo) is det.

check_mass_deletion(Log, Config, NumUnseenPairingIds, Res, !IO) :-
    AllowMassDelete = Config ^ allow_mass_delete,
    (
        AllowMassDelete = yes(MaxDeletes)
    ;
        AllowMassDelete = no,
        MaxDeletes = 50
    ),
    ( NumUnseenPairingIds =< MaxDeletes ->
        Res = ok
    ;
        log_warning(Log,
            "Detected " ++ from_int(NumUnseenPairingIds) ++
            " deleted files in local mailbox", !IO),
        Res = error("Too many files deleted in local mailbox")
    ).

%-----------------------------------------------------------------------------%

    % Could use a transaction around this.
:- pred update_pairings(database::in, list(change)::in, maybe_error::out,
    io::di, io::uo) is det.

update_pairings(_Db, [], ok, !IO).
update_pairings(Db, [Change | Changes], Res, !IO) :-
    update_pairing(Db, Change, Res0, !IO),
    (
        Res0 = ok,
        update_pairings(Db, Changes, Res, !IO)
    ;
        Res0 = error(_),
        Res = Res0
    ).

:- pred update_pairing(database::in, change::in, maybe_error::out,
    io::di, io::uo) is det.

update_pairing(Db, Change, Res, !IO) :-
    Change = change(PairingId, LocalFlagDeltas),
    update_local_message_flags(Db, PairingId, LocalFlagDeltas,
        require_attn(LocalFlagDeltas), Res, !IO).

%-----------------------------------------------------------------------------%

    % Could use a transaction around this.
:- pred insert_pairings(log::in, database::in, mailbox_pair::in,
    files::in, maybe_error::out, io::di, io::uo) is det.

insert_pairings(Log, Db, MailboxPair, NewFiles, Res, !IO) :-
    foldl2(insert_pairing(Log, Db, MailboxPair), NewFiles, ok, Res, !IO).

:- pred insert_pairing(log::in, database::in, mailbox_pair::in,
    basename::in, dirname::in, maybe_error::in, maybe_error::out,
    io::di, io::uo) is det.

insert_pairing(Log, Db, MailboxPair, BaseName, DirName, Res0, Res, !IO) :-
    (
        Res0 = ok,
        insert_pairing_2(Log, Db, MailboxPair, BaseName, DirName, Res, !IO)
    ;
        Res0 = error(_),
        Res = Res0
    ).

:- pred insert_pairing_2(log::in, database::in, mailbox_pair::in,
    basename::in, dirname::in, maybe_error::out, io::di, io::uo) is det.

insert_pairing_2(Log, Db, MailboxPair, BaseName, DirName, Res, !IO) :-
    ( parse_basename(BaseName, Unique, Flags) ->
        insert_pairing_3(Log, Db, MailboxPair, DirName, BaseName, Unique,
            Flags, Res, !IO)
    ;
        Res = error("cannot parse message filename: " ++ BaseName ^ bn)
    ).

:- pred insert_pairing_3(log::in, database::in, mailbox_pair::in, dirname::in,
    basename::in, uniquename::in, set(flag)::in, maybe_error::out, io::di,
    io::uo) is det.

insert_pairing_3(Log, Db, MailboxPair, DirName, BaseName, Unique, Flags, Res,
        !IO) :-
    DirName / BaseName = path(Path),
    read_message_id_from_file(Path, ResRead, !IO),
    (
        ResRead = yes(message_id(MessageId)),
        % There may already be an unpaired remote message in the database
        % for this local message.  We will download the remote message to
        % compare, as we don't want to assume that Message-Ids are unique.
        % At that time we will notice that the local and remote message
        % contents match, then delete one of the two pairings (this one) in
        % favour of the other.
        insert_new_pairing_only_local_message(Db, MailboxPair,
            message_id(MessageId), Unique, Flags, ResInsert, !IO),
        (
            ResInsert = ok,
            Res = ok
        ;
            ResInsert = error(Error),
            Res = error(Error)
        )
    ;
        ResRead = no,
        % XXX we could add this file to the database, but we want to have
        % more indicators that it really is a message file
        log_warning(Log,
            format("skipping %s: missing Message-Id\n", [s(BaseName ^ bn)]),
                !IO),
        Res = ok
    ;
        ResRead = format_error(Error),
        log_warning(Log,
            format("skipping %s: %s\n", [s(BaseName ^ bn), s(Error)]), !IO),
        Res = ok
    ;
        ResRead = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred mark_expunged_local_messages(log::in, database::in, mailbox_pair::in,
    pairing_id_set::in, maybe_error::out, io::di, io::uo) is det.

mark_expunged_local_messages(Log, Db, MailboxPair, UnseenPairingIds, Res, !IO)
        :-
    begin_detect_expunge(Db, Res0, !IO),
    (
        Res0 = ok(InsertStmt),
        transaction(mark_expunged_local_messages_2(Log, Db, MailboxPair,
            UnseenPairingIds, InsertStmt), Db, Res1, !IO),
        (
            Res1 = ok(commit(Count)),
            ( Count = 0 ->
                Level = debug
            ;
                Level = info
            ),
            log(Log, Level,
                format("Detected %d expunged local messages.\n", [i(Count)]),
                !IO),
            end_detect_expunge(Db, InsertStmt, Res, !IO)
        ;
            (
                Res1 = ok(rollback(Error))
            ;
                Res1 = ok(rollback_exception(Univ)),
                Error = "exception thrown: " ++ string(Univ)
            ;
                Res1 = error(Error)
            ),
            Res = error(Error),
            end_detect_expunge(Db, InsertStmt, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred mark_expunged_local_messages_2(log::in, database::in, mailbox_pair::in,
    pairing_id_set::in, insert_into_detect_expunge_stmt::in,
    transaction_result(int, string)::out, io::di, io::uo) is det.

mark_expunged_local_messages_2(Log, Db, MailboxPair, UnseenPairingIds,
        InsertStmt, Res, !IO) :-
    insert(Db, InsertStmt, to_sorted_list(UnseenPairingIds), Res1, !IO),
    (
        Res1 = ok,
        % For each expunged message, set the local_expunged column and
        % add the \Deleted flag.
        fold_expunged_local_messages(mark_expunged_local_message(Log, Db),
            Db, MailboxPair, Res2, 0, Count, !IO),
        (
            Res2 = ok : maybe_error,
            Res = commit(Count)
        ;
            Res2 = error(Error),
            Res = rollback(Error)
        )
    ;
        Res1 = error(Error),
        Res = rollback(Error)
    ).

:- pred insert(database::in, insert_into_detect_expunge_stmt::in,
    list(pairing_id)::in, maybe_error::out, io::di, io::uo) is det.

insert(_Db, _InsertStmt, [], ok, !IO).
insert(Db, InsertStmt, [PairingId | PairingIds], Res, !IO) :-
    detect_expunge_insert_pairing_id(Db, InsertStmt, PairingId, Res0, !IO),
    (
        Res0 = ok,
        insert(Db, InsertStmt, PairingIds, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred mark_expunged_local_message(log::in, database::in, pairing_id::in,
    flag_deltas(local_mailbox)::in, maybe_error::out, int::in, int::out,
    io::di, io::uo) is det.

mark_expunged_local_message(_Log, Db, PairingId, LocalFlagDeltas0,
        Res, Count, Count + 1, !IO) :-
    add_deleted_flag(LocalFlagDeltas0, LocalFlagDeltas),
    set_local_message_expunged(Db, PairingId, LocalFlagDeltas,
        require_attn(LocalFlagDeltas), Res, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
