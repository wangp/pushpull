%-----------------------------------------------------------------------------%

:- module sync.update_local.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred update_db_local_mailbox(prog_config::in, database::in, inotify(S)::in,
    mailbox_pair::in, update_method::in, maybe_error::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module set_tree234.
:- import_module string.

:- import_module flag_delta.
:- import_module maildir.
:- import_module message_file.
:- import_module path.

    % diet might be good.
:- type pairing_id_set == set_tree234(pairing_id).

%-----------------------------------------------------------------------------%

update_db_local_mailbox(_Config, Db, Inotify, MailboxPair, UpdateMethod, Res,
        !DirCache, !IO) :-
    update_dir_cache(Inotify, UpdateMethod, ResCache, !DirCache, !IO),
    (
        ResCache = ok(yes),
        get_unexpunged_pairings_by_uniquename(Db, MailboxPair,
            ResExistingPairings, !IO),
        (
            ResExistingPairings = ok(ExistingPairings),
            make_pairing_id_set(ExistingPairings, UnseenPairingIds0),
            begin(!.DirCache, Index0),
            update_db_local_message_files(Db, MailboxPair, ExistingPairings,
                !.DirCache, Index0, _Index, ResUpdate, UnseenPairingIds0,
                UnseenPairingIds, !IO),
            (
                ResUpdate = ok,
                mark_expunged_local_messages(Db, MailboxPair, UnseenPairingIds,
                    Res, !IO)
            ;
                ResUpdate = error(Error),
                Res = error(Error)
            )
        ;
            ResExistingPairings = error(Error),
            Res = error(Error)
        )
    ;
        ResCache = ok(no),
        Res = ok
    ;
        ResCache = error(Error),
        Res = error(Error)
    ).

:- pred make_pairing_id_set(map(uniquename, pairing_flag_deltas)::in,
    pairing_id_set::out) is det.

make_pairing_id_set(Map, Set) :-
    map.foldl_values(make_pairing_id_set, Map, init, Set).

:- pred make_pairing_id_set(pairing_flag_deltas::in,
    pairing_id_set::in, pairing_id_set::out) is det.

make_pairing_id_set(pairing_flag_deltas(PairingId, _), !Set) :-
    insert(PairingId, !Set).

:- pred update_db_local_message_files(database::in, mailbox_pair::in,
    map(uniquename, pairing_flag_deltas)::in, dir_cache::in,
    dir_cache.index::in, dir_cache.index::out, maybe_error::out,
    pairing_id_set::in, pairing_id_set::out, io::di, io::uo) is det.

update_db_local_message_files(Db, MailboxPair, ExistingPairings,
        DirCache, !Index, Res, !UnseenPairingIds, !IO) :-
    ( get(DirCache, !Index, File) ->
        update_db_local_message_file(Db, MailboxPair, ExistingPairings, File,
            Res0, !UnseenPairingIds, !IO),
        (
            Res0 = ok,
            update_db_local_message_files(Db, MailboxPair, ExistingPairings,
                DirCache, !Index, Res, !UnseenPairingIds, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = ok
    ).

:- pred update_db_local_message_file(database::in, mailbox_pair::in,
    map(uniquename, pairing_flag_deltas)::in, dir_cache.file::in,
    maybe_error::out, pairing_id_set::in, pairing_id_set::out,
    io::di, io::uo) is det.

update_db_local_message_file(Db, MailboxPair, ExistingPairings,
        file(BaseName, DirName), Res, !UnseenPairingIds, !IO) :-
    ( parse_basename(BaseName, Unique, Flags) ->
        update_db_local_message_file_2(Db, MailboxPair, ExistingPairings,
            DirName, BaseName, Unique, Flags, Res, !UnseenPairingIds, !IO)
    ;
        % Should just skip.
        Res = error("cannot parse message filename: " ++ BaseName ^ bn)
    ).

:- pred update_db_local_message_file_2(database::in, mailbox_pair::in,
    map(uniquename, pairing_flag_deltas)::in, dirname::in, basename::in,
    uniquename::in, set(flag)::in, maybe_error::out,
    pairing_id_set::in, pairing_id_set::out, io::di, io::uo) is det.

update_db_local_message_file_2(Db, MailboxPair, ExistingPairings,
        DirName, BaseName, Unique, Flags, Res, !UnseenPairingIds, !IO) :-
    (
        map.search(ExistingPairings, Unique,
            pairing_flag_deltas(PairingId, LocalFlagDeltas0))
    ->
        set_tree234.delete(PairingId, !UnseenPairingIds),
        update_maildir_standard_flags(Flags, LocalFlagDeltas0, LocalFlagDeltas,
            IsChanged),
        (
            IsChanged = yes,
            Unique = uniquename(UniqueString),
            io.format("Updating local message flags %s: %s\n",
                [s(UniqueString), s(string(LocalFlagDeltas))], !IO),
            update_local_message_flags(Db, PairingId, LocalFlagDeltas,
                require_attn(LocalFlagDeltas), Res, !IO)
        ;
            IsChanged = no,
            Res = ok
        )
    ;
        update_db_local_message_file_3(Db, MailboxPair, DirName, BaseName,
            Unique, Flags, Res, !IO)
    ).

:- pred update_db_local_message_file_3(database::in,
    mailbox_pair::in, dirname::in, basename::in, uniquename::in, set(flag)::in,
    maybe_error::out, io::di, io::uo) is det.

update_db_local_message_file_3(Db, MailboxPair, DirName, BaseName, Unique,
        Flags, Res, !IO) :-
    DirName / BaseName = path(Path),
    read_message_id_from_file(Path, ResRead, !IO),
    (
        ResRead = yes(MessageId),
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
        io.format("skipping %s: missing Message-Id\n", [s(BaseName ^ bn)],
            !IO),
        Res = ok
    ;
        ResRead = format_error(Error),
        io.format("skipping %s: %s\n", [s(BaseName ^ bn), s(Error)], !IO),
        Res = ok
    ;
        ResRead = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred mark_expunged_local_messages(database::in, mailbox_pair::in,
    pairing_id_set::in, maybe_error::out, io::di, io::uo) is det.

mark_expunged_local_messages(Db, MailboxPair, UnseenPairingIds, Res, !IO) :-
    begin_detect_expunge(Db, Res0, !IO),
    (
        Res0 = ok(InsertStmt),
        insert(Db, InsertStmt, to_sorted_list(UnseenPairingIds), Res1, !IO),
        (
            Res1 = ok,
            mark_expunged_local_messages(Db, MailboxPair, Res2, !IO),
            (
                Res2 = ok(Count),
                io.format("Detected %d expunged local messages.\n",
                    [i(Count)], !IO)
            ;
                Res2 = error(_)
            )
        ;
            Res1 = error(Error1),
            Res2 = error(Error1)
        ),
        (
            Res2 = ok(_),
            end_detect_expunge(Db, InsertStmt, Res, !IO)
        ;
            Res2 = error(Error),
            Res = error(Error),
            end_detect_expunge(Db, InsertStmt, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
