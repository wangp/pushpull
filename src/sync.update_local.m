%-----------------------------------------------------------------------------%

:- module sync.update_local.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred update_db_local_mailbox(database::in, mailbox_pair::in,
    dir_cache::in, maybe_error(dir_cache)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module set_tree234.
:- import_module string.

:- import_module flag_delta.
:- import_module maildir.
:- import_module message_file.

    % diet might be good.
:- type pairing_id_set == set_tree234(pairing_id).

%-----------------------------------------------------------------------------%

update_db_local_mailbox(Db, MailboxPair, DirCache0, Res, !IO) :-
    LocalMailboxPath = get_local_mailbox_path(MailboxPair),
    update_file_list(LocalMailboxPath, DirCache0, ResCache, !IO),
    (
        ResCache = ok(DirCache),
        list_files(DirCache, LocalMailboxPath, NewFiles, CurFiles),

        get_unexpunged_pairings_by_uniquename(Db, MailboxPair,
            ResExistingPairings, !IO),
        (
            ResExistingPairings = ok(ExistingPairings),
            make_pairing_id_set(ExistingPairings, UnseenPairingIds0),

            LocalMailboxPath = local_mailbox_path(DirName),
            update_db_local_message_files(Db, MailboxPair, ExistingPairings,
                DirName / "new", NewFiles, Res1,
                UnseenPairingIds0, UnseenPairingIds1, !IO),
            (
                Res1 = ok,
                update_db_local_message_files(Db, MailboxPair,
                    ExistingPairings, DirName / "cur", CurFiles, Res2,
                    UnseenPairingIds1, UnseenPairingIds, !IO),
                (
                    Res2 = ok,
                    mark_expunged_local_messages(Db, MailboxPair,
                        UnseenPairingIds, Res3, !IO),
                    (
                        Res3 = ok,
                        Res = ok(DirCache)
                    ;
                        Res3 = error(Error),
                        Res = error(Error)
                    )
                ;
                    Res2 = error(Error),
                    Res = error(Error)
                )
            ;
                Res1 = error(Error),
                Res = error(Error)
            )
        ;
            ResExistingPairings = error(Error),
            Res = error(Error)
        )
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
    map(uniquename, pairing_flag_deltas)::in, string::in, list(string)::in,
    maybe_error::out, pairing_id_set::in, pairing_id_set::out, io::di, io::uo)
    is det.

update_db_local_message_files(_Db, _MailboxPair, _ExistingPairings, _DirName,
        [], ok, !UnseenPairingIds, !IO).
update_db_local_message_files(Db, MailboxPair, ExistingPairings, DirName,
        [BaseName | BaseNames], Res, !UnseenPairingIds, !IO) :-
    update_db_local_message_file(Db, MailboxPair, ExistingPairings,
        DirName, BaseName, Res0, !UnseenPairingIds, !IO),
    (
        Res0 = ok,
        update_db_local_message_files(Db, MailboxPair, ExistingPairings,
            DirName, BaseNames, Res, !UnseenPairingIds, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_file(database::in, mailbox_pair::in,
    map(uniquename, pairing_flag_deltas)::in, string::in, string::in,
    maybe_error::out, pairing_id_set::in, pairing_id_set::out,
    io::di, io::uo) is det.

update_db_local_message_file(Db, MailboxPair, ExistingPairings,
        DirName, BaseName, Res, !UnseenPairingIds, !IO) :-
    ( parse_basename(BaseName, Unique, Flags) ->
        update_db_local_message_file_2(Db, MailboxPair, ExistingPairings,
            DirName, BaseName, Unique, Flags, Res, !UnseenPairingIds, !IO)
    ;
        % Should just skip.
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

:- pred update_db_local_message_file_2(database::in, mailbox_pair::in,
    map(uniquename, pairing_flag_deltas)::in, string::in, string::in,
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
    mailbox_pair::in, string::in, string::in, uniquename::in, set(flag)::in,
    maybe_error::out, io::di, io::uo) is det.

update_db_local_message_file_3(Db, MailboxPair, DirName, BaseName, Unique,
        Flags, Res, !IO) :-
    Path = DirName / BaseName,
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
        io.format("skipping %s: missing Message-Id\n", [s(BaseName)], !IO),
        Res = ok
    ;
        ResRead = format_error(Error),
        io.format("skipping %s: %s\n", [s(BaseName), s(Error)], !IO),
        Res = ok
    ;
        ResRead = error(Error),
        Res = error(io.error_message(Error))
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
