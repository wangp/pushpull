%-----------------------------------------------------------------------------%

:- module sync.update_local.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred update_db_local_mailbox(database::in, mailbox_pair::in,
    maybe_error(dir_cache)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module list.
:- import_module set.
:- import_module string.

:- import_module flag_delta.
:- import_module maildir.
:- import_module message_file.

%-----------------------------------------------------------------------------%

update_db_local_mailbox(Db, MailboxPair, Res, !IO) :-
    prepare_search_pairing_by_local_message(Db, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        update_db_local_mailbox_2(Db, Stmt, MailboxPair, Res, !IO),
        finalize_search_pairing_by_local_message(Stmt, !IO)
    ;
        ResStmt = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_mailbox_2(database::in,
    search_pairing_by_local_message_stmt::in, mailbox_pair::in,
    maybe_error(dir_cache)::out, io::di, io::uo) is det.

update_db_local_mailbox_2(Db, Stmt, MailboxPair, Res, !IO) :-
    LocalMailboxPath = get_local_mailbox_path(MailboxPair),
    update_file_list(LocalMailboxPath, init, ResCache, !IO),
    (
        ResCache = ok(DirCache),
        list_files(DirCache, LocalMailboxPath, NewFiles, CurFiles),

        LocalMailboxPath = local_mailbox_path(DirName),
        update_db_local_message_files(Db, Stmt, MailboxPair,
            DirName / "new", NewFiles, Res1, [], PairingIds1, !IO),
        (
            Res1 = ok,
            update_db_local_message_files(Db, Stmt, MailboxPair,
                DirName / "cur", CurFiles, Res2, [], PairingIds2, !IO),
            (
                Res2 = ok,
                mark_expunged_local_messages(Db, MailboxPair, PairingIds1,
                    PairingIds2, Res3, !IO),
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
        ResCache = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_files(database::in,
    search_pairing_by_local_message_stmt::in, mailbox_pair::in, string::in,
    list(string)::in, maybe_error::out,
    list(pairing_id)::in, list(pairing_id)::out, io::di, io::uo) is det.

update_db_local_message_files(_Db, _Stmt, _MailboxPair, _DirName,
        [], ok, !PairingIds, !IO).
update_db_local_message_files(Db, Stmt, MailboxPair, DirName,
        [BaseName | BaseNames], Res, !PairingIds, !IO) :-
    update_db_local_message_file(Db, Stmt, MailboxPair, DirName,
        BaseName, Res0, !PairingIds, !IO),
    (
        Res0 = ok,
        update_db_local_message_files(Db, Stmt, MailboxPair, DirName,
            BaseNames, Res, !PairingIds, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_file(database::in,
    search_pairing_by_local_message_stmt::in, mailbox_pair::in, string::in,
    string::in, maybe_error::out, list(pairing_id)::in, list(pairing_id)::out,
    io::di, io::uo) is det.

update_db_local_message_file(Db, Stmt, MailboxPair, DirName, BaseName, Res,
        !PairingIds, !IO) :-
    ( parse_basename(BaseName, Unique, Flags) ->
        update_db_local_message_file_2(Db, Stmt, MailboxPair, DirName,
            BaseName, Unique, Flags, Res, !PairingIds, !IO)
    ;
        % Should just skip.
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

:- pred update_db_local_message_file_2(database::in,
    search_pairing_by_local_message_stmt::in,
    mailbox_pair::in, string::in, string::in, uniquename::in, set(flag)::in,
    maybe_error::out, list(pairing_id)::in, list(pairing_id)::out,
    io::di, io::uo) is det.

update_db_local_message_file_2(Db, Stmt, MailboxPair, DirName, BaseName,
        Unique, Flags, Res, !PairingIds, !IO) :-
    search_pairing_by_local_message(Db, Stmt, MailboxPair, Unique,
        ResSearch, !IO),
    (
        ResSearch = ok(yes({PairingId, LocalFlagDeltas0})),
        cons(PairingId, !PairingIds),
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
        ResSearch = ok(no),
        update_db_local_message_file_3(Db, Stmt, MailboxPair, DirName,
            BaseName, Unique, Flags, Res, !PairingIds, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_file_3(database::in,
    search_pairing_by_local_message_stmt::in,
    mailbox_pair::in, string::in, string::in, uniquename::in, set(flag)::in,
    maybe_error::out, list(pairing_id)::in, list(pairing_id)::out,
    io::di, io::uo) is det.

update_db_local_message_file_3(Db, Stmt, MailboxPair, DirName, BaseName,
        Unique, Flags, Res, !PairingIds, !IO) :-
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
            % Search for it again.
            search_pairing_by_local_message(Db, Stmt, MailboxPair, Unique,
                ResSearch, !IO),
            (
                ResSearch = ok(yes({PairingId, _})),
                cons(PairingId, !PairingIds),
                Res = ok
            ;
                ResSearch = ok(no),
                Res = error("cannot find inserted message")
            ;
                ResSearch = error(Error),
                Res = error(Error)
            )
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
    list(pairing_id)::in, list(pairing_id)::in, maybe_error::out,
    io::di, io::uo) is det.

mark_expunged_local_messages(Db, MailboxPair, NewFiles, CurFiles, Res, !IO) :-
    begin_detect_expunge(Db, Res0, !IO),
    (
        Res0 = ok(InsertStmt),
        insert(Db, InsertStmt, NewFiles, Res1, !IO),
        (
            Res1 = ok,
            insert(Db, InsertStmt, CurFiles, Res2, !IO),
            (
                Res2 = ok,
                mark_expunged_local_messages(Db, MailboxPair, Res3, !IO),
                (
                    Res3 = ok(Count),
                    io.format("Detected %d expunged local messages.\n",
                        [i(Count)], !IO)
                ;
                    Res3 = error(_)
                )
            ;
                Res2 = error(Error2),
                Res3 = error(Error2)
            )
        ;
            Res1 = error(Error1),
            Res3 = error(Error1)
        ),
        (
            Res3 = ok(_),
            end_detect_expunge(Db, InsertStmt, Res, !IO)
        ;
            Res3 = error(Error),
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
