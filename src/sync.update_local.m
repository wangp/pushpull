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
    LocalMailboxPath = get_local_mailbox_path(MailboxPair),
    update_file_list(LocalMailboxPath, init, ResCache, !IO),
    (
        ResCache = ok(DirCache),
        list_files(DirCache, LocalMailboxPath, NewFiles, CurFiles),

        LocalMailboxPath = local_mailbox_path(DirName),
        update_db_local_message_files(Db, MailboxPair,
            DirName / "new", NewFiles, Res1, !IO),
        (
            Res1 = ok,
            update_db_local_message_files(Db, MailboxPair,
                DirName / "cur", CurFiles, Res2, !IO),
            (
                Res2 = ok,
                mark_expunged_local_messages(Db, MailboxPair,
                    NewFiles, CurFiles, Res3, !IO),
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

:- pred update_db_local_message_files(database::in, mailbox_pair::in,
    string::in, list(string)::in, maybe_error::out, io::di, io::uo) is det.

update_db_local_message_files(_Db, _MailboxPair, _DirName,
        [], ok, !IO).
update_db_local_message_files(Db, MailboxPair, DirName,
        [BaseName | BaseNames], Res, !IO) :-
    update_db_local_message_file(Db, MailboxPair, DirName,
        BaseName, Res0, !IO),
    (
        Res0 = ok,
        update_db_local_message_files(Db, MailboxPair, DirName,
            BaseNames, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred update_db_local_message_file(database::in, mailbox_pair::in,
    string::in, string::in, maybe_error::out, io::di, io::uo) is det.

update_db_local_message_file(Db, MailboxPair, DirName, BaseName, Res, !IO) :-
    ( parse_basename(BaseName, Unique, Flags) ->
        update_db_local_message_file_2(Db, MailboxPair, DirName, BaseName,
            Unique, Flags, Res, !IO)
    ;
        % Should just skip.
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

:- pred update_db_local_message_file_2(database::in, mailbox_pair::in,
    string::in, string::in, uniquename::in, set(flag)::in, maybe_error::out,
    io::di, io::uo) is det.

update_db_local_message_file_2(Db, MailboxPair, DirName, BaseName, Unique,
        Flags, Res, !IO) :-
    search_pairing_by_local_message(Db, MailboxPair, Unique, ResSearch, !IO),
    (
        ResSearch = ok(yes({PairingId, LocalFlagDeltas0})),
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
                message_id(MessageId), Unique, Flags, Res, !IO)
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
        )
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred mark_expunged_local_messages(database::in, mailbox_pair::in,
    list(string)::in, list(string)::in, maybe_error::out, io::di, io::uo)
    is det.

mark_expunged_local_messages(Db, MailboxPair, NewFiles, CurFiles, Res, !IO) :-
    create_detect_local_expunge_temp_table(Db, Res0, !IO),
    (
        Res0 = ok,
        insert_into_detect_local_expunge_table(Db, NewFiles, Res1, !IO),
        (
            Res1 = ok,
            insert_into_detect_local_expunge_table(Db, CurFiles, Res2, !IO),
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
            drop_detect_local_expunge_temp_table(Db, Res, !IO)
        ;
            Res3 = error(Error),
            Res = error(Error),
            drop_detect_local_expunge_temp_table(Db, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred insert_into_detect_local_expunge_table(database::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

insert_into_detect_local_expunge_table(_Db, [], ok, !IO).
insert_into_detect_local_expunge_table(Db, [BaseName | BaseNames], Res, !IO) :-
    ( parse_basename(BaseName, Unique, _Flags) ->
        insert_into_detect_local_expunge_table(Db, Unique, Res0, !IO),
        (
            Res0 = ok,
            insert_into_detect_local_expunge_table(Db, BaseNames, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = error("cannot parse message filename: " ++ BaseName)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
