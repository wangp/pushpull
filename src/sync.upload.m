%-----------------------------------------------------------------------------%

:- module sync.upload.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred upload_unpaired_local_messages(prog_config::in, database::in, imap::in,
    mailbox_pair::in, dir_cache::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module time.

:- import_module flag_delta.
:- import_module imap.time.
:- import_module log.
:- import_module maildir.
:- import_module message_file.

:- type file_data
    --->    file_data(
                content :: string,
                modtime :: time_t
            ).

%-----------------------------------------------------------------------------%

upload_unpaired_local_messages(Config, Database, IMAP, MailboxPair, DirCache,
        Res, !IO) :-
    % Currently we download unpaired remote messages first and try to pair them
    % with existing local messages, so the remaining unpaired local messages
    % should actually not have remote counterparts.
    search_unpaired_local_messages(Database, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        upload_messages(Config, Database, IMAP, MailboxPair, DirCache,
            UnpairedLocals, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred upload_messages(prog_config::in, database::in, imap::in,
    mailbox_pair::in, dir_cache::in, list(unpaired_local_message)::in,
    maybe_error::out, io::di, io::uo) is det.

upload_messages(Config, Database, IMAP, MailboxPair, DirCache, UnpairedLocals,
        Res, !IO) :-
    (
        UnpairedLocals = [],
        Res = ok
    ;
        UnpairedLocals = [H | T],
        upload_message(Config, Database, IMAP, MailboxPair, DirCache, H, Res0,
            !IO),
        (
            Res0 = ok,
            upload_messages(Config, Database, IMAP, MailboxPair, DirCache, T,
                Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred upload_message(prog_config::in, database::in, imap::in,
    mailbox_pair::in, dir_cache::in, unpaired_local_message::in,
    maybe_error::out, io::di, io::uo) is det.

upload_message(_Config, Database, IMAP, MailboxPair, DirCache, UnpairedLocal,
        Res, !IO) :-
    LocalMailboxPath = get_local_mailbox_path(MailboxPair),
    UnpairedLocal = unpaired_local_message(PairingId, Unique),
    find_file(DirCache, LocalMailboxPath, Unique, ResFind),
    (
        ResFind = found(Path, _InfoSuffix),
        lookup_local_message_flags(Database, PairingId, ResFlags, !IO),
        (
            ResFlags = ok(LocalFlagDeltas),
            get_file_data(Path, ResFileData, !IO),
            (
                ResFileData = ok(FileData),
                LocalFlags = LocalFlagDeltas ^ cur_set,
                do_upload_message(IMAP, MailboxPair, FileData, LocalFlags,
                    ResUpload, !IO),
                (
                    ResUpload = ok(MaybeUID),
                    (
                        MaybeUID = yes(UID),
                        % XXX is it okay to assume RemoteFlags?
                        RemoteFlags = init_flags(LocalFlags),
                        set_pairing_remote_message(Database, PairingId, UID,
                            RemoteFlags, Res, !IO)
                    ;
                        MaybeUID = no,
                        % We don't know the UID of the appended message.
                        % At the next sync we should notice the new message
                        % and, after downloading it, pair it up with the local
                        % message.
                        Res = ok
                    )
                ;
                    ResUpload = error(Error),
                    Res = error(Error)
                )
            ;
                ResFileData = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            ResFlags = error(Error),
            Res = error(Error)
        )
    ;
        ResFind = found_but_unexpected(Path),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = not_found,
        % Maybe deleted since.
        Res = ok
    ).

:- pred get_file_data(string::in, io.res(file_data)::out, io::di, io::uo)
    is det.

get_file_data(Path, Res, !IO) :-
    io.file_modification_time(Path, ResTime, !IO),
    (
        ResTime = ok(ModTime),
        io.open_input(Path, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            io.read_file_as_string(Stream, ResRead, !IO),
            io.close_input(Stream, !IO),
            (
                ResRead = ok(Content),
                Res = ok(file_data(Content, ModTime))
            ;
                ResRead = error(_, Error),
                Res = error(Error)
            )
        ;
            ResOpen = error(Error),
            Res = error(Error)
        )
    ;
        ResTime = error(Error),
        Res = error(Error)
    ).

:- pred do_upload_message(imap::in, mailbox_pair::in, file_data::in,
    set(flag)::in, maybe_error(maybe(uid))::out, io::di, io::uo) is det.

do_upload_message(IMAP, MailboxPair, FileData, Flags, Res, !IO) :-
    MailboxName = get_remote_mailbox_name(MailboxPair),
    UIDValidity = get_remote_mailbox_uidvalidity(MailboxPair),
    % Try to get an older mod-seq-value prior to APPEND if appending to the
    % selected mailbox.
    get_selected_mailbox_uidvalidity(IMAP, SelectedUIDValidity, !IO),
    get_selected_mailbox_highest_modseqvalue(IMAP, HighestModSeqValue, !IO),
    (
        SelectedUIDValidity = yes(UIDValidity),
        HighestModSeqValue = yes(highestmodseq(ModSeqValue))
    ->
        PriorModSeqValue = yes(ModSeqValue)
    ;
        PriorModSeqValue = no
    ),

    FileData = file_data(ContentLf, ModTime),
    DateTime = make_date_time(ModTime),
    append(IMAP, MailboxName, to_sorted_list(Flags), yes(DateTime), ContentLf,
        result(ResAppend, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResAppend = ok_with_data(MaybeAppendUID),
        io.write_string(Text, !IO),
        io.nl(!IO),
        get_appended_uid(IMAP, UIDValidity, MaybeAppendUID, ContentLf,
            PriorModSeqValue, Res, !IO)
    ;
        ( ResAppend = no
        ; ResAppend = bad
        ; ResAppend = bye
        ; ResAppend = continue
        ; ResAppend = error
        ),
        Res = error("unexpected response to APPEND: " ++ Text)
    ).

:- pred get_appended_uid(imap::in, uidvalidity::in, maybe(appenduid)::in,
    string::in, maybe(mod_seq_value)::in, maybe_error(maybe(uid))::out,
    io::di, io::uo) is det.

get_appended_uid(IMAP, MailboxUIDValidity, MaybeAppendUID, ContentLf,
        PriorModSeqValue, Res, !IO) :-
    % In the best case the server sent an APPENDUID response with the UID.
    % Otherwise we search the mailbox for the Message-Id (if it's unique).
    % Otherwise we give up.
    (
        MaybeAppendUID = yes(appenduid(MailboxUIDValidity, UIDSet)),
        is_singleton_set(UIDSet, UID)
    ->
        Res = ok(yes(UID))
    ;
        get_appended_uid_fallback(IMAP, ContentLf, PriorModSeqValue, Res, !IO)
    ).

:- pred get_appended_uid_fallback(imap::in, string::in,
    maybe(mod_seq_value)::in, maybe_error(maybe(uid))::out, io::di, io::uo)
    is det.

get_appended_uid_fallback(IMAP, ContentLf, PriorModSeqValue, Res, !IO) :-
    read_message_id_from_message_lf(ContentLf, ReadMessageId),
    (
        ReadMessageId = yes(MessageId),
        SearchKey0 = header(make_astring("Message-Id"),
            make_astring(MessageId)),
        % If we know a mod-seq-value prior to the APPEND then we can restrict
        % the search.
        (
            PriorModSeqValue = yes(mod_seq_value(ModSeqValue)),
            SearchKey = and(modseq(mod_seq_valzer(ModSeqValue)), [SearchKey0])
        ;
            PriorModSeqValue = no,
            SearchKey = SearchKey0
        ),
        uid_search(IMAP, SearchKey, no, result(ResSearch, Text, Alerts), !IO),
        report_alerts(Alerts, !IO),
        (
            ResSearch = ok_with_data(uid_search_result(UIDs,
                _HighestModSeqValueOfFound, _ReturnDatas)),
            io.write_string(Text, !IO),
            io.nl(!IO),
            ( UIDs = [UID] ->
                Res = ok(yes(UID))
            ;
                Res = ok(no)
            )
        ;
            ( ResSearch = no
            ; ResSearch = bad
            ; ResSearch = bye
            ; ResSearch = continue
            ; ResSearch = error
            ),
            Res = error("unexpected response to UID SEARCH: " ++ Text)
        )
    ;
        ( ReadMessageId = no
        ; ReadMessageId = format_error(_)
        ; ReadMessageId = error(_)
        ),
        Res = ok(no)
    ).

:- pred is_singleton_set(uid_set::in, uid::out) is semidet.

is_singleton_set(Set, UID) :-
    solutions(
        (pred(X::out) is nondet :- member(uid_range(X, X), Set)),
        [UID]).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
