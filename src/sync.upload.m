% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module sync.upload.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred upload_unpaired_local_messages(log::in, prog_config::in, database::in,
    imap::in, mailbox_pair::in, dir_cache::in, maybe_result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module time.

:- import_module binary_string.
:- import_module flag_delta.
:- import_module imap.date_time.
:- import_module log.
:- import_module log_help.
:- import_module maildir.
:- import_module maybe_result.
:- import_module message_file.
:- import_module path.

:- type file_data
    --->    file_data(
                content :: message(lf), % assumed
                modtime :: time_t
            ).

%-----------------------------------------------------------------------------%

upload_unpaired_local_messages(Log, _Config, Database, IMAP, MailboxPair,
        DirCache, Res, !IO) :-
    % Currently we download unpaired remote messages first and try to pair them
    % with existing local messages, so the remaining unpaired local messages
    % should actually not have remote counterparts.
    search_unpaired_local_messages(Database, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        list.length(UnpairedLocals, Total),
        upload_messages(Log, Database, IMAP, MailboxPair, DirCache,
            UnpairedLocals, 1, Total, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred upload_messages(log::in, database::in, imap::in, mailbox_pair::in,
    dir_cache::in, list(unpaired_local_message)::in, int::in, int::in,
    maybe_result::out, io::di, io::uo) is det.

upload_messages(Log, Database, IMAP, MailboxPair, DirCache, UnpairedLocals,
        Count0, Total, Res, !IO) :-
    (
        UnpairedLocals = [],
        Res = ok
    ;
        UnpairedLocals = [H | T],
        upload_message(Log, Database, IMAP, MailboxPair, DirCache, H,
            Count0, Total, Res0, !IO),
        ( Res0 = ok ->
            upload_messages(Log, Database, IMAP, MailboxPair, DirCache, T,
                Count0 + 1, Total, Res, !IO)
        ;
            Res = Res0
        )
    ).

:- pred upload_message(log::in, database::in, imap::in, mailbox_pair::in,
    dir_cache::in, unpaired_local_message::in, int::in, int::in,
    maybe_result::out, io::di, io::uo) is det.

upload_message(Log, Database, IMAP, MailboxPair, DirCache, UnpairedLocal,
        Count, Total, Res, !IO) :-
    UnpairedLocal = unpaired_local_message(PairingId, Unique),
    find_file(DirCache, Unique, ResFind),
    (
        ResFind = found(DirName, BaseName, _InfoSuffix),
        Path = DirName / BaseName,
        log_notice(Log,
            format("Uploading (%d of %d) %s\n",
                [i(Count), i(Total), s(Path ^ path)]), !IO),
        lookup_local_message_flags(Database, PairingId, ResFlags, !IO),
        (
            ResFlags = ok(LocalFlagDeltas),
            get_file_data(Path, ResFileData, !IO),
            (
                ResFileData = ok(FileData),
                get_selected_mailbox_highest_modseqvalue(IMAP,
                    MaybePrevHighestModSeq, !IO),
                LocalFlags = LocalFlagDeltas ^ cur_set,
                do_upload_message(Log, IMAP, MailboxPair, FileData, LocalFlags,
                    ResUpload, !IO),
                (
                    ResUpload = ok(MaybeUID),
                    (
                        MaybeUID = yes(UID),
                        % XXX is it okay to assume RemoteFlags?
                        RemoteFlags = init_flags(LocalFlags),
                        % We can say that the pairing is up-to-date at least up
                        % to the mod-seq-value before the message was added.
                        (
                            MaybePrevHighestModSeq =
                                yes(highestmodseq(ModSeqValue))
                        ;
                            ( MaybePrevHighestModSeq = yes(unknown)
                            ; MaybePrevHighestModSeq = yes(nomodseq)
                            ; MaybePrevHighestModSeq = no
                            ),
                            % Should not happen.
                            ModSeqValue = mod_seq_value(one)
                        ),
                        set_pairing_remote_message(Database, PairingId, UID,
                            RemoteFlags, ModSeqValue, ResSet, !IO),
                        Res = from_maybe_error(ResSet)
                    ;
                        MaybeUID = no,
                        % We don't know the UID of the appended message.
                        % At the next sync we should notice the new message
                        % and, after downloading it, pair it up with the local
                        % message.
                        Res = ok
                    )
                ;
                    ResUpload = eof,
                    Res = eof
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
        ResFind = found_but_unexpected(path(Path)),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = not_found,
        % Maybe deleted since.
        Res = ok
    ).

:- pred get_file_data(path::in, io.res(file_data)::out, io::di, io::uo) is det.

get_file_data(path(Path), Res, !IO) :-
    io.file_modification_time(Path, ResTime, !IO),
    (
        ResTime = ok(ModTime),
        io.open_binary_input(Path, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            io.read_binary_file_as_bitmap(Stream, ResRead, !IO),
            io.close_binary_input(Stream, !IO),
            (
                ResRead = ok(Bitmap),
                Content = message(from_bitmap(Bitmap)),
                Res = ok(file_data(Content, ModTime))
            ;
                ResRead = error(Error),
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

:- pred do_upload_message(log::in, imap::in, mailbox_pair::in, file_data::in,
    set(flag)::in, maybe_result(maybe(uid))::out, io::di, io::uo) is det.

do_upload_message(Log, IMAP, MailboxPair, FileData, Flags, Res, !IO) :-
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

    FileData = file_data(Message @ message(ContentLf), ModTime),
    local_date_time(ModTime, DateTime, !IO),
    append(IMAP, MailboxName, to_sorted_list(Flags), yes(DateTime), ContentLf,
        Res0, !IO),
    (
        Res0 = ok(result(Status, Text, Alerts)),
        report_alerts(Log, Alerts, !IO),
        (
            Status = ok_with_data(MaybeAppendUID),
            log_debug(Log, Text, !IO),
            get_appended_uid(Log, IMAP, UIDValidity, MaybeAppendUID, Message,
                PriorModSeqValue, Res, !IO)
        ;
            ( Status = no
            ; Status = bad
            ; Status = bye
            ; Status = continue
            ),
            Res = error("unexpected response to APPEND: " ++ Text)
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred get_appended_uid(log::in, imap::in, uidvalidity::in,
    maybe(appenduid)::in, message(lf)::in, maybe(mod_seq_value)::in,
    maybe_result(maybe(uid))::out, io::di, io::uo) is det.

get_appended_uid(Log, IMAP, MailboxUIDValidity, MaybeAppendUID, Message,
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
        get_appended_uid_fallback(Log, IMAP, Message, PriorModSeqValue, Res,
            !IO)
    ).

:- pred get_appended_uid_fallback(log::in, imap::in, message(lf)::in,
    maybe(mod_seq_value)::in, maybe_result(maybe(uid))::out, io::di, io::uo)
    is det.

get_appended_uid_fallback(Log, IMAP, Message, PriorModSeqValue, Res, !IO) :-
    read_message_id_from_message_lf(Message, ReadMessageId),
    (
        ReadMessageId = yes(message_id(MessageId)),
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
        uid_search(IMAP, SearchKey, no, ResSearch, !IO),
        (
            ResSearch = ok(result(Status, Text, Alerts)),
            report_alerts(Log, Alerts, !IO),
            (
                Status = ok_with_data(uid_search_result(UIDs,
                    _HighestModSeqValueOfFound, _ReturnDatas)),
                log_debug(Log, Text, !IO),
                ( UIDs = [UID] ->
                    Res = ok(yes(UID))
                ;
                    Res = ok(no)
                )
            ;
                ( Status = no
                ; Status = bad
                ; Status = bye
                ; Status = continue
                ),
                Res = error("unexpected response to UID SEARCH: " ++ Text)
            )
        ;
            ResSearch = eof,
            Res = eof
        ;
            ResSearch = error(Error),
            Res = error(Error)
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
