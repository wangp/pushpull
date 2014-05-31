%-----------------------------------------------------------------------------%

:- module sync.download.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred download_unpaired_remote_messages(prog_config::in, database::in,
    imap::in, mailbox_pair::in, dir_cache::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- import_module flag_delta.
:- import_module imap.time.
:- import_module log.
:- import_module maildir.
:- import_module message_file.
:- import_module path.
:- import_module utime.
:- import_module verify_file.

%-----------------------------------------------------------------------------%

download_unpaired_remote_messages(Config, Database, IMAP, MailboxPair,
        DirCache, Res, !IO) :-
    search_unpaired_remote_messages(Database, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(Unpaireds),
        download_messages(Config, Database, IMAP, MailboxPair, DirCache,
            Unpaireds, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred download_messages(prog_config::in, database::in, imap::in,
    mailbox_pair::in, dir_cache::in, list(unpaired_remote_message)::in,
    maybe_error::out, io::di, io::uo) is det.

download_messages(Config, Database, IMAP, MailboxPair, DirCache, Unpaireds,
        Res, !IO) :-
    (
        Unpaireds = [],
        Res = ok
    ;
        Unpaireds = [_ | _],
        list.split_upto(max_batch_messages, Unpaireds, Heads, Tails),
        download_message_batch(Config, Database, IMAP, MailboxPair, DirCache,
            Heads, Res0, !IO),
        (
            Res0 = ok,
            download_messages(Config, Database, IMAP, MailboxPair, DirCache,
                Tails, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- func max_batch_messages = int.

max_batch_messages = 50.

:- pred download_message_batch(prog_config::in, database::in, imap::in,
    mailbox_pair::in, dir_cache::in, list(unpaired_remote_message)::in,
    maybe_error::out, io::di, io::uo) is det.

download_message_batch(Config, Database, IMAP, MailboxPair, DirCache,
        UnpairedRemotes, Res, !IO) :-
    UIDs = list.map(get_uid, UnpairedRemotes),
    ( make_sequence_set(UIDs, SequenceSet) ->
        % Need FLAGS for Maildir filename.
        % INTERNALDATE for setting mtime on new files.
        %
        % [RFC 4551] "Once the client specified the MODSEQ message data item in
        % a FETCH request, the server MUST include the MODSEQ fetch response
        % data items in all subsequent unsolicited FETCH responses."
        % We will use this to update our highest mod-seq-value.
        Items = atts(rfc822, [flags, modseq, internaldate]),
        uid_fetch(IMAP, SequenceSet, Items, no,
            result(ResFetch, Text, Alerts), !IO),
        report_alerts(Alerts, !IO)
    ;
        % Empty sequence.
        ResFetch = ok_with_data([]),
        Text = ""
    ),
    (
        ResFetch = ok_with_data(FetchResults),
        LocalMailboxName = get_local_mailbox_name(MailboxPair),
        LocalMailboxPath = make_local_mailbox_path(Config, LocalMailboxName),
        list.foldl2(handle_downloaded_message(Database, MailboxPair,
            LocalMailboxPath, DirCache, FetchResults), UnpairedRemotes,
            ok, Res, !IO)
    ;
        ( ResFetch = no
        ; ResFetch = bad
        ; ResFetch = bye
        ; ResFetch = continue
        ; ResFetch = error
        ),
        Res = error("unexpected response to UID FETCH: " ++ Text)
    ).

:- pred handle_downloaded_message(database::in, mailbox_pair::in,
    local_mailbox_path::in, dir_cache::in,
    assoc_list(message_seq_nr, msg_atts)::in, unpaired_remote_message::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

handle_downloaded_message(Database, MailboxPair, LocalMailboxPath, DirCache,
        FetchResults, UnpairedRemote, Res0, Res, !IO) :-
    (
        Res0 = ok,
        handle_downloaded_message_2(Database, MailboxPair, LocalMailboxPath,
            DirCache, FetchResults, UnpairedRemote, Res, !IO)
    ;
        Res0 = error(_),
        Res = Res0
    ).

:- pred handle_downloaded_message_2(database::in, mailbox_pair::in,
    local_mailbox_path::in, dir_cache::in,
    assoc_list(message_seq_nr, msg_atts)::in, unpaired_remote_message::in,
    maybe_error::out, io::di, io::uo) is det.

handle_downloaded_message_2(Database, MailboxPair, LocalMailboxPath, DirCache,
        FetchResults, UnpairedRemote, Res, !IO) :-
    UnpairedRemote = unpaired_remote_message(_PairingId, UID,
        ExpectedMessageId),
    ( find_uid_fetch_result(FetchResults, UID, Atts) ->
        % XXX If changes are being made on the remote mailbox concurrently the
        % server (at least Dovecot) may send unsolicited FETCH responses which
        % are not directly in response to our FETCH request, and therefore not
        % matching the items that we asked for.  We probably need to filter
        % those responses out, or add them to the responses that we did ask
        % for.
        (
            get_rfc822_att(Atts, RawMessageCrLf),
            get_flags(Atts, Flags),
            get_internaldate(Atts, InternalDate)
        ->
            read_message_id_from_message_crlf(RawMessageCrLf, ResMessageId),
            (
                (
                    ResMessageId = yes(ReadMessageId),
                    HaveMessageId = message_id(ReadMessageId)
                ;
                    ResMessageId = no,
                    HaveMessageId = nil
                ),
                ( ExpectedMessageId = HaveMessageId ->
                    RawMessageLf = crlf_to_lf(RawMessageCrLf),
                    save_message_and_pair(Database, MailboxPair,
                        LocalMailboxPath, UnpairedRemote, RawMessageLf, Flags,
                        InternalDate, DirCache, Res, !IO)
                ;
                    Res = error("unexpected Message-Id")
                )
            ;
                ResMessageId = format_error(Error),
                Res = error("format error in RFC822 response: " ++ Error)
            ;
                ResMessageId = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            Res = error("problem with UID FETCH response")
        )
    ;
        % Message no longer exists on server?
        Res = ok
    ).

:- pred find_uid_fetch_result(assoc_list(message_seq_nr, msg_atts)::in,
    uid::in, msg_atts::out) is semidet.

find_uid_fetch_result(FetchResults, UID, MsgAtts) :-
    list.find_first_match(
        (pred(_ - Atts::in) is semidet :- list.member(uid(UID), Atts)),
        FetchResults, _MsgSeqNr - MsgAtts).

:- pred get_rfc822_att(msg_atts::in, string::out) is semidet.

get_rfc822_att(Atts, String) :-
    solutions(pred(X::out) is nondet :- member(rfc822(X), Atts), [NString]),
    NString = yes(IString),
    String = from_imap_string(IString).

:- pred get_flags(msg_atts::in, set(flag)::out) is semidet.

get_flags(Atts, Flags) :-
    solutions(pred(X::out) is nondet :- member(flags(X), Atts), [Flags0]),
    list.filter_map(flag_except_recent, Flags0, Flags1),
    set.list_to_set(Flags1, Flags).

% XXX dupe
:- pred flag_except_recent(flag_fetch::in, flag::out) is semidet.

flag_except_recent(flag(Flag), Flag).
flag_except_recent(recent, _) :- fail.

:- pred get_internaldate(msg_atts::in, date_time::out) is semidet.

get_internaldate(Atts, DateTime) :-
    solutions(pred(X::out) is nondet :- member(internaldate(X), Atts),
        [DateTime]).

:- pred save_message_and_pair(database::in, mailbox_pair::in,
    local_mailbox_path::in, unpaired_remote_message::in, string::in,
    set(flag)::in, date_time::in, dir_cache::in, maybe_error::out,
    io::di, io::uo) is det.

save_message_and_pair(Database, MailboxPair, LocalMailboxPath, UnpairedRemote,
        RawMessageLf, Flags, InternalDate, DirCache, Res, !IO) :-
    UnpairedRemote = unpaired_remote_message(PairingId, UID, MessageId),
    % Avoid duplicating an existing local message.
    match_unpaired_local_message(Database, MailboxPair, DirCache, MessageId,
        RawMessageLf, ResMatch, !IO),
    (
        ResMatch = ok(no),
        save_raw_message(LocalMailboxPath, UID, RawMessageLf, Flags,
            InternalDate, ResSave, !IO),
        (
            ResSave = ok(Unique),
            set_pairing_local_message(Database, PairingId, Unique,
                init_flags(Flags), Res, !IO)
        ;
            ResSave = error(Error),
            Res = error(Error)
        )
    ;
        ResMatch = ok(yes(UnpairedLocal)),
        UnpairedLocal = unpaired_local_message(OtherPairingId, Unique),
        % XXX use a transaction around this sequence
        lookup_local_message_flags(Database, OtherPairingId, ResLocalFlags,
            !IO),
        delete_pairing(Database, OtherPairingId, ResDeleteOther, !IO),
        (
            ResDeleteOther = ok,
            (
                ResLocalFlags = ok(LocalFlags),
                set_pairing_local_message(Database, PairingId, Unique,
                    LocalFlags, Res, !IO)
            ;
                ResLocalFlags = error(Error),
                Res = error(Error)
            )
        ;
            ResDeleteOther = error(Error),
            Res = error(Error)
        )
    ;
        ResMatch = error(Error),
        Res = error(Error)
    ).

:- pred match_unpaired_local_message(database::in, mailbox_pair::in,
    dir_cache::in, maybe_message_id::in, string::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

match_unpaired_local_message(Database, MailboxPair, DirCache,
        MessageId, RawMessageLf, Res, !IO) :-
    search_unpaired_local_messages_by_message_id(Database, MailboxPair,
        MessageId, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        verify_unpaired_local_messages(DirCache, UnpairedLocals, RawMessageLf,
            Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred verify_unpaired_local_messages(dir_cache::in,
    list(unpaired_local_message)::in, string::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

verify_unpaired_local_messages(DirCache, UnpairedLocals, RawMessageLf, Res,
        !IO) :-
    (
        UnpairedLocals = [],
        Res = ok(no)
    ;
        UnpairedLocals = [UnpairedLocal | RestUnpairedLocals],
        verify_unpaired_local_message(DirCache, UnpairedLocal, RawMessageLf,
            ResVerify, !IO),
        (
            ResVerify = ok(yes),
            Res = ok(yes(UnpairedLocal))
        ;
            ResVerify = ok(no),
            verify_unpaired_local_messages(DirCache, RestUnpairedLocals,
                RawMessageLf, Res, !IO)
        ;
            ResVerify = error(Error),
            Res = error(Error)
        )
    ).

:- pred verify_unpaired_local_message(dir_cache::in,
    unpaired_local_message::in, string::in, maybe_error(bool)::out,
    io::di, io::uo) is det.

verify_unpaired_local_message(DirCache, UnpairedLocal, RawMessageLf, Res, !IO)
        :-
    UnpairedLocal = unpaired_local_message(_PairingId, Unique),
    find_file(DirCache, Unique, ResFind),
    (
        ResFind = found(DirName, BaseName, _InfoSuffix),
        DirName / BaseName = path(Path),
        io.format("Verifying %s\n", [s(Path)], !IO),
        verify_file(Path, RawMessageLf, Res, !IO)
    ;
        ResFind = found_but_unexpected(path(Path)),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = not_found,
        Res = ok(no)
    ).

:- pred save_raw_message(local_mailbox_path::in, uid::in, string::in,
    set(flag)::in, date_time::in,  maybe_error(uniquename)::out,
    io::di, io::uo) is det.

save_raw_message(LocalMailboxPath, uid(UID), RawMessageLf, Flags, InternalDate,
        Res, !IO) :-
    generate_unique_tmp_path(LocalMailboxPath, ResUnique, !IO),
    (
        ResUnique = ok({Unique, TmpPath}),
        InfoSuffix = flags_to_info_suffix(Flags),
        % XXX don't think this condition is right
        ( InfoSuffix = info_suffix(set.init, "") ->
            make_path(LocalMailboxPath, new, Unique, no, DestDir, DestBaseName)
        ;
            make_path(LocalMailboxPath, cur, Unique, yes(InfoSuffix),
                DestDir, DestBaseName)
        ),
        DestDir / DestBaseName = path(DestPath),
        io.format("Saving message UID %s to %s\n",
            [s(to_string(UID)), s(DestPath)], !IO),
        io.open_output(TmpPath, ResOpen, !IO),
        (
            ResOpen = ok(Stream),
            % XXX catch exceptions during write and fsync
            io.write_string(Stream, RawMessageLf, !IO),
            io.close_output(Stream, !IO),
            set_file_atime_mtime(TmpPath, mktime(InternalDate), ResTime, !IO),
            (
                ResTime = ok,
                io.rename_file(TmpPath, DestPath, ResRename, !IO),
                (
                    ResRename = ok,
                    Res = ok(Unique)
                ;
                    ResRename = error(Error),
                    io.remove_file(TmpPath, _, !IO),
                    Res = error(io.error_message(Error))
                )
            ;
                ResTime = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            ResOpen = error(Error),
            io.remove_file(TmpPath, _, !IO),
            Res = error(io.error_message(Error))
        )
    ;
        ResUnique = error(Error),
        Res = error(Error)
    ).

:- func crlf_to_lf(string) = string.

crlf_to_lf(S) = string.replace_all(S, "\r\n", "\n").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
