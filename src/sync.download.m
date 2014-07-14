%-----------------------------------------------------------------------------%

:- module sync.download.
:- interface.

:- import_module io.

:- import_module dir_cache.

:- pred download_unpaired_remote_messages(log::in, prog_config::in,
    database::in, imap::in, mailbox_pair::in, maybe_error::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module unit.

:- import_module binary_string.
:- import_module crc8.
:- import_module flag_delta.
:- import_module imap.time.
:- import_module log.
:- import_module log_help.
:- import_module lowio.
:- import_module maildir.
:- import_module message_file.
:- import_module path.
:- import_module signal.
:- import_module utime.
:- import_module verify_file.

:- type remote_message
    --->    remote_message(
                unpaired        :: unpaired_remote_message,
                raw_message     :: message(lf),
                flags           :: set(flag),
                internaldate    :: date_time,
                count           :: int,
                total           :: int
            ).

:- type save_result
    --->    ok(uniquename, dirname, basename)
    ;       error(string).

%-----------------------------------------------------------------------------%

download_unpaired_remote_messages(Log, Config, Database, IMAP, MailboxPair,
        Res, !DirCache, !IO) :-
    log_debug(Log, "Find remote messages to download", !IO),
    search_unpaired_remote_messages(Database, MailboxPair, ResSearch, !IO),
    (
        ResSearch = ok(Unpaireds),
        list.length(Unpaireds, Total),
        download_messages(Log, Config, Database, IMAP, MailboxPair, Total,
            Unpaireds, Res, 1, _Count, !DirCache, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred download_messages(log::in, prog_config::in, database::in, imap::in,
    mailbox_pair::in, int::in, list(unpaired_remote_message)::in,
    maybe_error::out, int::in, int::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

download_messages(Log, Config, Database, IMAP, MailboxPair, Total, Unpaireds,
        Res, !Count, !DirCache, !IO) :-
    (
        Unpaireds = [],
        Res = ok
    ;
        Unpaireds = [_ | _],
        list.split_upto(max_batch_messages, Unpaireds, Heads, Tails),
        download_message_batch(Log, Config, Database, IMAP, MailboxPair, Total,
            Heads, Res0, !Count, !DirCache, !IO),
        (
            Res0 = ok,
            download_messages(Log, Config, Database, IMAP, MailboxPair, Total,
                Tails, Res, !Count, !DirCache, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- func max_batch_messages = int.

max_batch_messages = 50.

:- pred download_message_batch(log::in, prog_config::in, database::in,
    imap::in, mailbox_pair::in, int::in, list(unpaired_remote_message)::in,
    maybe_error::out, int::in, int::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

download_message_batch(Log, Config, Database, IMAP, MailboxPair,
        Total, UnpairedRemotes, Res, !Count, !DirCache, !IO) :-
    UIDs = list.map(get_uid, UnpairedRemotes),
    ( make_sequence_set(UIDs, SequenceSet) ->
        % Use BODY.PEEK instead of RFC822 to avoid setting \Seen flag.
        % Need FLAGS for Maildir filename.
        % INTERNALDATE for setting mtime on new files.
        %
        % [RFC 4551] "Once the client specified the MODSEQ message data item in
        % a FETCH request, the server MUST include the MODSEQ fetch response
        % data items in all subsequent unsolicited FETCH responses."
        % We will use this to update our highest mod-seq-value.
        Items = atts(body_peek(entire_message, no),
            [flags, modseq, internaldate]),
        uid_fetch(IMAP, SequenceSet, Items, no,
            result(ResFetch, Text, Alerts), !IO),
        report_alerts(Log, Alerts, !IO)
    ;
        % Empty sequence.
        ResFetch = ok_with_data([]),
        Text = ""
    ),
    (
        ResFetch = ok_with_data(FetchResults),
        LocalMailboxName = get_local_mailbox_name(MailboxPair),
        LocalMailboxPath = make_local_mailbox_path(Config, LocalMailboxName),
        handle_downloaded_messages(Log, Config, Database,
            MailboxPair, LocalMailboxPath, Total, FetchResults,
            UnpairedRemotes, Res, !Count, !DirCache, !IO)
    ;
        ( ResFetch = no
        ; ResFetch = bad
        ; ResFetch = bye
        ; ResFetch = continue
        ; ResFetch = error
        ),
        Res = error("unexpected response to UID FETCH: " ++ Text)
    ).

:- pred handle_downloaded_messages(log::in, prog_config::in, database::in,
    mailbox_pair::in, local_mailbox_path::in, int::in,
    assoc_list(message_seq_nr, msg_atts)::in,
    list(unpaired_remote_message)::in, maybe_error::out,
    int::in, int::out, dir_cache::in, dir_cache::out, io::di, io::uo) is det.

handle_downloaded_messages(Log, Config, Database, MailboxPair,
        LocalMailboxPath, Total, FetchResults, UnpairedRemotes, Res,
        !Count, !DirCache, !IO) :-
    (
        UnpairedRemotes = [],
        Res = ok
    ;
        UnpairedRemotes = [H | T],
        handle_downloaded_message(Log, Config, Database, MailboxPair,
            LocalMailboxPath, Total, FetchResults, H, Res0,
            !Count, !DirCache, !IO),
        (
            Res0 = ok,
            signal.get_sigint_count(Sigint, !IO),
            ( Sigint > 0 ->
                Res = error("interrupted")
            ;
                handle_downloaded_messages(Log, Config, Database, MailboxPair,
                    LocalMailboxPath, Total, FetchResults, T, Res,
                    !Count, !DirCache, !IO)
            )
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

:- pred handle_downloaded_message(log::in, prog_config::in, database::in,
    mailbox_pair::in, local_mailbox_path::in, int::in,
    assoc_list(message_seq_nr, msg_atts)::in, unpaired_remote_message::in,
    maybe_error::out, int::in, int::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

handle_downloaded_message(Log, Config, Database, MailboxPair,
        LocalMailboxPath, Total, FetchResults, UnpairedRemote, Res,
        Count, Count + 1, !DirCache, !IO) :-
    UnpairedRemote = unpaired_remote_message(_PairingId, UID,
        ExpectedMessageId),
    % If changes are being made on the remote mailbox concurrently the server
    % (at least Dovecot) may send unsolicited FETCH responses which are not
    % directly in response to our FETCH request, and therefore not matching the
    % items that we asked for.
    ( find_uid_fetch_result(FetchResults, UID, Atts) ->
        (
            get_entire_body(Atts, RawMessageCrLf),
            get_flags(Atts, Flags),
            get_internaldate(Atts, InternalDate)
        ->
            read_message_id_from_message_crlf(RawMessageCrLf, ResMessageId),
            (
                (
                    ResMessageId = yes(message_id(ReadMessageId)),
                    HaveMessageId = message_id(ReadMessageId)
                ;
                    ResMessageId = no,
                    HaveMessageId = nil
                ),
                ( ExpectedMessageId = HaveMessageId ->
                    RawMessageLf = crlf_to_lf(RawMessageCrLf),
                    RemoteMessage = remote_message(UnpairedRemote,
                        RawMessageLf, Flags, InternalDate, Count, Total),
                    save_message_and_pair(Log, Config, Database, MailboxPair,
                        LocalMailboxPath, RemoteMessage, Res, !DirCache, !IO)
                ;
                    Res = error("unexpected Message-Id")
                )
            ;
                ResMessageId = format_error(Error),
                Res = error(Error)
            ;
                ResMessageId = error(Error),
                Res = error(Error)
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

:- pred get_entire_body(msg_atts::in, message(crlf)::out) is semidet.

get_entire_body(Atts, message(BinaryString)) :-
    solutions(
        (pred(X::out) is nondet :-
            member(body(entire_message, no, X), Atts)
        ),
        [NString]),
    NString = yes(IString),
    (
        IString = quoted(S),
        BinaryString = from_string(S)
    ;
        IString = literal(BinaryString)
    ).

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

:- pred save_message_and_pair(log::in, prog_config::in, database::in,
    mailbox_pair::in, local_mailbox_path::in, remote_message::in,
    maybe_error::out, dir_cache::in, dir_cache::out, io::di, io::uo) is det.

save_message_and_pair(Log, Config, Database, MailboxPair, LocalMailboxPath,
        RemoteMessage, Res, !DirCache, !IO) :-
    RemoteMessage = remote_message(UnpairedRemote, _RawMessageLf, Flags,
        _InternalDate, _Count, _Total),
    UnpairedRemote = unpaired_remote_message(PairingId, _UID, _MessageId),
    % Avoid duplicating an existing local message.
    match_unpaired_local_message(Log, Database, MailboxPair, !.DirCache,
        RemoteMessage, ResMatch, !IO),
    (
        ResMatch = ok(no),
        save_raw_message(Log, Config, LocalMailboxPath, RemoteMessage,
            ResSave, !IO),
        (
            ResSave = ok(Unique, DirName, BaseName),
            (
                update_for_new_file(DirName, BaseName, !DirCache)
            ->
                set_pairing_local_message(Database, PairingId, Unique,
                    init_flags(Flags), Res, !IO)
            ;
                Res = error("update_for_new_file failed")
            )
        ;
            ResSave = error(Error),
            Res = error(Error)
        )
    ;
        ResMatch = ok(yes(UnpairedLocal)),
        UnpairedLocal = unpaired_local_message(OtherPairingId, Unique),
        lookup_local_message_flags(Database, OtherPairingId, ResLocalFlags,
            !IO),
        (
            ResLocalFlags = ok(LocalFlags),
            % Delete OtherPairingId, keep PairingId.
            transaction(
                delete_pairing_set_pairing_local_message(Database,
                    OtherPairingId, PairingId, Unique, LocalFlags),
                Database, ResTxn, !IO),
            (
                ResTxn = ok(commit(_ : unit)),
                Res = ok
            ;
                ResTxn = ok(rollback(Error)),
                Res = error(Error)
            ;
                ResTxn = ok(rollback_exception(Univ)),
                Res = error("exception thrown: " ++ string(Univ))
            ;
                ResTxn = error(Error),
                Res = error(Error)
            )
        ;
            ResLocalFlags = error(Error),
            Res = error(Error)
        )
    ;
        ResMatch = error(Error),
        Res = error(Error)
    ).

:- pred match_unpaired_local_message(log::in, database::in, mailbox_pair::in,
    dir_cache::in, remote_message::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

match_unpaired_local_message(Log, Database, MailboxPair, DirCache,
        RemoteMessage, Res, !IO) :-
    UnpairedRemote = RemoteMessage ^ unpaired,
    UnpairedRemote = unpaired_remote_message(_PairingId, _UID, MessageId),
    search_unpaired_local_messages_by_message_id(Database, MailboxPair,
        MessageId, ResSearch, !IO),
    (
        ResSearch = ok(UnpairedLocals),
        verify_unpaired_local_messages(Log, DirCache, RemoteMessage,
            UnpairedLocals, Res, !IO)
    ;
        ResSearch = error(Error),
        Res = error(Error)
    ).

:- pred delete_pairing_set_pairing_local_message(database::in, pairing_id::in,
    pairing_id::in, uniquename::in, flag_deltas(local_mailbox)::in,
    transaction_result(unit, string)::out, io::di, io::uo) is det.

delete_pairing_set_pairing_local_message(Database, UnwantedPairingId,
        KeepPairingId, Unique, LocalFlags, Res, !IO) :-
    delete_pairing(Database, UnwantedPairingId, ResDelete, !IO),
    (
        ResDelete = ok,
        set_pairing_local_message(Database, KeepPairingId, Unique, LocalFlags,
            ResSetPairingLocal, !IO),
        (
            ResSetPairingLocal = ok,
            Res = commit(unit)
        ;
            ResSetPairingLocal = error(Error),
            Res = rollback(Error)
        )
    ;
        ResDelete = error(Error),
        Res = rollback(Error)
    ).

:- pred verify_unpaired_local_messages(log::in, dir_cache::in,
    remote_message::in, list(unpaired_local_message)::in,
    maybe_error(maybe(unpaired_local_message))::out, io::di, io::uo) is det.

verify_unpaired_local_messages(Log, DirCache, RemoteMessage, UnpairedLocals,
        Res, !IO) :-
    (
        UnpairedLocals = [],
        Res = ok(no)
    ;
        UnpairedLocals = [UnpairedLocal | RestUnpairedLocals],
        verify_unpaired_local_message(Log, DirCache, RemoteMessage, UnpairedLocal,
            ResVerify, !IO),
        (
            ResVerify = ok(yes),
            Res = ok(yes(UnpairedLocal))
        ;
            ResVerify = ok(no),
            verify_unpaired_local_messages(Log, DirCache, RemoteMessage,
                RestUnpairedLocals, Res, !IO)
        ;
            ResVerify = error(Error),
            Res = error(Error)
        )
    ).

:- pred verify_unpaired_local_message(log::in, dir_cache::in,
    remote_message::in, unpaired_local_message::in, maybe_error(bool)::out,
    io::di, io::uo) is det.

verify_unpaired_local_message(Log, DirCache, RemoteMessage, UnpairedLocal,
        Res, !IO) :-
    RemoteMessage = remote_message(UnpairedRemote, message(RawMessageLf),
        _Flags, _InternalDate, Count, Total),
    UnpairedRemote = unpaired_remote_message(_PairingIdWithRemote, uid(UID),
        _MessageId),
    UnpairedLocal = unpaired_local_message(_PairingIdWithLocal, Unique),
    find_file(DirCache, Unique, ResFind),
    (
        ResFind = found(DirName, BaseName, _InfoSuffix),
        DirName / BaseName = path(Path),
        log_info(Log,
            format("Verifying UID %s (%d of %d) is %s",
                [s(to_string(UID)), i(Count), i(Total), s(Path)]), !IO),
        verify_file(Path, RawMessageLf, Res, !IO)
    ;
        ResFind = found_but_unexpected(path(Path)),
        Res = error("found unique name but unexpected: " ++ Path)
    ;
        ResFind = not_found,
        Res = ok(no)
    ).

:- pred save_raw_message(log::in, prog_config::in, local_mailbox_path::in,
    remote_message::in, save_result::out, io::di, io::uo) is det.

save_raw_message(Log, Config, local_mailbox_path(DirName), RemoteMessage,
        Res, !IO) :-
    RemoteMessage = remote_message(UnpairedRemote, _RawMessageLf, _Flags,
        _InternalDate, _Count, _Total),
    UnpairedRemote = unpaired_remote_message(_PairingId, _UID, MessageId),
    Buckets = Config ^ buckets,
    (
        Buckets = use_buckets,
        SubDirName = DirName / hex_bits(MessageId)
    ;
        Buckets = no_buckets,
        SubDirName = DirName
    ),
    dir.make_directory(SubDirName / "tmp", Res0, !IO),
    (
        Res0 = ok,
        dir.make_directory(SubDirName / "cur", Res1, !IO),
        (
            Res1 = ok,
            dir.make_directory(SubDirName / "new", Res2, !IO),
            (
                Res2 = ok,
                save_raw_message_2(Log, Config, dirname(SubDirName),
                    RemoteMessage, Res, !IO)
            ;
                Res2 = error(Error),
                Res = error(io.error_message(Error))
            )
        ;
            Res1 = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        Res0 = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred save_raw_message_2(log::in, prog_config::in, dirname::in,
    remote_message::in, save_result::out, io::di, io::uo) is det.

save_raw_message_2(Log, Config, dirname(SubDirName), RemoteMessage,
        Res, !IO) :-
    RemoteMessage = remote_message(UnpairedRemote, RawMessageLf, Flags,
        InternalDate, Count, Total),
    UnpairedRemote = unpaired_remote_message(_PairingId, uid(UID), _MessageId),

    TmpDir = SubDirName / "tmp",
    DestDir = SubDirName / "cur",
    generate_unique_name(dirname(TmpDir), ResUnique, !IO),
    (
        ResUnique = ok({Unique, path(TmpPath), Fd}),
        InfoSuffix = flags_to_info_suffix(Flags),
        % XXX don't think this condition is right
        ( InfoSuffix = info_suffix(set.init, "") ->
            make_message_basename(Unique, no, basename(BaseName))
        ;
            make_message_basename(Unique, yes(InfoSuffix), basename(BaseName))
        ),
        DestPath = DestDir / BaseName,
        log_notice(Log,
            format("Saving UID %s (%d of %d) to %s\n",
                [s(to_string(UID)), i(Count), i(Total), s(DestPath)]), !IO),
        do_save_raw_message(Config, path(TmpPath), dirname(DestDir),
            path(DestPath), Fd, RawMessageLf, InternalDate, ResSave, !IO),
        (
            ResSave = ok,
            Res = ok(Unique, dirname(DestDir), basename(BaseName))
        ;
            ResSave = error(Error),
            Res = error(Error)
        )
    ;
        ResUnique = error(Error),
        Res = error(Error)
    ).

:- pred do_save_raw_message(prog_config::in, path::in, dirname::in, path::in,
    filedes::in, message(lf)::in, date_time::in, maybe_error::out,
    io::di, io::uo) is det.

do_save_raw_message(Config, path(TmpPath), DestDir, path(DestPath),
        Fd, message(RawMessageLf), InternalDate, Res, !IO) :-
    lowio.write_binary_string(Fd, RawMessageLf, ResWrite, !IO),
    (
        ResWrite = ok,
        maybe_fsync(Config, Fd, ResFsync, !IO),
        (
            ResFsync = ok,
            lowio.close(Fd, ResClose, !IO),
            (
                ResClose = ok,
                set_file_atime_mtime(TmpPath, mktime(InternalDate),
                    ResTime, !IO),
                (
                    ResTime = ok,
                    io.rename_file(TmpPath, DestPath, ResRename, !IO),
                    (
                        ResRename = ok,
                        maybe_fsync_dir(Config, DestDir, ResFsyncDir, !IO),
                        (
                            ResFsyncDir = ok,
                            Res = ok
                        ;
                            ResFsyncDir = error(Error),
                            Res = error(Error)
                        )
                    ;
                        ResRename = error(Error),
                        Res = error(io.error_message(Error))
                    )
                ;
                    ResTime = error(Error),
                    Res = error(io.error_message(Error))
                )
            ;
                ResClose = error(Error),
                Res = error(Error)
            )
        ;
            ResFsync = error(Error),
            Res = error(Error),
            lowio.close(Fd, _ResClose, !IO)
        )
    ;
        ResWrite = error(Error),
        Res = error(Error),
        lowio.close(Fd, _ResClose, !IO)
    ),
    (
        Res = ok
    ;
        Res = error(_),
        io.remove_file(TmpPath, _, !IO)
    ).

:- pred maybe_fsync(prog_config::in, filedes::in, maybe_error::out,
    io::di, io::uo) is det.

maybe_fsync(Config, Fd, Res, !IO) :-
    Fsync = Config ^ fsync,
    (
        Fsync = do_fsync,
        lowio.fsync(Fd, Res, !IO)
    ;
        Fsync = do_not_fsync,
        Res = ok
    ).

:- pred maybe_fsync_dir(prog_config::in, dirname::in, maybe_error::out,
    io::di, io::uo) is det.

maybe_fsync_dir(Config, dirname(DirName), Res, !IO) :-
    Fsync = Config ^ fsync,
    (
        Fsync = do_fsync,
        lowio.fsync_dir(DirName, Res, !IO)
    ;
        Fsync = do_not_fsync,
        Res = ok
    ).

:- func hex_bits(maybe_message_id) = string.

hex_bits(MessageId) = Str :-
    % Divide messages into 256 sub-maildirs.  If we use too many sub-maildirs
    % we run into the inotify default limit of 8192 watches per user.  We need
    % to watch two directories (cur, new) per maildir.  Scanning and setting up
    % many small directories is also slow.
    (
        MessageId = message_id(Id),
        string.to_code_unit_list(Id, Bytes),
        Crc = crc_8(Bytes)
    ;
        MessageId = nil,
        Crc = crc_8([])
    ),
    string.format("%02x", [i(Crc)], Str).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
