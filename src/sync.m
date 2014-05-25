%-----------------------------------------------------------------------------%

:- module sync.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module database.
:- import_module dir_cache.
:- import_module imap.
:- import_module imap.types.
:- import_module prog_config.

:- pred sync_mailboxes(prog_config::in, database::in, imap::in,
    mailbox_pair::in, mod_seq_valzer::in, mod_seq_value::in, maybe_error::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module sync.download.
:- include_module sync.flags_local.
:- include_module sync.flags_remote.
:- include_module sync.update_local.
:- include_module sync.update_remote.
:- include_module sync.upload.

:- import_module sync.download.
:- import_module sync.flags_local.
:- import_module sync.flags_remote.
:- import_module sync.update_local.
:- import_module sync.update_remote.
:- import_module sync.upload.

%-----------------------------------------------------------------------------%

sync_mailboxes(Config, Db, IMAP, MailboxPair,
        LastModSeqValzer, HighestModSeqValue, Res, DirCache0, DirCache, !IO) :-
    % It might be better to get set of valid UIDs first, then use that
    % as part of update_db_remote_mailbox and for detecting expunges.
    update_db_remote_mailbox(Config, Db, IMAP, MailboxPair,
        LastModSeqValzer, HighestModSeqValue, ResUpdate, !IO),
    (
        ResUpdate = ok,
        detect_remote_message_expunges(Db, IMAP, MailboxPair,
            ResRemoteExpunges, !IO),
        (
            ResRemoteExpunges = ok,
            update_db_local_mailbox(Db, MailboxPair, DirCache0, ResUpdateLocal,
                !IO),
            (
                ResUpdateLocal = ok(DirCache),
                % Propagate flags first to allow pairings with
                % previously-expunged messages to be be reset, and thus
                % downloaded in the following steps.
                propagate_flag_deltas_from_remote(Config, Db, MailboxPair,
                    DirCache, ResPropRemote, !IO),
                (
                    ResPropRemote = ok,
                    propagate_flag_deltas_from_local(Config, Db, IMAP,
                        MailboxPair, ResPropLocal, !IO),
                    (
                        ResPropLocal = ok,
                        download_unpaired_remote_messages(Config, Db, IMAP,
                            MailboxPair, DirCache, ResDownload, !IO),
                        % DirCache does not include newly added messages.
                        (
                            ResDownload = ok,
                            upload_unpaired_local_messages(Config, Db, IMAP,
                                MailboxPair, DirCache, ResUpload, !IO),
                            (
                                ResUpload = ok,
                                delete_expunged_pairings(Db, Res, !IO)
                            ;
                                ResUpload = error(Error),
                                Res = error(Error)
                            )
                        ;
                            ResDownload = error(Error),
                            Res = error(Error)
                        )
                    ;
                        ResPropLocal = error(Error),
                        Res = error(Error)
                    )
                ;
                    ResPropRemote = error(Error),
                    Res = error(Error)
                )
            ;
                ResUpdateLocal = error(Error),
                Res = error(Error),
                DirCache = DirCache0
            )
        ;
            ResRemoteExpunges = error(Error),
            Res = error(Error),
            DirCache = DirCache0
        )
    ;
        ResUpdate = error(Error),
        Res = error(Error),
        DirCache = DirCache0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
