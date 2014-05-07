%-----------------------------------------------------------------------------%

:- module database.
:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module imap.
:- import_module imap.types.
:- import_module maildir.

%-----------------------------------------------------------------------------%

:- type database.

:- pred open_database(string::in, maybe_error(database)::out, io::di, io::uo)
    is det.

:- pred close_database(database::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type local_mailbox.

:- type remote_mailbox.

:- type local_mailbox_path
    --->    local_mailbox_path(string). % XXX canonicalise

:- type lookup_remote_mailbox_result
    --->    found(remote_mailbox, mod_seq_valzer)
    ;       error(string).

    % Insert a local_mailbox row into the database with the given path.
    % If such a row already exists then do nothing.
    %
:- pred insert_or_ignore_local_mailbox(database::in, local_mailbox_path::in,
    maybe_error::out, io::di, io::uo) is det.

    % Lookup the local_mailbox in the database with the given path.
    %
:- pred lookup_local_mailbox(database::in, local_mailbox_path::in,
    maybe_error(local_mailbox)::out, io::di, io::uo) is det.

    % Insert a remote_mailbox row into the database with the given mailbox name
    % and uidvalidity value.  If such a row already exists then do nothing.
    %
:- pred insert_or_ignore_remote_mailbox(database::in, mailbox::in,
    uidvalidity::in, maybe_error::out, io::di, io::uo) is det.

    % Lookup the remote_mailbox in the database with the given mailbox name
    % and uidvalidity value.
    %
:- pred lookup_remote_mailbox(database::in, mailbox::in, uidvalidity::in,
    lookup_remote_mailbox_result::out, io::di, io::uo) is det.

:- pred update_remote_mailbox_modseqvalue(database::in, remote_mailbox::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type pairing_id.

:- pred search_pairing_by_remote_message(database::in, remote_mailbox::in,
    uid::in, message_id::in, maybe_error(maybe(pairing_id))::out,
    io::di, io::uo) is det.

:- pred insert_new_pairing_only_remote_message(database::in, message_id::in,
    local_mailbox::in, remote_mailbox::in, uid::in, list(flag)::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred set_pairing_local_message(database::in, pairing_id::in,
    uniquename::in, list(flag)::in, maybe_error::out, io::di, io::uo) is det.

:- pred update_remote_message_flags(database::in, pairing_id::in,
    list(flag)::in, maybe_error::out, io::di, io::uo) is det.

:- pred search_pairings_without_local_message(database::in,
    remote_mailbox::in, maybe_error(assoc_list(pairing_id, uid))::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module integer.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module sqlite3.

:- type database == sqlite3.db(rw).

:- type local_mailbox
    --->    local_mailbox(local_mailbox_path, local_mailbox_id).

:- type local_mailbox_id
    --->    local_mailbox_id(int).

:- type remote_mailbox
    --->    remote_mailbox(mailbox, uidvalidity, remote_mailbox_id).

:- type remote_mailbox_id
    --->    remote_mailbox_id(int).

:- type pairing_id
    --->    pairing_id(int).

%-----------------------------------------------------------------------------%

:- typeclass bind_value(T) where [
    func bind_value(T) = bind_value
].

:- instance bind_value(local_mailbox_id) where [
    bind_value(local_mailbox_id(I)) = int(I)
].

:- instance bind_value(remote_mailbox_id) where [
    bind_value(remote_mailbox_id(I)) = int(I)
].

:- instance bind_value(pairing_id) where [
    bind_value(pairing_id(I)) = int(I)
].

:- instance bind_value(integer) where [
    bind_value(I) = text_by_reference(to_string(I))
].

:- instance bind_value(uidvalidity) where [
    bind_value(uidvalidity(I)) = bind_value(I)
].

:- instance bind_value(uid) where [
    bind_value(uid(I)) = bind_value(I)
].

:- instance bind_value(mod_seq_value) where [
    bind_value(mod_seq_value(I)) = bind_value(I)
].

:- instance bind_value(string) where [
    bind_value(S) = text_by_reference(S)
].

:- instance bind_value(mailbox) where [
    bind_value(inbox) = bind_value("INBOX"),
    bind_value(astring(S)) = bind_value(S)
].

:- instance bind_value(astring) where [
    bind_value(astring(S)) = bind_value(S),
    bind_value(imap_string(S)) = bind_value(S)
].

:- instance bind_value(imap_string) where [
    bind_value(quoted(S)) = bind_value(S),
    bind_value(literal(S)) = bind_value(S)
].

:- instance bind_value(message_id) where [
    % We will treat the lack of a Message-Id header field the same as
    % Message-Id header field with empty string for the value.
    bind_value(message_id(no)) = bind_value(""),
    bind_value(message_id(yes(S))) = bind_value(S)
].

:- instance bind_value(local_mailbox_path) where [
    bind_value(local_mailbox_path(S)) = bind_value(S)
].

:- instance bind_value(uniquename) where [
    bind_value(uniquename(S)) = bind_value(S)
].

%-----------------------------------------------------------------------------%

open_database(FileName, Res, !IO) :-
    open_rw(FileName, normal, ResOpen, !IO),
    (
        ResOpen = ok(Db),
        exec(Db, create_tables, ExecRes, !IO),
        (
            ExecRes = ok,
            Res = ok(Db)
        ;
            ExecRes = error(Error),
            close(Db, !IO),
            Res = error(Error)
        )
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

:- func create_tables = string.

create_tables =
"
    /* Foreign keys are off by default */
    PRAGMA foreign_keys = ON;

    CREATE TABLE IF NOT EXISTS local_mailbox(
        local_mailbox_id    INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        path                TEXT UNIQUE NOT NULL
    );

    CREATE TABLE IF NOT EXISTS remote_mailbox(
        remote_mailbox_id   INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        mailbox_name        TEXT NOT NULL,
        uidvalidity         NUMBER NOT NULL,
        last_modseqvalzer   INTEGER NOT NULL, /* includes zero */
        UNIQUE(mailbox_name, uidvalidity)
    );

    CREATE TABLE IF NOT EXISTS pairing(
        pairing_id          INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        message_id          TEXT NOT NULL,

        local_mailbox_id    INTEGER NOT NULL
                            REFERENCES local_mailbox(local_mailbox_id),
        local_uniquename    TEXT, /* may be NULL */
        local_flags         TEXT NOT NULL,

        remote_mailbox_id   INTEGER NOT NULL
                            REFERENCES remote_mailbox(remote_mailbox_id),
        remote_uid          INTEGER, /* may be NULL */
        remote_flags        TEXT NOT NULL,

        UNIQUE(local_mailbox_id, local_uniquename),
        UNIQUE(remote_mailbox_id, remote_uid)
    );

    CREATE INDEX IF NOT EXISTS message_id_index ON pairing(message_id);
".

%-----------------------------------------------------------------------------%

close_database(Db, !IO) :-
    close(Db, !IO).

%-----------------------------------------------------------------------------%

insert_or_ignore_local_mailbox(Db, Path, Res, !IO) :-
    Stmt = "INSERT OR IGNORE INTO local_mailbox(path) VALUES (:path)",
    with_stmt(insert_or_ignore_local_mailbox_2, Db, Stmt, [
        name(":path") - bind_value(Path)
    ], Res, !IO).

:- pred insert_or_ignore_local_mailbox_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_or_ignore_local_mailbox_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

lookup_local_mailbox(Db, Path, Res, !IO) :-
    Stmt = "SELECT local_mailbox_id FROM local_mailbox"
        ++ " WHERE path = :path",
    with_stmt(lookup_local_mailbox_2(Path), Db, Stmt, [
        name(":path") - bind_value(Path)
    ], Res, !IO).

:- pred lookup_local_mailbox_2(local_mailbox_path::in,
    db(rw)::in, stmt::in, maybe_error(local_mailbox)::out, io::di, io::uo)
    is det.

lookup_local_mailbox_2(Path, Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("local_mailbox not found")
    ;
        StepResult = row,
        column_int(Stmt, column(0), MailboxId, !IO),
        LocalMailbox = local_mailbox(Path, local_mailbox_id(MailboxId)),
        Res = ok(LocalMailbox)
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

insert_or_ignore_remote_mailbox(Db, Mailbox, UIDValidity, Res, !IO) :-
    Stmt = "INSERT OR IGNORE INTO"
        ++ " remote_mailbox(mailbox_name, uidvalidity, last_modseqvalzer)"
        ++ " VALUES (:mailbox_name, :uidvalidity, :last_modseqvalzer)",
    with_stmt(insert_or_ignore_remote_mailbox_2, Db, Stmt, [
        name(":mailbox_name") - bind_value(Mailbox),
        name(":uidvalidity") - bind_value(UIDValidity),
        name(":last_modseqvalzer") - bind_value(zero)
    ], Res, !IO).

:- pred insert_or_ignore_remote_mailbox_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

insert_or_ignore_remote_mailbox_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

lookup_remote_mailbox(Db, Mailbox, UIDValidity, Res, !IO) :-
    Stmt = "SELECT remote_mailbox_id, last_modseqvalzer FROM remote_mailbox"
        ++ " WHERE mailbox_name = :mailbox_name"
        ++ "   AND uidvalidity = :uidvalidity",
    with_stmt(lookup_remote_mailbox_2(Mailbox, UIDValidity), Db, Stmt, [
        name(":mailbox_name") - bind_value(Mailbox),
        name(":uidvalidity") - bind_value(UIDValidity)
    ], Res, !IO).

:- pred lookup_remote_mailbox_2(mailbox::in, uidvalidity::in,
    db(rw)::in, stmt::in, lookup_remote_mailbox_result::out, io::di, io::uo)
    is det.

lookup_remote_mailbox_2(Mailbox, UIDValidity, Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("remote_mailbox not found")
    ;
        StepResult = row,
        column_int(Stmt, column(0), MailboxId, !IO),
        RemoteMailbox = remote_mailbox(Mailbox, UIDValidity,
            remote_mailbox_id(MailboxId)),

        column_text(Stmt, column(1), String, !IO),
        (
            Integer = integer.from_string(String),
            Integer >= zero
        ->
            Res = found(RemoteMailbox, mod_seq_valzer(Integer))
        ;
            Res = error("invalid mod sequence value")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

update_remote_mailbox_modseqvalue(Db, RemoteMailbox, ModSeqValue, Res, !IO) :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    Stmt = "UPDATE remote_mailbox"
        ++ " SET last_modseqvalzer = :modseqvalue"
        ++ " WHERE remote_mailbox_id = :remote_mailbox_id",
    with_stmt(update_remote_mailbox_modseqvalue_2, Db, Stmt, [
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId),
        name(":modseqvalue") - bind_value(ModSeqValue)
    ], Res, !IO).

:- pred update_remote_mailbox_modseqvalue_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

update_remote_mailbox_modseqvalue_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_pairing_by_remote_message(Db, RemoteMailbox, UID, MessageId, Res, !IO)
        :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxRowId),
    Stmt = "SELECT pairing_id FROM pairing"
        ++ " WHERE remote_mailbox_id = :remote_mailbox_id"
        ++ "   AND remote_uid = :remote_uid"
        ++ "   AND message_id = :message_id",
    with_stmt(search_pairing_by_remote_message_2, Db, Stmt, [
        name(":remote_mailbox_id") - bind_value(RemoteMailboxRowId),
        name(":remote_uid") - bind_value(UID),
        name(":message_id") - bind_value(MessageId)
    ], Res, !IO).

:- pred search_pairing_by_remote_message_2(db(rw)::in, stmt::in,
    maybe_error(maybe(pairing_id))::out, io::di, io::uo) is det.

search_pairing_by_remote_message_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_int(Stmt, column(0), Id, !IO),
        Res = ok(yes(pairing_id(Id)))
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

insert_new_pairing_only_remote_message(Db, MessageId, LocalMailbox,
        RemoteMailbox, UID, Flags, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    Stmt = "INSERT OR FAIL INTO pairing(message_id,"
        ++ "    local_mailbox_id, local_uniquename, local_flags,"
        ++ "    remote_mailbox_id, remote_uid, remote_flags)"
        ++ " VALUES(:message_id,"
        ++ "    :local_mailbox_id, NULL, '',"
        ++ "    :remote_mailbox_id, :remote_uid, :remote_flags)",
    with_stmt(insert_new_remote_message_2, Db, Stmt, [
        name(":message_id") - bind_value(MessageId),
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId),
        name(":remote_uid") - bind_value(UID),
        name(":remote_flags") - bind_value(flags_to_string(Flags))
    ], Res, !IO).

:- pred insert_new_remote_message_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_new_remote_message_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

set_pairing_local_message(Db, PairingId, UniqueName, Flags, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_uniquename = :local_uniquename,"
        ++ "     local_flags = :local_flags"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(set_pairing_local_message_2, Db, Stmt, [
        name(":local_uniquename") - bind_value(UniqueName),
        name(":local_flags") - bind_value(flags_to_string(Flags)),
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

:- pred set_pairing_local_message_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

set_pairing_local_message_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

update_remote_message_flags(Db, PairingId, Flags, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_flags = :remote_flags"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(update_remote_message_flags_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":remote_flags") - bind_value(flags_to_string(Flags))
    ], Res, !IO).

:- pred update_remote_message_flags_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

update_remote_message_flags_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        Res = error("unexpected row")
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

:- func flags_to_string(list(flag)) = string.

flags_to_string(Flags) =
    string.join_list(" ", list.map(flag_to_string, Flags)).

:- func flag_to_string(flag) = string.

flag_to_string(system(Flag)) = system_flag_to_string(Flag).
flag_to_string(keyword(Atom)) = atom_to_string(Atom).

:- func system_flag_to_string(system_flag) = string.

system_flag_to_string(answered) = "\\Answered".
system_flag_to_string(flagged) = "\\Flagged".
system_flag_to_string(deleted) = "\\Deleted".
system_flag_to_string(seen) = "\\Seen".
system_flag_to_string(draft) = "\\Draft".
system_flag_to_string(extension(Atom)) = "\\" ++ atom_to_string(Atom).

:- func atom_to_string(atom) = string.

atom_to_string(atom(S)) = S. % will be uppercase

%-----------------------------------------------------------------------------%

search_pairings_without_local_message(Db, RemoteMailbox, Res, !IO) :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    Stmt = "SELECT pairing_id, remote_uid FROM pairing"
        ++ " WHERE remote_mailbox_id = :remote_mailbox_id"
        ++ "   AND local_uniquename IS NULL",
    with_stmt(search_pairings_without_local_message_2, Db, Stmt, [
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ], Res, !IO).

:- pred search_pairings_without_local_message_2(db(rw)::in, stmt::in,
    maybe_error(assoc_list(pairing_id, uid))::out, io::di, io::uo) is det.

search_pairings_without_local_message_2(Db, Stmt, Res, !IO) :-
    search_pairings_without_local_message_3(Db, Stmt, Res0, [], UnpairedUIDs,
        !IO),
    (
        Res0 = ok,
        Res = ok(UnpairedUIDs)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred search_pairings_without_local_message_3(db(rw)::in, stmt::in,
    maybe_error::out, assoc_list(pairing_id, uid)::in,
    assoc_list(pairing_id, uid)::out, io::di, io::uo) is det.

search_pairings_without_local_message_3(Db, Stmt, Res, !UnpairedUIDs, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        column_int(Stmt, column(0), PairingId, !IO),
        column_text(Stmt, column(1), UIDText, !IO),
        ( integer.from_string(UIDText) = UIDInteger ->
            cons(pairing_id(PairingId) - uid(UIDInteger), !UnpairedUIDs),
            search_pairings_without_local_message_3(Db, Stmt, Res,
                !UnpairedUIDs, !IO)
        ;
            Res = error("bad UID")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
