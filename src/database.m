%-----------------------------------------------------------------------------%

:- module database.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type database.

:- pred open_database(string::in, maybe_error(database)::out, io::di, io::uo)
    is det.

:- pred close_database(database::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type remote_mailbox
    --->    remote_mailbox(mailbox, uidvalidity, remote_mailbox_id).

:- type remote_mailbox_id.

:- type lookup_remote_mailbox_result
    --->    found(remote_mailbox, mod_seq_valzer)
    ;       error(string).

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

:- type remote_message_id.

:- pred search_remote_message(database::in, remote_mailbox::in, uid::in,
    message_id::in, maybe_error(maybe(remote_message_id))::out, io::di, io::uo)
    is det.

:- pred insert_new_remote_message(database::in, remote_mailbox::in, uid::in,
    message_id::in, list(flag)::in, maybe_error::out, io::di, io::uo) is det.

:- pred update_remote_message_flags(database::in, remote_message_id::in,
    list(flag)::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred insert_new_remote_pairing(database::in, remote_message_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred search_messages_without_local_pairing(database::in, remote_mailbox::in,
    maybe_error(list(uid))::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module integer.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module sqlite3.

:- type database == sqlite3.db(rw).

:- type remote_mailbox_id
    --->    remote_mailbox_id(int).

:- type remote_message_id
    --->    remote_message_id(int).

%-----------------------------------------------------------------------------%

:- typeclass bind_value(T) where [
    func bind_value(T) = bind_value
].

:- instance bind_value(remote_mailbox_id) where [
    bind_value(remote_mailbox_id(I)) = int(I)
].

:- instance bind_value(remote_message_id) where [
    bind_value(remote_message_id(I)) = int(I)
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

    CREATE TABLE IF NOT EXISTS message_ids(
        id                  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        message_id_string   TEXT UNIQUE NOT NULL
    );

    CREATE TABLE IF NOT EXISTS local_mailbox(
        id                  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        path                TEXT UNIQUE NOT NULL
    );

    CREATE TABLE IF NOT EXISTS local_message(
        id                  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        mailbox_id          INTEGER NOT NULL REFERENCES local_mailbox(id),
        basename            TEXT NOT NULL,
        message_id          INTEGER NOT NULL REFERENCES message_ids(id),
        flags               TEXT NOT NULL,
        UNIQUE(mailbox_id, basename)
    );

    CREATE TABLE IF NOT EXISTS remote_mailbox(
        id                  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        name                TEXT NOT NULL,
        uidvalidity         NUMBER NOT NULL,
        last_modseqvalzer   INTEGER NOT NULL, /* includes zero */
        UNIQUE(name, uidvalidity)
    );

    CREATE TABLE IF NOT EXISTS remote_message(
        id                  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        mailbox_id          INTEGER NOT NULL REFERENCES remote_mailbox(id),
        uid                 INTEGER NOT NULL,
        message_id          INTEGER NOT NULL REFERENCES message_ids(id),
        flags               TEXT NOT NULL,
        /* modseqvalue      INTEGER NOT NULL, */
        UNIQUE(mailbox_id, uid)
    );

    CREATE TABLE IF NOT EXISTS pairing(
        local_message_id    INTEGER REFERENCES local_message(id),
        remote_message_id   INTEGER REFERENCES remote_message(id),
        UNIQUE(local_message_id, remote_message_id)
    );
".

%-----------------------------------------------------------------------------%

close_database(Db, !IO) :-
    close(Db, !IO).

%-----------------------------------------------------------------------------%

insert_or_ignore_remote_mailbox(Db, Mailbox, UIDValidity, Res, !IO) :-
    Stmt = "INSERT OR IGNORE INTO"
        ++ " remote_mailbox(name, uidvalidity, last_modseqvalzer)"
        ++ " VALUES (:name, :uidvalidity, :last_modseqvalzer)",
    with_stmt(insert_or_ignore_remote_mailbox_2, Db, Stmt, [
        name(":name") - bind_value(Mailbox),
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
    Stmt = "SELECT id, last_modseqvalzer FROM remote_mailbox"
        ++ " WHERE name = :name"
        ++ " AND uidvalidity = :uidvalidity",
    with_stmt(lookup_remote_mailbox_2(Mailbox, UIDValidity), Db, Stmt, [
        name(":name") - bind_value(Mailbox),
        name(":uidvalidity") - bind_value(UIDValidity)
    ], Res, !IO).

:- pred lookup_remote_mailbox_2(mailbox::in, uidvalidity::in,
    db(rw)::in, stmt::in, lookup_remote_mailbox_result::out, io::di, io::uo)
    is det.

lookup_remote_mailbox_2(Mailbox, UIDValidity, Db, Stmt, Res, !IO) :-
    step_ok(Db, Stmt, StepResult, !IO),
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
    ).

%-----------------------------------------------------------------------------%

update_remote_mailbox_modseqvalue(Db, RemoteMailbox, ModSeqValue, Res, !IO) :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxRowId),
    Stmt = "UPDATE remote_mailbox"
        ++ " SET last_modseqvalzer = :modseqvalue WHERE id = :mailbox_id",
    with_stmt(update_remote_mailbox_modseqvalue_2, Db, Stmt, [
        name(":mailbox_id") - bind_value(RemoteMailboxRowId),
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

:- pred insert_or_ignore_message_id(database::in, message_id::in,
    maybe_error::out, io::di, io::uo) is det.

insert_or_ignore_message_id(Db, MessageId, Res, !IO) :-
    Stmt = "INSERT OR IGNORE INTO message_ids(message_id_string) VALUES(?1)",
    with_stmt(insert_or_ignore_message_id_2, Db, Stmt, [
        num(1) - bind_value(MessageId)
    ], Res, !IO).

:- pred insert_or_ignore_message_id_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

insert_or_ignore_message_id_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        ( StepResult = done
        ; StepResult = row
        ),
        Res = ok
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_remote_message(Db, RemoteMailbox, UID, MessageId, Res, !IO) :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxRowId),
    Stmt = "SELECT id FROM remote_message"
        ++ " WHERE mailbox_id = :mailbox_id"
        ++ "   AND uid = :uid"
        ++ "   AND message_id = (SELECT id FROM message_ids"
        ++ "                     WHERE message_id_string = :message_id_string)",
    with_stmt(search_remote_message_2, Db, Stmt, [
        name(":mailbox_id") - bind_value(RemoteMailboxRowId),
        name(":uid") - bind_value(UID),
        name(":message_id_string") - bind_value(MessageId)
    ], Res, !IO).

:- pred search_remote_message_2(db(rw)::in, stmt::in,
    maybe_error(maybe(remote_message_id))::out, io::di, io::uo) is det.

search_remote_message_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_int(Stmt, column(0), Id, !IO),
        Res = ok(yes(remote_message_id(Id)))
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

insert_new_remote_message(Db, RemoteMailbox, UID, MessageId, Flags, Res, !IO)
        :-
    insert_or_ignore_message_id(Db, MessageId, Res0, !IO),
    (
        Res0 = ok,
        insert_new_remote_message_2(Db, RemoteMailbox, UID, MessageId,
            Flags, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred insert_new_remote_message_2(db(rw)::in, remote_mailbox::in, uid::in,
    message_id::in, list(flag)::in, maybe_error::out, io::di, io::uo) is det.

insert_new_remote_message_2(Db, RemoteMailbox, UID, MessageId, Flags, Res, !IO)
        :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxRowId),
    Stmt = "INSERT OR FAIL INTO"
        ++ " remote_message(mailbox_id, uid, flags, message_id)"
        ++ " VALUES(:mailbox_id, :uid, :flags,"
        ++ "    (SELECT id FROM message_ids"
        ++ "     WHERE message_id_string = :message_id_string ))",
    with_stmt(insert_new_remote_message_3, Db, Stmt, [
        name(":mailbox_id") - bind_value(RemoteMailboxRowId),
        name(":uid") - bind_value(UID),
        name(":flags") - bind_value(flags_to_string(Flags)),
        name(":message_id_string") - bind_value(MessageId)
    ], Res, !IO).

:- pred insert_new_remote_message_3(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_new_remote_message_3(Db, Stmt, Res, !IO) :-
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

update_remote_message_flags(Db, RemoteMessageId, Flags, Res, !IO) :-
    StmtB = "UPDATE remote_message SET flags = :flags"
        ++ " WHERE id = :id",
    with_stmt(update_remote_message_flags_2, Db, StmtB, [
        name(":id") - bind_value(RemoteMessageId),
        name(":flags") - bind_value(flags_to_string(Flags))
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

insert_new_remote_pairing(Db, RemoteMessageId, Res, !IO) :-
    Stmt = "INSERT OR FAIL INTO pairing(local_message_id, remote_message_id)"
        ++ " VALUES(NULL, ?1)",
    with_stmt(insert_new_remote_pairing_2, Db, Stmt, [
        num(1) - bind_value(RemoteMessageId)
    ], Res, !IO).

:- pred insert_new_remote_pairing_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_new_remote_pairing_2(Db, Stmt, Res, !IO) :-
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

search_messages_without_local_pairing(Db, RemoteMailbox, Res, !IO) :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    Stmt = "SELECT remote_message.uid FROM remote_message INNER JOIN pairing"
        ++ " WHERE remote_message.mailbox_id = :mailbox_id"
        ++ "   AND remote_message.id = pairing.remote_message_id"
        ++ "   AND pairing.local_message_id IS NULL",
    with_stmt(search_messages_without_local_pairing_2, Db, Stmt, [
        name(":mailbox_id") - bind_value(RemoteMailboxId)
    ], Res, !IO).

:- pred search_messages_without_local_pairing_2(db(rw)::in, stmt::in,
    maybe_error(list(uid))::out, io::di, io::uo) is det.

search_messages_without_local_pairing_2(Db, Stmt, Res, !IO) :-
    search_messages_without_local_pairing_3(Db, Stmt, Res0, [], UIDs, !IO),
    (
        Res0 = ok,
        Res = ok(UIDs)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred search_messages_without_local_pairing_3(db(rw)::in, stmt::in,
    maybe_error::out, list(uid)::in, list(uid)::out, io::di, io::uo) is det.

search_messages_without_local_pairing_3(Db, Stmt, Res, !UIDs, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        column_text(Stmt, column(0), UIDText, !IO),
        ( integer.from_string(UIDText) = Integer ->
            UID = uid(Integer),
            cons(UID, !UIDs),
            search_messages_without_local_pairing_3(Db, Stmt, Res, !UIDs, !IO)
        ;
            Res = error("bad UID")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
