%-----------------------------------------------------------------------------%

:- module database.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

:- import_module flag_delta.
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

:- func get_local_mailbox_path(local_mailbox) = local_mailbox_path.

:- func get_remote_mailbox_name(remote_mailbox) = imap.types.mailbox.

:- func get_remote_mailbox_uidvalidity(remote_mailbox) = imap.types.uidvalidity.

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

:- type lookup_remote_mailbox_result
    --->    found(remote_mailbox, mod_seq_valzer)
    ;       error(string).

    % Lookup the remote_mailbox in the database with the given mailbox name
    % and uidvalidity value.
    %
:- pred lookup_remote_mailbox(database::in, mailbox::in, uidvalidity::in,
    lookup_remote_mailbox_result::out, io::di, io::uo) is det.

:- pred update_remote_mailbox_modseqvalue(database::in, remote_mailbox::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type pairing_id.

:- pred search_pairing_by_local_message(database::in, local_mailbox::in,
    uniquename::in,
    maybe_error(maybe({pairing_id, flag_deltas(local_mailbox)}))::out,
    io::di, io::uo) is det.

:- pred search_pairing_by_remote_message(database::in, remote_mailbox::in,
    uid::in, maybe_message_id::in,
    maybe_error(maybe({pairing_id, flag_deltas(remote_mailbox)}))::out,
    io::di, io::uo) is det.

:- pred insert_new_pairing_only_local_message(database::in,
    maybe_message_id::in, local_mailbox::in, remote_mailbox::in,
    uniquename::in, set(flag)::in, maybe_error::out, io::di, io::uo) is det.

:- pred insert_new_pairing_only_remote_message(database::in,
    maybe_message_id::in, local_mailbox::in, remote_mailbox::in, uid::in,
    set(flag)::in, maybe_error::out, io::di, io::uo) is det.

:- pred delete_pairing(database::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred set_pairing_local_message(database::in, pairing_id::in,
    uniquename::in, flag_deltas(local_mailbox)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred set_pairing_remote_message(database::in, pairing_id::in,
    uid::in, flag_deltas(remote_mailbox)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred reset_pairing_local_message(database::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred reset_pairing_remote_message(database::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred lookup_local_message_flags(database::in, pairing_id::in,
    maybe_error(flag_deltas(local_mailbox))::out, io::di, io::uo) is det.

:- pred update_local_message_flags(database::in, pairing_id::in,
    flag_deltas(local_mailbox)::in, bool::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred update_remote_message_flags(database::in, pairing_id::in,
    flag_deltas(remote_mailbox)::in, bool::in, maybe_error::out,
    io::di, io::uo) is det.

:- type unpaired_remote_message
    --->    unpaired_remote_message(
                pairing_id,
                uid,
                maybe_message_id
            ).

:- pred search_unpaired_remote_messages(database::in, remote_mailbox::in,
    maybe_error(list(unpaired_remote_message))::out, io::di, io::uo) is det.

:- type unpaired_local_message
    --->    unpaired_local_message(
                pairing_id,
                uniquename
            ).

:- pred search_unpaired_local_messages(database::in, local_mailbox::in,
    maybe_error(list(unpaired_local_message))::out, io::di, io::uo) is det.

:- pred search_unpaired_local_messages_by_message_id(database::in,
    local_mailbox::in, maybe_message_id::in,
    maybe_error(list(unpaired_local_message))::out, io::di, io::uo) is det.

:- type pending_flag_deltas
    --->    pending_flag_deltas(
                pairing_id,
                maybe(uniquename),
                flag_deltas(local_mailbox),
                expunged(local_mailbox),
                maybe(uid),
                flag_deltas(remote_mailbox),
                expunged(remote_mailbox)
            ).

:- pred search_pending_flag_deltas_from_local(database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error(list(pending_flag_deltas))::out,
    io::di, io::uo) is det.

:- pred search_pending_flag_deltas_from_remote(database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error(list(pending_flag_deltas))::out,
    io::di, io::uo) is det.

:- pred record_remote_flag_deltas_applied_to_local(database::in,
    pairing_id::in, flag_deltas(local_mailbox)::in,
    flag_deltas(remote_mailbox)::in, maybe_error::out, io::di, io::uo) is det.

:- pred record_local_flag_deltas_applied_to_remote(database::in,
    pairing_id::in, flag_deltas(local_mailbox)::in,
    flag_deltas(remote_mailbox)::in, maybe_error::out, io::di, io::uo) is det.

:- pred record_remote_flag_deltas_inapplicable_to_local(database::in,
    pairing_id::in, flag_deltas(remote_mailbox)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred record_local_flag_deltas_inapplicable_to_remote(database::in,
    pairing_id::in, flag_deltas(local_mailbox)::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred create_detect_local_expunge_temp_table(database::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred drop_detect_local_expunge_temp_table(database::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred insert_into_detect_local_expunge_table(database::in, uniquename::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred mark_expunged_local_messages(database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error(int)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred create_detect_remote_expunge_temp_table(database::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred drop_detect_remote_expunge_temp_table(database::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred insert_into_detect_remote_expunge_table(database::in, uid::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred mark_expunged_remote_messages(database::in, local_mailbox::in,
    remote_mailbox::in, maybe_error(int)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module sqlite3.

%-----------------------------------------------------------------------------%

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

:- instance bind_value(bool) where [
    bind_value(no) = int(0),
    bind_value(yes) = int(1)
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

:- instance bind_value(maybe_message_id) where [
    % Message-Id values have a form that can never be confused with "NIL".
    bind_value(nil) = bind_value("NIL"),
    bind_value(message_id(S)) = bind_value(S)
].

:- instance bind_value(local_mailbox_path) where [
    bind_value(local_mailbox_path(S)) = bind_value(S)
].

:- instance bind_value(uniquename) where [
    bind_value(uniquename(S)) = bind_value(S)
].

:- instance bind_value(flag_deltas(S)) where [
    bind_value(FlagDeltas) = bind_value(to_string(FlagDeltas))
].

%-----------------------------------------------------------------------------%

:- typeclass convert(T, U) where [
    pred convert(T::in, U::out) is semidet
].

:- instance convert(maybe(T), maybe(U)) <= convert(T, U) where [
    convert(no, no),
    convert(yes(X), yes(Y)) :- convert(X, Y)
].

:- instance convert(int, local_mailbox_id) where [
    convert(I, local_mailbox_id(I)) :- I > 0
].

:- instance convert(int, remote_mailbox_id) where [
    convert(I, remote_mailbox_id(I)) :- I > 0
].

:- instance convert(int, pairing_id) where [
    convert(I, pairing_id(I)) :- I > 0
].

:- instance convert(string, uid) where [
    convert(S, uid(Integer)) :-
    (
        Integer = integer.from_string(S),
        Integer > zero
    )
].

:- instance convert(string, mod_seq_value) where [
    convert(S, mod_seq_value(Integer)) :-
    (
        Integer = integer.from_string(S),
        Integer > zero
    )
].

:- instance convert(string, mod_seq_valzer) where [
    convert(S, mod_seq_valzer(Integer)) :-
    (
        Integer = integer.from_string(S),
        Integer >= zero
    )
].

:- instance convert(string, uniquename) where [
    convert(S, uniquename(S))
].

:- instance convert(string, maybe_message_id) where [
    convert(S, MessageId) :-
        ( S = "NIL" ->
            MessageId = nil
        ;
            MessageId = message_id(S)
        )
].

:- instance convert(string, flag_deltas(S)) where [
    convert(S, FlagDeltas) :-
        from_string(S, FlagDeltas)
].

:- instance convert(int, expunged(S)) where [
    convert(0, exists),
    convert(1, expunged)
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
        local_expunged      INTEGER NOT NULL, /* boolean */
        local_flags         TEXT NOT NULL,
        local_flags_attn    INTEGER NOT NULL,

        remote_mailbox_id   INTEGER NOT NULL
                            REFERENCES remote_mailbox(remote_mailbox_id),
        remote_uid          INTEGER, /* may be NULL */
        remote_expunged     INTEGER NOT NULL, /* boolean */
        remote_flags        TEXT NOT NULL,
        remote_flags_attn   INTEGER NOT NULL, /* boolean */

        UNIQUE(local_mailbox_id, local_uniquename),
        UNIQUE(remote_mailbox_id, remote_uid)
    );

    CREATE INDEX IF NOT EXISTS message_id_index ON pairing(message_id);
    CREATE INDEX IF NOT EXISTS local_attn_index ON pairing(local_flags_attn);
    CREATE INDEX IF NOT EXISTS remote_attn_index ON pairing(remote_flags_attn);
".

%   name,expunged   state
%   NULL,0          never existed
%   TEXT,0          exists
%   NULL,1          expunged

%-----------------------------------------------------------------------------%

close_database(Db, !IO) :-
    close(Db, !IO).

%-----------------------------------------------------------------------------%

get_local_mailbox_path(local_mailbox(LocalMailboxPath, _)) = LocalMailboxPath.

get_remote_mailbox_name(remote_mailbox(Mailbox, _, _)) = Mailbox.

get_remote_mailbox_uidvalidity(remote_mailbox(_, UIDValidity, _))
    = UIDValidity.

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
        column_int(Stmt, column(0), X0, !IO),
        ( convert(X0, MailboxId) ->
            LocalMailbox = local_mailbox(Path, MailboxId),
            Res = ok(LocalMailbox)
        ;
            Res = error("database error")
        )
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
        column_int(Stmt, column(0), X0, !IO),
        column_text(Stmt, column(1), X1, !IO),
        (
            convert(X0, MailboxId),
            convert(X1, ModSeqValzer)
        ->
            RemoteMailbox = remote_mailbox(Mailbox, UIDValidity, MailboxId),
            Res = found(RemoteMailbox, ModSeqValzer)
        ;
            Res = error("database error")
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

search_pairing_by_local_message(Db, LocalMailbox, Unique, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    Stmt = "SELECT pairing_id, local_flags FROM pairing"
        ++ " WHERE local_mailbox_id = :local_mailbox_id"
        ++ "   AND local_uniquename = :local_uniquename",
    with_stmt(search_pairing_by_local_message_2, Db, Stmt, [
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":local_uniquename") - bind_value(Unique)
    ], Res, !IO).

:- pred search_pairing_by_local_message_2(db(rw)::in, stmt::in,
    maybe_error(maybe({pairing_id, flag_deltas(local_mailbox)}))::out,
    io::di, io::uo) is det.

search_pairing_by_local_message_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        column_text(Stmt, column(1), X1, !IO),
        (
            convert(X0, PairingId),
            convert(X1, FlagDeltas)
        ->
            Res = ok(yes({PairingId, FlagDeltas}))
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_pairing_by_remote_message(Db, RemoteMailbox, UID, MessageId, Res, !IO)
        :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxRowId),
    Stmt = "SELECT pairing_id, remote_flags FROM pairing"
        ++ " WHERE remote_mailbox_id = :remote_mailbox_id"
        ++ "   AND remote_uid = :remote_uid"
        ++ "   AND message_id = :message_id",
    with_stmt(search_pairing_by_remote_message_2, Db, Stmt, [
        name(":remote_mailbox_id") - bind_value(RemoteMailboxRowId),
        name(":remote_uid") - bind_value(UID),
        name(":message_id") - bind_value(MessageId)
    ], Res, !IO).

:- pred search_pairing_by_remote_message_2(db(rw)::in, stmt::in,
    maybe_error(maybe({pairing_id, flag_deltas(remote_mailbox)}))::out,
    io::di, io::uo) is det.

search_pairing_by_remote_message_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        column_text(Stmt, column(1), X1, !IO),
        (
            convert(X0, PairingId),
            convert(X1, FlagDeltas)
        ->
            Res = ok(yes({PairingId, FlagDeltas}))
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

insert_new_pairing_only_local_message(Db, MessageId, LocalMailbox,
        RemoteMailbox, Unique, Flags, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    LocalFlagDeltas = init_flags(Flags) : flag_deltas(local_mailbox),

    Stmt = "INSERT OR FAIL INTO pairing(message_id,"
        ++ "    local_mailbox_id, local_uniquename, local_expunged,"
        ++ "    local_flags, local_flags_attn,"
        ++ "    remote_mailbox_id, remote_uid, remote_expunged,"
        ++ "    remote_flags, remote_flags_attn)"
        ++ " VALUES(:message_id,"
        ++ "    :local_mailbox_id, :local_uniquename, 0, :local_flags, 0,"
        ++ "    :remote_mailbox_id, NULL, 0, '', 0);",
    with_stmt(insert_new_local_message_2, Db, Stmt, [
        name(":message_id") - bind_value(MessageId),
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":local_uniquename") - bind_value(Unique),
        name(":local_flags") - bind_value(LocalFlagDeltas),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ], Res, !IO).

:- pred insert_new_local_message_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_new_local_message_2(Db, Stmt, Res, !IO) :-
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

insert_new_pairing_only_remote_message(Db, MessageId, LocalMailbox,
        RemoteMailbox, UID, Flags, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    RemoteFlagDeltas = init_flags(Flags) : flag_deltas(remote_mailbox),

    Stmt = "INSERT OR FAIL INTO pairing(message_id,"
        ++ "    local_mailbox_id, local_uniquename, local_expunged,"
        ++ "    local_flags, local_flags_attn,"
        ++ "    remote_mailbox_id, remote_uid, remote_expunged,"
        ++ "    remote_flags, remote_flags_attn)"
        ++ " VALUES(:message_id,"
        ++ "    :local_mailbox_id, NULL, 0, '', 0,"
        ++ "    :remote_mailbox_id, :remote_uid, 0, :remote_flags, 0);",
    with_stmt(insert_new_remote_message_2, Db, Stmt, [
        name(":message_id") - bind_value(MessageId),
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId),
        name(":remote_uid") - bind_value(UID),
        name(":remote_flags") - bind_value(RemoteFlagDeltas)
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

delete_pairing(Db, PairingId, Res, !IO) :-
    Stmt = "DELETE FROM pairing"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(delete_pairing_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

:- pred delete_pairing_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

delete_pairing_2(Db, Stmt, Res, !IO) :-
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

set_pairing_local_message(Db, PairingId, UniqueName, FlagDeltas, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_uniquename = :local_uniquename,"
        ++ "     local_expunged = 0,"
        ++ "     local_flags = :local_flags"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(set_pairing_local_message_2, Db, Stmt, [
        name(":local_uniquename") - bind_value(UniqueName),
        name(":local_flags") - bind_value(FlagDeltas),
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

set_pairing_remote_message(Db, PairingId, UID, FlagDeltas, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_uid = :remote_uid,"
        ++ "     remote_expunged = 0,"
        ++ "     remote_flags = :remote_flags"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(set_pairing_remote_message_2, Db, Stmt, [
        name(":remote_uid") - bind_value(UID),
        name(":remote_flags") - bind_value(FlagDeltas),
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

:- pred set_pairing_remote_message_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

set_pairing_remote_message_2(Db, Stmt, Res, !IO) :-
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

reset_pairing_local_message(Db, PairingId, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_uniquename = NULL,"
        ++ "     local_expunged = 0,"
        ++ "     local_flags = '',"
        ++ "     local_flags_attn = 0"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(reset_pairing_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

reset_pairing_remote_message(Db, PairingId, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_uid = NULL,"
        ++ "     remote_expunged = 0,"
        ++ "     remote_flags = '',"
        ++ "     remote_flags_attn = 0"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(reset_pairing_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

:- pred reset_pairing_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

reset_pairing_2(Db, Stmt, Res, !IO) :-
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

lookup_local_message_flags(Db, PairingId, Res, !IO) :-
    Stmt = "SELECT local_flags FROM pairing"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(lookup_local_message_flags_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

:- pred lookup_local_message_flags_2(db(rw)::in, stmt::in,
    maybe_error(flag_deltas(local_mailbox))::out, io::di, io::uo) is det.

lookup_local_message_flags_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("pairing not found")
    ;
        StepResult = row,
        column_text(Stmt, column(0), X0, !IO),
        ( convert(X0, Flags) ->
            Res = ok(Flags)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

update_local_message_flags(Db, PairingId, Flags, Attn, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_flags = :local_flags,"
        ++ "     local_flags_attn = :local_flags_attn"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(update_local_message_flags_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":local_flags") - bind_value(Flags),
        name(":local_flags_attn") - bind_value(Attn)
    ], Res, !IO).

:- pred update_local_message_flags_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

update_local_message_flags_2(Db, Stmt, Res, !IO) :-
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

update_remote_message_flags(Db, PairingId, Flags, Attn, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_flags = :remote_flags,"
        ++ "     remote_flags_attn = :remote_flags_attn"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(update_remote_message_flags_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":remote_flags") - bind_value(Flags),
        name(":remote_flags_attn") - bind_value(Attn)
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

%-----------------------------------------------------------------------------%

search_unpaired_remote_messages(Db, RemoteMailbox, Res, !IO) :-
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    Stmt = "SELECT pairing_id, remote_uid, message_id FROM pairing"
        ++ " WHERE remote_mailbox_id = :remote_mailbox_id"
        ++ "   AND remote_uid IS NOT NULL"
        ++ "   AND NOT remote_expunged"
        ++ "   AND local_uniquename IS NULL"
        ++ "   AND NOT local_expunged",
    with_stmt(search_unpaired_remote_messages_2, Db, Stmt, [
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ], Res, !IO).

:- pred search_unpaired_remote_messages_2(db(rw)::in, stmt::in,
    maybe_error(list(unpaired_remote_message))::out, io::di, io::uo) is det.

search_unpaired_remote_messages_2(Db, Stmt, Res, !IO) :-
    search_unpaired_remote_messages_3(Db, Stmt, Res0, [], Unpaired, !IO),
    (
        Res0 = ok,
        Res = ok(Unpaired)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred search_unpaired_remote_messages_3(db(rw)::in, stmt::in,
    maybe_error::out, list(unpaired_remote_message)::in,
    list(unpaired_remote_message)::out, io::di, io::uo) is det.

search_unpaired_remote_messages_3(Db, Stmt, Res, !Unpaired, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        column_text(Stmt, column(1), X1, !IO),
        column_text(Stmt, column(2), X2, !IO),
        (
            convert(X0, PairingId),
            convert(X1, UID),
            convert(X2, MessageId)
        ->
            Unpaired = unpaired_remote_message(PairingId, UID, MessageId),
            cons(Unpaired, !Unpaired),
            search_unpaired_remote_messages_3(Db, Stmt, Res, !Unpaired, !IO)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_unpaired_local_messages(Db, LocalMailbox, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    Stmt = "SELECT pairing_id, local_uniquename FROM pairing"
        ++ " WHERE local_mailbox_id = :local_mailbox_id"
        ++ "   AND local_uniquename IS NOT NULL"
        ++ "   AND NOT local_expunged"
        ++ "   AND remote_uid IS NULL"
        ++ "   AND NOT remote_expunged",
    with_stmt(search_unpaired_local_messages_2, Db, Stmt, [
        name(":local_mailbox_id") - bind_value(LocalMailboxId)
    ], Res, !IO).

search_unpaired_local_messages_by_message_id(Db, LocalMailbox, MessageId, Res,
        !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    Stmt = "SELECT pairing_id, local_uniquename FROM pairing"
        ++ " WHERE local_mailbox_id = :local_mailbox_id"
        ++ "   AND local_uniquename IS NOT NULL"
        ++ "   AND NOT local_expunged"
        ++ "   AND remote_uid IS NULL"
        ++ "   AND message_id = :message_id",
    with_stmt(search_unpaired_local_messages_2, Db, Stmt, [
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":message_id") - bind_value(MessageId)
    ], Res, !IO).

:- pred search_unpaired_local_messages_2(db(rw)::in, stmt::in,
    maybe_error(list(unpaired_local_message))::out, io::di, io::uo) is det.

search_unpaired_local_messages_2(Db, Stmt, Res, !IO) :-
    search_unpaired_local_messages_3(Db, Stmt, Res0, [], Unpaired, !IO),
    (
        Res0 = ok,
        Res = ok(Unpaired)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred search_unpaired_local_messages_3(db(rw)::in, stmt::in,
    maybe_error::out, list(unpaired_local_message)::in,
    list(unpaired_local_message)::out, io::di, io::uo) is det.

search_unpaired_local_messages_3(Db, Stmt, Res, !Unpaired, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        column_text(Stmt, column(1), X1, !IO),
        (
            convert(X0, PairingId),
            convert(X1, Unique)
        ->
            Unpaired = unpaired_local_message(PairingId, Unique),
            cons(Unpaired, !Unpaired),
            search_unpaired_local_messages_3(Db, Stmt, Res, !Unpaired, !IO)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_pending_flag_deltas_from_local(Db, LocalMailbox, RemoteMailbox, Res,
        !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    % A local expunged message may still change the flags of the remote
    % message, especially to set the \Deleted flag.
    %
    % If the remote message of a pairing is expunged but then the local message
    % is subsequently undeleted, the remote side of the pairing can be reset so
    % that the local message is re-paired.
    Stmt = "SELECT pairing_id, local_uniquename, local_flags, local_expunged,"
        ++ "       remote_uid, remote_flags, remote_expunged"
        ++ "  FROM pairing"
        ++ " WHERE local_mailbox_id = :local_mailbox_id"
        ++ "   AND remote_mailbox_id = :remote_mailbox_id"
        ++ "   AND local_flags_attn = 1",
    with_stmt(accum_pending_flag_deltas, Db, Stmt, [
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ], Res, !IO).

search_pending_flag_deltas_from_remote(Db, LocalMailbox, RemoteMailbox, Res,
        !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),
    % See above.
    Stmt = "SELECT pairing_id, local_uniquename, local_flags, local_expunged,"
        ++ "       remote_uid, remote_flags, remote_expunged"
        ++ "  FROM pairing"
        ++ " WHERE local_mailbox_id = :local_mailbox_id"
        ++ "   AND remote_mailbox_id = :remote_mailbox_id"
        ++ "   AND remote_flags_attn = 1",
    with_stmt(accum_pending_flag_deltas, Db, Stmt, [
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ], Res, !IO).

:- pred accum_pending_flag_deltas(db(rw)::in, stmt::in,
    maybe_error(list(pending_flag_deltas))::out, io::di, io::uo) is det.

accum_pending_flag_deltas(Db, Stmt, Res, !IO) :-
    accum_pending_flag_deltas_2(Db, Stmt, Res0, [], Pending, !IO),
    (
        Res0 = ok,
        Res = ok(Pending)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred accum_pending_flag_deltas_2(db(rw)::in, stmt::in,
    maybe_error::out, list(pending_flag_deltas)::in,
    list(pending_flag_deltas)::out, io::di, io::uo) is det.

accum_pending_flag_deltas_2(Db, Stmt, Res, !Pending, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        % local
        column_maybe_text(Stmt, column(1), X1, !IO),
        column_text(Stmt, column(2), X2, !IO),
        column_int(Stmt, column(3), X3, !IO),
        % remote
        column_maybe_text(Stmt, column(4), X4, !IO),
        column_text(Stmt, column(5), X5, !IO),
        column_int(Stmt, column(6), X6, !IO),
        (
            convert(X0, PairingId),
            convert(X1, MaybeUniqueName),
            convert(X2, LocalFlagDeltas),
            convert(X3, LocalExpunged),
            convert(X4, MaybeUID),
            convert(X5, RemoteFlagDeltas),
            convert(X6, RemoteExpunged)
        ->
            Pending = pending_flag_deltas(PairingId,
                MaybeUniqueName, LocalFlagDeltas, LocalExpunged,
                MaybeUID, RemoteFlagDeltas, RemoteExpunged),
            cons(Pending, !Pending),
            accum_pending_flag_deltas_2(Db, Stmt, Res, !Pending, !IO)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

record_remote_flag_deltas_applied_to_local(Db, PairingId, LocalFlags,
        RemoteFlags, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_flags = :local_flags,"
        ++ "     remote_flags = :remote_flags,"
        ++ "     remote_flags_attn = 0"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(record_flag_deltas_applied_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":local_flags") - bind_value(LocalFlags),
        name(":remote_flags") - bind_value(RemoteFlags)
    ], Res, !IO).

record_local_flag_deltas_applied_to_remote(Db, PairingId, LocalFlags,
        RemoteFlags, Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_flags = :local_flags,"
        ++ "     remote_flags = :remote_flags,"
        ++ "     local_flags_attn = 0"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(record_flag_deltas_applied_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":local_flags") - bind_value(LocalFlags),
        name(":remote_flags") - bind_value(RemoteFlags)
    ], Res, !IO).

record_remote_flag_deltas_inapplicable_to_local(Db, PairingId, RemoteFlags,
        Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_flags = :remote_flags,"
        ++ "     remote_flags_attn = 0"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(record_flag_deltas_applied_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":remote_flags") - bind_value(RemoteFlags)
    ], Res, !IO).

record_local_flag_deltas_inapplicable_to_remote(Db, PairingId, LocalFlags,
        Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_flags = :local_flags,"
        ++ "     local_flags_attn = 0"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(record_flag_deltas_applied_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":local_flags") - bind_value(LocalFlags)
    ], Res, !IO).

:- pred record_flag_deltas_applied_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

record_flag_deltas_applied_2(Db, Stmt, Res, !IO) :-
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

create_detect_local_expunge_temp_table(Db, Res, !IO) :-
    Stmt = "CREATE TEMP TABLE detect_local_expunge(local_uniquename NOT NULL)",
    exec(Db, Stmt, Res, !IO).

drop_detect_local_expunge_temp_table(Db, Res, !IO) :-
    Stmt = "DROP TABLE detect_local_expunge",
    exec(Db, Stmt, Res, !IO).

insert_into_detect_local_expunge_table(Db, UniqueName, Res, !IO) :-
    Stmt = "INSERT INTO detect_local_expunge VALUES(?1)",
    with_stmt(insert_into_detect_local_expunge_table_2, Db, Stmt, [
        num(1) - bind_value(UniqueName)
    ], Res, !IO).

:- pred insert_into_detect_local_expunge_table_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_into_detect_local_expunge_table_2(Db, Stmt, Res, !IO) :-
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

    % XXX maybe unmark messages which "reappear" as not expunged?
mark_expunged_local_messages(Db, LocalMailbox, RemoteMailbox, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),

    Where = 
       " WHERE local_mailbox_id = :local_mailbox_id
           AND remote_mailbox_id = :remote_mailbox_id
           AND NOT local_expunged
           AND local_uniquename IS NOT NULL
           AND local_uniquename NOT IN detect_local_expunge",
    Bindings = [
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ],

    StmtCount = "SELECT count(*) FROM pairing" ++ Where,
    with_stmt(count, Db, StmtCount, Bindings, ResCount, !IO),
    (
        ResCount = ok(Count),
        Count > 0
    ->
        StmtUpdate
            =  "UPDATE pairing SET"
            ++ " local_uniquename = NULL,"
            ++ " local_expunged = 1,"
            ++ " local_flags_attn = 1 "
            ++ Where,
        with_stmt(mark_expunged_local_messages_2, Db, StmtUpdate, Bindings,
            ResUpdate, !IO),
        (
            ResUpdate = ok,
            Res = ResCount
        ;
            ResUpdate = error(Error),
            Res = error(Error)
        )
    ;
        Res = ResCount
    ).

:- pred count(db(rw)::in, stmt::in, maybe_error(int)::out, io::di, io::uo)
    is det.

count(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("expected row")
    ;
        StepResult = row,
        column_int(Stmt, column(0), Count, !IO),
        Res = ok(Count)
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

:- pred mark_expunged_local_messages_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

mark_expunged_local_messages_2(Db, Stmt, Res, !IO) :-
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

create_detect_remote_expunge_temp_table(Db, Res, !IO) :-
    Stmt = "CREATE TEMP TABLE detect_remote_expunge(remote_uid NOT NULL)",
    exec(Db, Stmt, Res, !IO).

drop_detect_remote_expunge_temp_table(Db, Res, !IO) :-
    Stmt = "DROP TABLE detect_remote_expunge",
    exec(Db, Stmt, Res, !IO).

insert_into_detect_remote_expunge_table(Db, UID, Res, !IO) :-
    Stmt = "INSERT INTO detect_remote_expunge VALUES(?1)",
    with_stmt(insert_into_detect_remote_expunge_table_2, Db, Stmt, [
        num(1) - bind_value(UID)
    ], Res, !IO).

:- pred insert_into_detect_remote_expunge_table_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

insert_into_detect_remote_expunge_table_2(Db, Stmt, Res, !IO) :-
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

mark_expunged_remote_messages(Db, LocalMailbox, RemoteMailbox, Res, !IO) :-
    LocalMailbox = local_mailbox(_, LocalMailboxId),
    RemoteMailbox = remote_mailbox(_, _, RemoteMailboxId),

    Where = 
       " WHERE local_mailbox_id = :local_mailbox_id
           AND remote_mailbox_id = :remote_mailbox_id
           AND NOT remote_expunged
           AND remote_uid IS NOT NULL
           AND remote_uid NOT IN detect_remote_expunge",
    Bindings = [
        name(":local_mailbox_id") - bind_value(LocalMailboxId),
        name(":remote_mailbox_id") - bind_value(RemoteMailboxId)
    ],

    StmtCount = "SELECT count(*) FROM pairing" ++ Where,
    with_stmt(count, Db, StmtCount, Bindings, ResCount, !IO),
    (
        ResCount = ok(Count),
        Count > 0
    ->
        StmtUpdate
            = "UPDATE pairing SET"
            ++ " remote_uid = NULL,"
            ++ " remote_expunged = 1,"
            ++ " remote_flags_attn = 1"
            ++ Where,
        with_stmt(mark_expunged_remote_message_2, Db, StmtUpdate, Bindings,
            ResUpdate, !IO),
        (
            ResUpdate = ok,
            Res = ResCount
        ;
            ResUpdate = error(Error),
            Res = error(Error)
        )
    ;
        Res = ResCount
    ).

:- pred mark_expunged_remote_message_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

mark_expunged_remote_message_2(Db, Stmt, Res, !IO) :-
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
% vim: ft=mercury ts=4 sts=4 sw=4 et
