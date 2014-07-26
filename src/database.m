%-----------------------------------------------------------------------------%

:- module database.
:- interface.

:- import_module array.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module univ.

:- import_module flag_delta.
:- import_module imap.
:- import_module imap.types.
:- import_module maildir.
:- import_module prog_config.

%-----------------------------------------------------------------------------%

:- type database.

:- pred open_database(string::in, maybe_error(database)::out, io::di, io::uo)
    is det.

:- pred close_database(database::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type transaction_result(T, U)
    --->    commit(T)
    ;       rollback(U)
    ;       rollback_exception(univ).

:- pred transaction(pred(transaction_result(T, U), io, io), database,
    maybe_error(transaction_result(T, U)), io, io).
:- mode transaction(pred(out, di, uo) is det, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- type mailbox_pair.

:- type remote_mailbox_name == imap.types.mailbox.

:- func get_local_mailbox_name(mailbox_pair) = local_mailbox_name.

:- func get_remote_mailbox_name(mailbox_pair) = remote_mailbox_name.

:- func get_remote_mailbox_uidvalidity(mailbox_pair) = imap.types.uidvalidity.

    % Insert a mailbox_pair row into the database.
    % If such a row already exists then do nothing.
    %
:- pred insert_or_ignore_mailbox_pair(database::in,
    local_mailbox_name::in, remote_mailbox_name::in, uidvalidity::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred lookup_mailbox_pair(database::in,
    local_mailbox_name::in, remote_mailbox_name::in, uidvalidity::in,
    maybe_error(mailbox_pair)::out, io::di, io::uo) is det.

:- pred lookup_remote_mailbox_modseqvalzer(database::in, mailbox_pair::in,
    maybe_error(mod_seq_valzer)::out, io::di, io::uo) is det.

:- pred update_remote_mailbox_modseqvalue(database::in, mailbox_pair::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type pairing_id.

:- pred search_pairing_by_local_uniquename(database::in, mailbox_pair::in,
    uniquename::in, maybe_error(maybe(pairing_id))::out, io::di, io::uo)
    is det.

:- type pairing_flag_deltas
    --->    pairing_flag_deltas(
                pairing_id,
                flag_deltas(local_mailbox)
            ).

:- pred fold_unexpunged_pairings_with_uniquename(
    pred(pairing_id, uniquename, flag_deltas(local_mailbox), A, A, B, B, C, C,
    io, io), database, mailbox_pair, maybe_error,
    A, A, B, B, C, C, io, io).
% :- mode fold_unexpunged_pairings_with_uniquename(
%     pred(in, in, in, in, out, in, out, in, out, di, uo) is det,
%     in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold_unexpunged_pairings_with_uniquename(
    pred(in, in, in, in, out, in, out, array_di, array_uo, di, uo) is det,
    in, in, out, in, out, in, out, array_di, array_uo, di, uo) is det.

:- pred search_pairing_by_remote_message(database::in,
    mailbox_pair::in, uid::in, maybe_message_id::in,
    maybe_error(maybe({pairing_id, flag_deltas(remote_mailbox)}))::out,
    io::di, io::uo) is det.

:- pred insert_new_pairing_only_local_message(database::in,
    mailbox_pair::in, maybe_message_id::in, uniquename::in, set(flag)::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred insert_new_pairing_only_remote_message(database::in,
    mailbox_pair::in, maybe_message_id::in, uid::in, set(flag)::in,
    mod_seq_value::in, maybe_error::out, io::di, io::uo) is det.

:- pred delete_pairing(database::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred set_pairing_local_message(database::in, pairing_id::in,
    uniquename::in, flag_deltas(local_mailbox)::in, bool::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred set_pairing_remote_message(database::in, pairing_id::in,
    uid::in, flag_deltas(remote_mailbox)::in, mod_seq_value::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred reset_pairing_local_message(database::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred reset_pairing_remote_message(database::in, pairing_id::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred lookup_local_message_flags(database::in, pairing_id::in,
    maybe_error(flag_deltas(local_mailbox))::out, io::di, io::uo) is det.

:- pred lookup_remote_message_flags(database::in, pairing_id::in,
    maybe_error(flag_deltas(remote_mailbox))::out, io::di, io::uo) is det.

:- pred update_local_message_flags(database::in, pairing_id::in,
    flag_deltas(local_mailbox)::in, bool::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred update_remote_message_flags(database::in, pairing_id::in,
    flag_deltas(remote_mailbox)::in, bool::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred update_remote_message_flags_modseq(database::in, pairing_id::in,
    flag_deltas(remote_mailbox)::in, bool::in, mod_seq_value::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred search_min_max_uid(database::in, mailbox_pair::in,
    maybe_error(maybe({uid, uid}))::out, io::di, io::uo) is det.

:- pred search_max_uid_more_recent_than(database::in, mailbox_pair::in,
    mod_seq_valzer::in, maybe_error(maybe(uid))::out, io::di, io::uo) is det.

:- pred search_min_modseq(database::in, mailbox_pair::in, uid::in,
    maybe_error(mod_seq_valzer)::out, io::di, io::uo) is det.

:- type unpaired_remote_message
    --->    unpaired_remote_message(
                pairing_id,
                uid,
                maybe_message_id
            ).

:- func get_uid(unpaired_remote_message) = uid.

:- pred search_unpaired_remote_messages(database::in, mailbox_pair::in,
    maybe_error(list(unpaired_remote_message))::out, io::di, io::uo) is det.

:- type unpaired_local_message
    --->    unpaired_local_message(
                pairing_id,
                uniquename
            ).

:- pred search_unpaired_local_messages(database::in, mailbox_pair::in,
    maybe_error(list(unpaired_local_message))::out, io::di, io::uo) is det.

:- pred search_unpaired_local_messages_by_message_id(database::in,
    mailbox_pair::in, maybe_message_id::in,
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

:- pred search_pending_flag_deltas_from_local(database::in, mailbox_pair::in,
    maybe_error(list(pending_flag_deltas))::out, io::di, io::uo) is det.

:- pred search_pending_flag_deltas_from_remote(database::in, mailbox_pair::in,
    maybe_error(list(pending_flag_deltas))::out, io::di, io::uo) is det.

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

:- type insert_into_detect_expunge_stmt.

:- pred begin_detect_expunge(database::in,
    maybe_error(insert_into_detect_expunge_stmt)::out, io::di, io::uo) is det.

:- pred end_detect_expunge(database::in, insert_into_detect_expunge_stmt::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred detect_expunge_insert_pairing_id(database::in,
    insert_into_detect_expunge_stmt::in, pairing_id::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred detect_expunge_insert_uid(database::in,
    insert_into_detect_expunge_stmt::in, uid::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred mark_expunged_local_messages(database::in, mailbox_pair::in,
    maybe_error(int)::out, io::di, io::uo) is det.

:- pred mark_expunged_remote_messages(database::in, mailbox_pair::in,
    maybe_error(int)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred delete_expunged_pairings(database::in, mailbox_pair::in,
    maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module maybe.
:- import_module pair.
:- import_module string.

:- import_module binary_string.
:- import_module sqlite3.

%-----------------------------------------------------------------------------%

:- type database == sqlite3.db(rw).

:- type mailbox_pair
    --->    mailbox_pair(
                mailbox_pair_id,
                local_mailbox_name,
                remote_mailbox_name,
                uidvalidity
            ).

:- type mailbox_pair_id
    --->    mailbox_pair_id(int).

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

:- instance bind_value(mailbox_pair_id) where [
    bind_value(mailbox_pair_id(I)) = int(I)
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

:- instance bind_value(mod_seq_valzer) where [
    bind_value(mod_seq_valzer(I)) = bind_value(I)
].

:- instance bind_value(string) where [
    bind_value(S) = text_by_reference(S)
].

:- instance bind_value(binary_string) where [
    bind_value(S) = blob_by_reference(unsafe_pointer(S), length(S))
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

:- instance bind_value(local_mailbox_name) where [
    bind_value(local_mailbox_name(S)) = bind_value(S)
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

:- instance convert(int, mailbox_pair_id) where [
    convert(I, mailbox_pair_id(I)) :- I > 0
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
    FollowSymlinks = yes,
    io.file_type(FollowSymlinks, FileName, ResFileType, !IO),
    (
        ResFileType = ok(FileType),
        (
            ( FileType = regular_file
            ; FileType = unknown
            )
        ->
            open_existing_database(FileName, Res, !IO)
        ;
            Res = error("unxpected file type for database path")
        )
    ;
        % Assume the database does not exist.
        % XXX better we had a io.file_exists predicate
        ResFileType = error(_),
        open_new_database(FileName, Res, !IO)
    ).

:- pred open_new_database(string::in, maybe_error(database)::out,
    io::di, io::uo) is det.

open_new_database(FileName, Res, !IO) :-
    open_database_2(FileName, misc_init ++ create_tables, ResOpen, !IO),
    (
        ResOpen = ok(Db),
        insert_database_version(Db, ResVersion, !IO),
        (
            ResVersion = ok,
            Res = ok(Db)
        ;
            ResVersion = error(Error),
            close_database(Db, !IO),
            Res = error(Error)
        )
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

:- pred open_existing_database(string::in, maybe_error(database)::out,
    io::di, io::uo) is det.

open_existing_database(FileName, Res, !IO) :-
    open_database_2(FileName, misc_init, ResOpen, !IO),
    (
        ResOpen = ok(Db),
        check_database_version(Db, ResVersion, !IO),
        (
            ResVersion = ok,
            Res = ok(Db)
        ;
            ResVersion = error(Error),
            close_database(Db, !IO),
            Res = error(Error)
        )
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

:- pred open_database_2(string::in, string::in, maybe_error(database)::out,
    io::di, io::uo) is det.

open_database_2(FileName, Sql, Res, !IO) :-
    open_rw(FileName, normal, ResOpen, !IO),
    (
        ResOpen = ok(Db),
        exec(Db, Sql, ExecRes, !IO),
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

:- func misc_init = string.

misc_init = "
    /* Foreign keys are off by default */
    PRAGMA foreign_keys = ON;

    ATTACH DATABASE ':memory:' AS mem;
".

:- func create_tables = string.

create_tables = "
    CREATE TABLE meta(
        key                 TEXT NOT NULL UNIQUE PRIMARY KEY,
        value               TEXT
    );

    CREATE TABLE mailbox_pair(
        mailbox_pair_id     INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        local_mailbox       TEXT NOT NULL,
        remote_mailbox      TEXT NOT NULL,
        uidvalidity         NUMBER NOT NULL,
        last_modseqvalzer   INTEGER NOT NULL, /* includes zero */
        UNIQUE(local_mailbox, remote_mailbox, uidvalidity)
    );

    CREATE TABLE pairing(
        pairing_id          INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
        mailbox_pair_id     INTEGER NOT NULL
                            REFERENCES mailbox_pair(mailbox_pair_id),
        message_id          TEXT NOT NULL,

        local_uniquename    TEXT, /* may be NULL */
        local_expunged      INTEGER NOT NULL, /* boolean */
        local_flags         TEXT NOT NULL,
        local_flags_attn    INTEGER NOT NULL,

        remote_uid          INTEGER, /* may be NULL */
        remote_expunged     INTEGER NOT NULL, /* boolean */
        remote_flags        TEXT NOT NULL,
        remote_flags_attn   INTEGER NOT NULL, /* boolean */
        remote_modseqvalzer INTEGER NOT NULL, /* includes zero */

        UNIQUE(mailbox_pair_id, local_uniquename, remote_uid)
    );

    CREATE INDEX message_id_index ON pairing(message_id);
    CREATE INDEX local_attn_index ON pairing(local_flags_attn);
    CREATE INDEX remote_attn_index ON pairing(remote_flags_attn);
".

%   name,expunged   state
%   NULL,0          never existed
%   TEXT,0          exists
%   NULL,1          expunged

%-----------------------------------------------------------------------------%

:- pred insert_database_version(database::in, maybe_error::out, io::di, io::uo)
    is det.

insert_database_version(Db, Res, !IO) :-
    Stmt = "INSERT INTO meta(key, value) VALUES('version', :version)",
    Bindings = [
        name(":version") - bind_value(current_database_version)
    ],
    with_stmt(insert_database_version_2, Db, Stmt, Bindings, Res, !IO).

:- pred insert_database_version_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

insert_database_version_2(Db, Stmt, Res, !IO) :-
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

:- pred check_database_version(database::in, maybe_error::out, io::di, io::uo)
    is det.

check_database_version(Db, Res, !IO) :-
    % Throws exception if table doesn't exist.
    Stmt = "SELECT VALUE FROM meta WHERE key = 'version'",
    promise_equivalent_solutions [ResVersion, !:IO]
    ( try [io(!IO)]
        with_stmt(get_database_version, Db, Stmt, [], ResVersionPrime, !IO)
    then
        ResVersion = ResVersionPrime
    catch_any Excp ->
        ResVersion = error(string(Excp))
    ),
    (
        ResVersion = ok(Version),
        ( Version = current_database_version ->
            Res = ok
        ;
            Res = error("unexpected database version: " ++ Version)
        )
    ;
        ResVersion = error(Error),
        Res = error(Error)
    ).

:- pred get_database_version(db(rw)::in, stmt::in, maybe_error(string)::out,
    io::di, io::uo) is det.

get_database_version(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("expected row")
    ;
        StepResult = row,
        column_text(Stmt, column(0), Version, !IO),
        Res = ok(Version)
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

:- func current_database_version = string.

current_database_version = "1:2014-07-12".

%-----------------------------------------------------------------------------%

close_database(Db, !IO) :-
    close(Db, !IO).

%-----------------------------------------------------------------------------%

transaction(Pred, Db, Res, !IO) :-
    begin_transaction(Db, Res0, !IO),
    (
        Res0 = ok,
        promise_equivalent_solutions [Out, !:IO]
        ( try [io(!IO)]
            Pred(OutPrime, !IO)
        then
            Out = OutPrime
        catch_any Excp ->
            Out = rollback_exception(univ(Excp))
        ),
        (
            Out = commit(_),
            end_transaction(Db, Res1, !IO),
            (
                Res1 = ok,
                Res = ok(Out)
            ;
                Res1 = error(Error),
                Res = error(Error)
            )
        ;
            ( Out = rollback(_)
            ; Out = rollback_exception(_)
            ),
            rollback_transaction(Db, Res2, !IO),
            (
                Res2 = ok,
                Res = ok(Out)
            ;
                Res2 = error(Error),
                Res = error(Error)
            )
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

get_local_mailbox_name(mailbox_pair(_, LocalMailbox, _, _)) = LocalMailbox.

get_remote_mailbox_name(mailbox_pair(_, _, Mailbox, _)) = Mailbox.

get_remote_mailbox_uidvalidity(mailbox_pair(_, _, _, UIDValidity))
    = UIDValidity.

%-----------------------------------------------------------------------------%

insert_or_ignore_mailbox_pair(Db, LocalMailboxName, RemoteMailboxName,
        UIDValidity, Res, !IO) :-
    Stmt =
        "INSERT OR IGNORE INTO mailbox_pair(
            local_mailbox, remote_mailbox, uidvalidity, last_modseqvalzer)
        VALUES (:local_mailbox, :remote_mailbox, :uidvalidity, 0)",
    with_stmt(insert_or_ignore_local_mailbox_2, Db, Stmt, [
        name(":local_mailbox") - bind_value(LocalMailboxName),
        name(":remote_mailbox") - bind_value(RemoteMailboxName),
        name(":uidvalidity") - bind_value(UIDValidity)
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

lookup_mailbox_pair(Db, LocalMailboxName, RemoteMailboxName, UIDValidity, Res,
        !IO) :-
    Stmt =
        "SELECT mailbox_pair_id FROM mailbox_pair
          WHERE local_mailbox = :local_mailbox
            AND remote_mailbox = :remote_mailbox
            AND uidvalidity = :uidvalidity",
    Bindings = [
        name(":local_mailbox") - bind_value(LocalMailboxName),
        name(":remote_mailbox") - bind_value(RemoteMailboxName),
        name(":uidvalidity") - bind_value(UIDValidity)
    ],
    with_stmt(
        lookup_mailbox_pair_2(LocalMailboxName, RemoteMailboxName,
            UIDValidity),
        Db, Stmt, Bindings, Res, !IO).

:- pred lookup_mailbox_pair_2(local_mailbox_name::in, remote_mailbox_name::in,
    uidvalidity::in, db(rw)::in, stmt::in, maybe_error(mailbox_pair)::out,
    io::di, io::uo) is det.

lookup_mailbox_pair_2(LocalMailboxName, RemoteMailboxName, UIDValidity,
        Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("mailbox_pair not found")
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        ( convert(X0, MailboxPairId) ->
            MailboxPair = mailbox_pair(MailboxPairId, LocalMailboxName,
                RemoteMailboxName, UIDValidity),
            Res = ok(MailboxPair)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

lookup_remote_mailbox_modseqvalzer(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt =
        "SELECT last_modseqvalzer FROM mailbox_pair
          WHERE mailbox_pair_id = :mailbox_pair_id",
    Bindings = [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
    ],
    with_stmt(
        lookup_remote_mailbox_modseqvalzer_2,
        Db, Stmt, Bindings, Res, !IO).

:- pred lookup_remote_mailbox_modseqvalzer_2(db(rw)::in, stmt::in,
    maybe_error(mod_seq_valzer)::out, io::di, io::uo) is det.

lookup_remote_mailbox_modseqvalzer_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("mailbox_pair not found")
    ;
        StepResult = row,
        column_text(Stmt, column(0), X1, !IO),
        ( convert(X1, ModSeqValzer) ->
            Res = ok(ModSeqValzer)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

update_remote_mailbox_modseqvalue(Db, MailboxPair, ModSeqValue, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "UPDATE mailbox_pair"
        ++ " SET last_modseqvalzer = :modseqvalue"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id",
    with_stmt(update_remote_mailbox_modseqvalue_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
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

search_pairing_by_local_uniquename(Database, MailboxPair, Unique, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "SELECT pairing_id, local_flags FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_uniquename = :local_uniquename",
    with_stmt(search_pairing_by_local_uniquename_2, Database, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
        name(":local_uniquename") - bind_value(Unique)
    ], Res, !IO).

:- pred search_pairing_by_local_uniquename_2(db(rw)::in, stmt::in,
    maybe_error(maybe(pairing_id))::out, io::di, io::uo) is det.

search_pairing_by_local_uniquename_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_int(Stmt, column(0), X0, !IO),
        ( convert(X0, PairingId) ->
            Res = ok(yes(PairingId))
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

fold_unexpunged_pairings_with_uniquename(Pred, Db, MailboxPair, Res,
        !A, !B, !C, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "SELECT pairing_id, local_uniquename, local_flags FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_uniquename IS NOT NULL"
        ++ "   AND NOT local_expunged",
    with_stmt_acc3(fold_unexpunged_pairings_with_uniquename_2(Pred), Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
    ], Res, !A, !B, !C, !IO).

:- pred fold_unexpunged_pairings_with_uniquename_2(
    pred(pairing_id, uniquename, flag_deltas(local_mailbox), A, A, B, B, C, C,
    io, io), database, stmt, maybe_error,
    A, A, B, B, C, C, io, io).
% :- mode fold_unexpunged_pairings_with_uniquename_2(
%     pred(in, in, in, in, out, in, out, in, out, di, uo) is det,
%     in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold_unexpunged_pairings_with_uniquename_2(
    pred(in, in, in, in, out, in, out, array_di, array_uo, di, uo) is det,
    in, in, out, in, out, in, out, array_di, array_uo, di, uo) is det.

fold_unexpunged_pairings_with_uniquename_2(Pred, Db, Stmt, Res, !A, !B, !C,
        !IO) :-
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
            convert(X1, Unique),
            convert(X2, LocalFlagDeltas)
        ->
            Pred(PairingId, Unique, LocalFlagDeltas, !A, !B, !C, !IO),
            fold_unexpunged_pairings_with_uniquename_2(Pred, Db, Stmt, Res,
                !A, !B, !C, !IO)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_pairing_by_remote_message(Db, MailboxPair, UID, MessageId, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "SELECT pairing_id, remote_flags FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND remote_uid = :remote_uid"
        ++ "   AND message_id = :message_id",
    with_stmt(search_pairing_by_remote_message_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
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

insert_new_pairing_only_local_message(Db, MailboxPair, MessageId, Unique,
        Flags, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    LocalFlagDeltas = init_flags(Flags) : flag_deltas(local_mailbox),

    Stmt = "INSERT OR FAIL INTO pairing(mailbox_pair_id, message_id,"
        ++ "    local_uniquename, local_expunged,"
        ++ "    local_flags, local_flags_attn,"
        ++ "    remote_uid, remote_expunged,"
        ++ "    remote_flags, remote_flags_attn, remote_modseqvalzer)"
        ++ " VALUES(:mailbox_pair_id, :message_id,"
        ++ "    :local_uniquename, 0, :local_flags, 0,"
        ++ "    NULL, 0, '', 0, 0);",
    with_stmt(insert_new_local_message_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
        name(":message_id") - bind_value(MessageId),
        name(":local_uniquename") - bind_value(Unique),
        name(":local_flags") - bind_value(LocalFlagDeltas)
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

insert_new_pairing_only_remote_message(Db, MailboxPair, MessageId, UID, Flags,
        ModSeqValue, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    RemoteFlagDeltas = init_flags(Flags) : flag_deltas(remote_mailbox),

    Stmt = "INSERT OR FAIL INTO pairing(mailbox_pair_id, message_id,"
        ++ "    local_uniquename, local_expunged,"
        ++ "    local_flags, local_flags_attn,"
        ++ "    remote_uid, remote_expunged,"
        ++ "    remote_flags, remote_flags_attn, remote_modseqvalzer)"
        ++ " VALUES(:mailbox_pair_id, :message_id,"
        ++ "    NULL, 0, '', 0,"
        ++ "    :remote_uid, 0, :remote_flags, 0, :remote_modseqvalzer);",
    with_stmt(insert_new_remote_message_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
        name(":message_id") - bind_value(MessageId),
        name(":remote_uid") - bind_value(UID),
        name(":remote_flags") - bind_value(RemoteFlagDeltas),
        name(":remote_modseqvalzer") - bind_value(ModSeqValue)
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

set_pairing_local_message(Db, PairingId, UniqueName, FlagDeltas, Attn, Res,
        !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET local_uniquename = :local_uniquename,"
        ++ "     local_expunged = 0,"
        ++ "     local_flags = :local_flags,"
        ++ "     local_flags_attn = :local_flags_attn"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(set_pairing_local_message_2, Db, Stmt, [
        name(":local_uniquename") - bind_value(UniqueName),
        name(":local_flags") - bind_value(FlagDeltas),
        name(":local_flags_attn") - bind_value(Attn),
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

set_pairing_remote_message(Db, PairingId, UID, FlagDeltas, ModSeqValue, Res,
        !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_uid = :remote_uid,"
        ++ "     remote_expunged = 0,"
        ++ "     remote_flags = :remote_flags,"
        ++ "     remote_modseqvalzer = :remote_modseqvalzer"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(set_pairing_remote_message_2, Db, Stmt, [
        name(":remote_uid") - bind_value(UID),
        name(":remote_flags") - bind_value(FlagDeltas),
        name(":remote_modseqvalzer") - bind_value(ModSeqValue),
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
        ++ "     remote_flags_attn = 0,"
        ++ "     remote_modseqvalzer = 0"
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

lookup_remote_message_flags(Db, PairingId, Res, !IO) :-
    Stmt = "SELECT remote_flags FROM pairing"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(lookup_remote_message_flags_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId)
    ], Res, !IO).

:- pred lookup_remote_message_flags_2(db(rw)::in, stmt::in,
    maybe_error(flag_deltas(remote_mailbox))::out, io::di, io::uo) is det.

lookup_remote_message_flags_2(Db, Stmt, Res, !IO) :-
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

update_remote_message_flags_modseq(Db, PairingId, Flags, Attn, ModSeqValue,
        Res, !IO) :-
    Stmt = "UPDATE pairing"
        ++ " SET remote_flags = :remote_flags,"
        ++ "     remote_flags_attn = :remote_flags_attn,"
        ++ "     remote_modseqvalzer = :remote_modseqvalzer"
        ++ " WHERE pairing_id = :pairing_id",
    with_stmt(update_remote_message_flags_2, Db, Stmt, [
        name(":pairing_id") - bind_value(PairingId),
        name(":remote_flags") - bind_value(Flags),
        name(":remote_flags_attn") - bind_value(Attn),
        name(":remote_modseqvalzer") - bind_value(ModSeqValue)
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

search_min_max_uid(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt =
        "SELECT MIN(remote_uid), MAX(remote_uid) FROM pairing
          WHERE mailbox_pair_id = :mailbox_pair_id",
    with_stmt(search_min_max_uid_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
    ], Res, !IO).

:- pred search_min_max_uid_2(db(rw)::in, stmt::in,
    maybe_error(maybe({uid, uid}))::out, io::di, io::uo) is det.

search_min_max_uid_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_text(Stmt, column(0), X0, !IO),
        column_text(Stmt, column(1), X1, !IO),
        (
            X0 = ""
        ->
            Res = ok(no)
        ;
            convert(X0, MinUID),
            convert(X1, MaxUID)
        ->
            Res = ok(yes({MinUID, MaxUID}))
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_max_uid_more_recent_than(Db, MailboxPair, ModSeqValzer, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt =
        "SELECT MAX(remote_uid) FROM pairing
          WHERE mailbox_pair_id = :mailbox_pair_id
            AND remote_modseqvalzer > :modseqvalzer",
    with_stmt(search_max_uid_more_recent_than, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
        name(":modseqvalzer") - bind_value(ModSeqValzer)
    ], Res, !IO).

:- pred search_max_uid_more_recent_than(db(rw)::in, stmt::in,
    maybe_error(maybe(uid))::out, io::di, io::uo) is det.

search_max_uid_more_recent_than(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = ok(no)
    ;
        StepResult = row,
        column_text(Stmt, column(0), X0, !IO),
        ( X0 = "" ->
            Res = ok(no)
        ; convert(X0, UID) ->
            Res = ok(yes(UID))
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

search_min_modseq(Db, MailboxPair, UID, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt =
        "SELECT MIN(remote_modseqvalzer) FROM pairing
          WHERE mailbox_pair_id = :mailbox_pair_id
            AND remote_uid <= :uid",
    with_stmt(search_min_modseq_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
        name(":uid") - bind_value(UID)
    ], Res, !IO).

:- pred search_min_modseq_2(db(rw)::in, stmt::in,
    maybe_error(mod_seq_valzer)::out, io::di, io::uo) is det.

search_min_modseq_2(Db, Stmt, Res, !IO) :-
    step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done,
        Res = error("expected row")
    ;
        StepResult = row,
        column_text(Stmt, column(0), X0, !IO),
        ( X0 = "" ->
            Res = ok(mod_seq_valzer(zero))
        ; convert(X0, ModSeqValzer) ->
            Res = ok(ModSeqValzer)
        ;
            Res = error("database error")
        )
    ;
        StepResult = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

get_uid(unpaired_remote_message(_, UID, _)) = UID.

search_unpaired_remote_messages(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "SELECT pairing_id, remote_uid, message_id FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_uniquename IS NULL"
        ++ "   AND NOT local_expunged"
        ++ "   AND remote_uid IS NOT NULL"
        ++ "   AND NOT remote_expunged",
    with_stmt(search_unpaired_remote_messages_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
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

search_unpaired_local_messages(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "SELECT pairing_id, local_uniquename FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_uniquename IS NOT NULL"
        ++ "   AND NOT local_expunged"
        ++ "   AND remote_uid IS NULL"
        ++ "   AND NOT remote_expunged",
    with_stmt(search_unpaired_local_messages_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
    ], Res, !IO).

search_unpaired_local_messages_by_message_id(Db, MailboxPair, MessageId, Res,
        !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "SELECT pairing_id, local_uniquename FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_uniquename IS NOT NULL"
        ++ "   AND NOT local_expunged"
        ++ "   AND remote_uid IS NULL"
        ++ "   AND message_id = :message_id",
    with_stmt(search_unpaired_local_messages_2, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId),
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

search_pending_flag_deltas_from_local(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    % A local expunged message may still change the flags of the remote
    % message, especially to set the \Deleted flag.
    %
    % If the remote message of a pairing is expunged but then the local message
    % is subsequently undeleted, the remote side of the pairing can be reset so
    % that the local message is re-paired.
    Stmt = "SELECT pairing_id, local_uniquename, local_flags, local_expunged,"
        ++ "       remote_uid, remote_flags, remote_expunged"
        ++ "  FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_flags_attn = 1",
    with_stmt(accum_pending_flag_deltas, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
    ], Res, !IO).

search_pending_flag_deltas_from_remote(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    % See above.
    Stmt = "SELECT pairing_id, local_uniquename, local_flags, local_expunged,"
        ++ "       remote_uid, remote_flags, remote_expunged"
        ++ "  FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND remote_flags_attn = 1",
    with_stmt(accum_pending_flag_deltas, Db, Stmt, [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
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

:- type insert_into_detect_expunge_stmt == stmt.

begin_detect_expunge(Db, Res, !IO) :-
    CreateStmt = "CREATE TABLE mem.detect_expunge(uniq NOT NULL)",
    exec(Db, CreateStmt, Res0, !IO),
    (
        Res0 = ok,
        prepare(Db, insert_into_detect_expunge_sql, Res1, !IO),
        (
            Res1 = ok(InsertStmt),
            Res = ok(InsertStmt)
        ;
            Res1 = error(Error),
            Res = error(Error),
            drop_detect_expunge(Db, _, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

end_detect_expunge(Db, InsertStmt, Res, !IO) :-
    sqlite3.finalize(InsertStmt, !IO),
    drop_detect_expunge(Db, Res, !IO).

:- pred drop_detect_expunge(database::in, maybe_error::out, io::di, io::uo)
    is det.

drop_detect_expunge(Db, Res, !IO) :-
    DropStmt = "DROP TABLE mem.detect_expunge",
    exec(Db, DropStmt, Res, !IO).

:- func insert_into_detect_expunge_sql = string.

insert_into_detect_expunge_sql = "INSERT INTO mem.detect_expunge VALUES(?1)".

detect_expunge_insert_pairing_id(Db, InsertStmt, PairingId, Res, !IO) :-
    with_prepared_stmt(detect_expunge_insert_2,
        Db, InsertStmt, [num(1) - bind_value(PairingId)], Res, !IO).

detect_expunge_insert_uid(Db, InsertStmt, UID, Res, !IO) :-
    with_prepared_stmt(detect_expunge_insert_2,
        Db, InsertStmt, [num(1) - bind_value(UID)], Res, !IO).

:- pred detect_expunge_insert_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

detect_expunge_insert_2(Db, Stmt, Res, !IO) :-
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

mark_expunged_local_messages(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Where = 
       " WHERE mailbox_pair_id = :mailbox_pair_id
           AND NOT local_expunged
           AND pairing_id IN mem.detect_expunge",
    Bindings = [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
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
        with_stmt(mark_expunged_2, Db, StmtUpdate, Bindings,
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

mark_expunged_remote_messages(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Where = 
       " WHERE mailbox_pair_id = :mailbox_pair_id
           AND NOT remote_expunged
           AND remote_uid IN mem.detect_expunge",
    Bindings = [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
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
        with_stmt(mark_expunged_2, Db, StmtUpdate, Bindings,
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

:- pred mark_expunged_2(db(rw)::in, stmt::in,
    maybe_error::out, io::di, io::uo) is det.

mark_expunged_2(Db, Stmt, Res, !IO) :-
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

delete_expunged_pairings(Db, MailboxPair, Res, !IO) :-
    MailboxPair = mailbox_pair(MailboxPairId, _, _, _),
    Stmt = "DELETE FROM pairing"
        ++ " WHERE mailbox_pair_id = :mailbox_pair_id"
        ++ "   AND local_expunged=1"
        ++ "   AND remote_expunged=1",
    Bindings = [
        name(":mailbox_pair_id") - bind_value(MailboxPairId)
    ],
    with_stmt(delete_expunged_pairings_2, Db, Stmt, Bindings, Res, !IO).

:- pred delete_expunged_pairings_2(db(rw)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

delete_expunged_pairings_2(Db, Stmt, Res, !IO) :-
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
