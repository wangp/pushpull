%-----------------------------------------------------------------------------%

:- module sqlite3.
:- interface.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module maybe.

% :- import_module int64.

%-----------------------------------------------------------------------------%

:- type rw ---> rw.
:- type ro ---> ro.

:- type db(RwRo).

:- type synchronous
    --->    off
    ;       normal
    ;       full.

:- type stmt.

:- type bind_index
    --->    num(int)
    ;       name(string).

:- type bind_value
    --->    null
    ;       int(int)
    %;      int64(int64)
    ;       text_by_reference(string)
    ;       blob_by_reference(c_pointer, int).

:- type step_result
    --->    done
    ;       row
    ;       error(string).

:- inst step_result_nonerror
    --->    done
    ;       row.

:- type column
    --->    column(int).

:- type sqlite_error % exception type
    --->    sqlite_error(string).

%-----------------------------------------------------------------------------%

:- pred init_multithreaded(maybe_error::out, io::di, io::uo) is det.

:- pred synchronous(synchronous, string).
:- mode synchronous(in, out) is det.
:- mode synchronous(out, in) is semidet.

:- pred open_rw(string::in, synchronous::in, maybe_error(db(rw))::out,
    io::di, io::uo) is det.

:- pred open_ro(string::in, maybe_error(db(ro))::out, io::di, io::uo) is det.

:- pred close(db(RwRo)::in, io::di, io::uo) is det.

    % This is only good for temporarily treating a rw database connection
    % as a ro database connection.  It should be avoided.
    %
:- pred rw_db_to_ro_db(db(rw)::in, db(ro)::out) is det.

%-----------------------------------------------------------------------------%

    % Must be paired with end_transaction or rollback_transaction.
    %
:- pred begin_transaction(db(RwRo)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred end_transaction(db(RwRo)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred rollback_transaction(db(RwRo)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred exec(db(RwRo)::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% Low-level interface

:- pred prepare(db(RwRo)::in, string::in, maybe_error(stmt)::out,
    io::di, io::uo) is det.

:- pred bind(db(RwRo)::in, stmt::in, bind_index::in, bind_value::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred bind_int(db(RwRo)::in, stmt::in, bind_index::in, int::in,
    maybe_error::out, io::di, io::uo) is det.

    % This is "unsafe" in that the GC could collect the string while it is
    % still bound to the stmt.  You must keep a reference to the string while
    % it is still bound to the stmt.
    %
:- pred unsafe_bind_text(db(RwRo)::in, stmt::in, bind_index::in, string::in,
    maybe_error::out, io::di, io::uo) is det.

    % This is "unsafe" in that the GC could collect the object containing
    % the pointer address while the address is still bound to the stmt.
    % You must keep a reference to the object while the pointer is still
    % bound to the stmt.
    %
:- pred unsafe_bind_blob(db(RwRo)::in, stmt::in, bind_index::in,
    c_pointer::in, int::in, maybe_error::out, io::di, io::uo) is det.

:- pred bind_null(db(RwRo)::in, stmt::in, bind_index::in,
    maybe_error::out, io::di, io::uo) is det.

:- pred step(db(RwRo)::in, sqlite3.stmt::in, step_result::out,
    io::di, io::uo) is det.

:- pred reset(db(RwRo)::in, stmt::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred finalize(stmt::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred column_is_null(stmt::in, column::in, bool::out,
    io::di, io::uo) is det.

:- pred column_int(stmt::in, column::in, int::out,
    io::di, io::uo) is det.

% :- pred column_int64(stmt::in, column::in, int64::out,
%     io::di, io::uo) is det.

:- pred column_text(stmt::in, column::in, string::out,
    io::di, io::uo) is det.

:- pred column_maybe_text(stmt::in, column::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred column_blob(stmt::in, column::in, c_pointer::out, int::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func escape_LIKE_argument(char, string) = string.

%-----------------------------------------------------------------------------%

% High-level interface

    % The bindings list is kept alive until the statement is finalized.
    %
:- pred with_stmt(
    pred(db(RwRo), stmt, T, io, io)::in(pred(in, in, out(TI), di, uo) is det),
    db(RwRo)::in, string::in, assoc_list(bind_index, bind_value)::in,
    T::out(TI), io::di, io::uo) is det.

:- pred with_prepared_stmt(
    pred(db(RwRo), stmt, T, io, io)::in(pred(in, in, out(TI), di, uo) is det),
    db(RwRo)::in, stmt::in, assoc_list(bind_index, bind_value)::in,
    T::out(TI), io::di, io::uo) is det.

:- pred with_stmt_acc(
    pred(db(RwRo), stmt, T, T, io, io)::in(pred(in, in, in, out, di, uo) is det),
    db(RwRo)::in, string::in, assoc_list(bind_index, bind_value)::in,
    T::in, T::out, io::di, io::uo) is det.

:- pred with_stmt_acc3(
    pred(db(RwRo), stmt, maybe_error, A, A, B, B, C, C, io, io),
    db(RwRo), string, assoc_list(bind_index, bind_value),
    maybe_error, A, A, B, B, C, C, io, io).
:- mode with_stmt_acc3(
    in(pred(in, in, out, in, out, in, out, in, out, di, uo) is det),
    in, in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode with_stmt_acc3(
    in(pred(in, in, out, in, out, in, out, array_di, array_uo, di, uo) is det),
    in, in, in, out, in, out, in, out, array_di, array_uo, di, uo) is det.

:- pred bind_checked(db(RwRo)::in, stmt::in,
    assoc_list(bind_index, bind_value)::in, io::di, io::uo) is det.

:- pred step_ok(db(RwRo)::in, stmt::in, step_result::out(step_result_nonerror),
    io::di, io::uo) is det.

:- pred step_ok_keep_alive(db(RwRo)::in, stmt::in,
    assoc_list(bind_index, bind_value)::in,
    step_result::out(step_result_nonerror), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module pair.

:- include_module sqlite3.keep_alive.
:- import_module sqlite3.keep_alive.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <sqlite3.h>
").

:- pragma foreign_type("C", db(_), "sqlite3 *").

:- pragma foreign_type("C", stmt, "sqlite3_stmt *").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
MR_String _msqlite_copy_errmsg(sqlite3 *db);
").

:- pragma foreign_code("C", "
MR_String
_msqlite_copy_errmsg(sqlite3 *db)
{
    MR_String str;
    MR_make_aligned_string_copy(str, sqlite3_errmsg(db));
    return str;
}
").

%-----------------------------------------------------------------------------%

init_multithreaded(Res, !IO) :-
    init_multithreaded_2(Error, !IO),
    ( Error = "" ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred init_multithreaded_2(string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    init_multithreaded_2(Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int rc;

    rc = sqlite3_config(SQLITE_CONFIG_MULTITHREAD);
    if (rc != SQLITE_OK) {
        Error = MR_make_string(MR_ALLOC_ID,
            ""sqlite3_config returned: %d"", rc);
    } else {
        rc = sqlite3_initialize();
        if (rc != SQLITE_OK) {
            Error = MR_make_string(MR_ALLOC_ID,
                ""sqlite3_initialize returned: %d"", rc);
        } else {
            Error = MR_make_string_const("""");
        }
    }
").

%-----------------------------------------------------------------------------%

synchronous(off,    "OFF").
synchronous(normal, "NORMAL").
synchronous(full,   "FULL").

%-----------------------------------------------------------------------------%

:- type open_mode
    --->    rw(synchronous)
    ;       ro.

open_rw(FileName, SynchronousValue, Res, !IO) :-
    open_2(FileName, rw(SynchronousValue), Res, !IO).

open_ro(FileName, Res, !IO) :-
    open_2(FileName, ro, Res, !IO).

:- pred open_2(string::in, open_mode::in, maybe_error(db(RwRo))::out,
    io::di, io::uo) is det.

open_2(FileName, OpenMode, Res, !IO) :-
    (
        OpenMode = rw(_),
        ReadWrite = yes
    ;
        OpenMode = ro,
        ReadWrite = no
    ),
    open_3(FileName, ReadWrite, Db, OpenError, !IO),
    ( OpenError = "" ->
        set_options_on_open(Db, OpenMode, Res, !IO)
    ;
        Res = error(OpenError)
    ),
    (
        Res = ok(_)
    ;
        Res = error(_),
        close(Db, !IO)
    ).

:- pred open_3(string::in, bool::in, db(RwRo)::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_3(FileName::in, ReadWrite::in, Db::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int flags;
    int rc;

    if (ReadWrite) {
        flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    } else {
        flags = SQLITE_OPEN_READONLY;
    }
    rc = sqlite3_open_v2(FileName, &Db, flags, NULL);
    if (rc == SQLITE_OK) {
        Error = MR_make_string_const("""");
    } else if (Db == NULL) {
        Error = MR_make_string_const(""out of memory"");
    } else {
        MR_make_aligned_string_copy_msg(Error, sqlite3_errmsg(Db),
            MR_ALLOC_ID);
    }
").

:- pred set_options_on_open(db(RwRo)::in, open_mode::in,
    maybe_error(db(RwRo))::out, io::di, io::uo) is det.

set_options_on_open(Db, OpenMode, Res, !IO) :-
    Common = "",
    (
        OpenMode = rw(Synchronous),
        synchronous(Synchronous, SynchronousValue),
        PragmaJournalMode = "PRAGMA journal_mode=WAL;",
        PragmaSynchronous = "PRAGMA synchronous=" ++ SynchronousValue,
        Sql = Common ++ PragmaJournalMode ++ PragmaSynchronous
    ;
        OpenMode = ro,
        Sql = Common
    ),
    sqlite3.exec(Db, Sql, ExecRes, !IO),
    (
        ExecRes = ok,
        Res = ok(Db)
    ;
        ExecRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    close(Db::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (Db) {
        sqlite3_close(Db);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    rw_db_to_ro_db(DbRw::in, DbRo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DbRo = DbRw;
").

%-----------------------------------------------------------------------------%

begin_transaction(Db, Res, !IO) :-
    exec(Db, "BEGIN TRANSACTION", Res, !IO).

end_transaction(Db, Res, !IO) :-
    exec(Db, "END TRANSACTION", Res, !IO).

rollback_transaction(Db, Res, !IO) :-
    exec(Db, "ROLLBACK TRANSACTION", Res, !IO).

%-----------------------------------------------------------------------------%

exec(Db, Sql, Res, !IO) :-
    exec_2(Db, Sql, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred exec_2(db(RwRo)::in, string::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    exec_2(Db::in, Sql::in, Rc::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    char *errmsg;
    if (SQLITE_OK == sqlite3_exec(Db, Sql, NULL, NULL, &errmsg)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        MR_make_aligned_string_copy_msg(Error, errmsg, MR_ALLOC_ID);
        sqlite3_free(errmsg);
    }
").

%-----------------------------------------------------------------------------%

prepare(Db, Sql, ResStmt, !IO) :-
    prepare_2(Db, Sql, Res, Stmt, Error, !IO),
    ( Res = 1 ->
        ResStmt = ok(Stmt)
    ;
        ResStmt = error(Error)
    ).

:- pred prepare_2(db(RwRo)::in, string::in, int::out, stmt::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    prepare_2(Db::in, Sql::in, Res::out, Stmt::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int rc = sqlite3_prepare_v2(Db, Sql, -1, &Stmt, NULL);
    if (rc == SQLITE_OK) {
        Res = 1;
        Error = MR_make_string_const("""");
    } else {
        Res = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

bind(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        Value = null,
        bind_null(Db, Stmt, IndexType, Res, !IO)
    ;
        Value = int(Int),
        bind_int(Db, Stmt, IndexType, Int, Res, !IO)
    /*
    ;
        Value = int64(Int64),
        bind_int64(Db, Stmt, IndexType, Int64, Res, !IO)
    */
    ;
        Value = text_by_reference(String),
        unsafe_bind_text(Db, Stmt, IndexType, String, Res, !IO)
    ;
        Value = blob_by_reference(Pointer, SizeBytes),
        unsafe_bind_blob(Db, Stmt, IndexType, Pointer, SizeBytes, Res, !IO)
    ).

%-----------------------------------------------------------------------------%

bind_int(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_int_2(Db, Stmt, Index, Value, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_int_2(db(RwRo)::in, stmt::in, int::in, int::in, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_int_2(Db::in, Stmt::in, Index::in, Value::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK == sqlite3_bind_int(Stmt, Index, Value)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

/*
:- pred bind_int64(db(RwRo)::in, stmt::in, bind_index::in, int64::in,
    maybe_error::out, io::di, io::uo) is det.

bind_int64(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_int64_2(Db, Stmt, Index, Value, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_int64_2(db(RwRo)::in, stmt::in, int::in, int64::in, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_int64_2(Db::in, Stmt::in, Index::in, Value::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK == sqlite3_bind_int64(Stmt, Index, Value)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").
*/

%-----------------------------------------------------------------------------%

unsafe_bind_text(Db, Stmt, IndexType, Value, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_text_2(Db, Stmt, Index, Value, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_text_2(db(RwRo)::in, stmt::in, int::in, string::in,
    int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_text_2(Db::in, Stmt::in, Index::in, Value::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK ==
        sqlite3_bind_text(Stmt, Index, Value, strlen(Value), SQLITE_STATIC))
    {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

unsafe_bind_blob(Db, Stmt, IndexType, Pointer, SizeBytes, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_blob_2(Db, Stmt, Index, Pointer, SizeBytes, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_blob_2(db(RwRo)::in, stmt::in, int::in,
    c_pointer::in, int::in, int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_blob_2(Db::in, Stmt::in, Index::in, Pointer::in, SizeBytes::in,
        Rc::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK ==
        sqlite3_bind_blob(Stmt, Index, (void *)Pointer, SizeBytes, SQLITE_STATIC))
    {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

bind_null(Db, Stmt, IndexType, Res, !IO) :-
    (
        IndexType = num(Index)
    ;
        IndexType = name(Name),
        bind_parameter_index(Stmt, Name, Index, !IO)
    ),
    bind_null_2(Db, Stmt, Index, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred bind_null_2(db(RwRo)::in, stmt::in, int::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind_null_2(Db::in, Stmt::in, Index::in, Rc::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (SQLITE_OK == sqlite3_bind_null(Stmt, Index)) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

:- pred bind_parameter_index(stmt::in, string::in, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    bind_parameter_index(Stmt::in, Name::in, Index::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Index = sqlite3_bind_parameter_index(Stmt, Name);
").

%-----------------------------------------------------------------------------%

step(Db, Stmt, Res, !IO) :-
    step_2(Db, Stmt, Rc, Error, !IO),
    ( Rc = 0 ->
        Res = done
    ; Rc = 1 ->
        Res = row
    ;
        Res = error(Error)
    ).

:- pred step_2(db(RwRo)::in, stmt::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    step_2(Db::in, Stmt::in, Res::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int rc = sqlite3_step(Stmt);
    if (rc == SQLITE_DONE) {
        Res = 0;
        Error = MR_make_string_const("""");
    } else if (rc == SQLITE_ROW) {
        Res = 1;
        Error = MR_make_string_const("""");
    } else {
        Res = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

reset(Db, Stmt, Res, !IO) :-
    reset_2(Db, Stmt, Rc, Error, !IO),
    ( Rc = 1 ->
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred reset_2(db(RwRo)::in, stmt::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    reset_2(Db::in, Stmt::in, Rc::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (sqlite3_reset(Stmt) == SQLITE_OK) {
        Rc = 1;
        Error = MR_make_string_const("""");
    } else {
        Rc = -1;
        Error = _msqlite_copy_errmsg(Db);
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    finalize(Stmt::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    if (Stmt) {
        sqlite3_finalize(Stmt);
    }
").

%-----------------------------------------------------------------------------%

column_is_null(Stmt, column(Col), IsNull, !IO) :-
    column_is_null_2(Stmt, Col, IsNull, !IO).

:- pred column_is_null_2(stmt::in, int::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_is_null_2(Stmt::in, Col::in, IsNull::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    int type = sqlite3_column_type(Stmt, Col);
    IsNull = (type == SQLITE_NULL) ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%

column_int(Stmt, column(Col), Int, !IO) :-
    column_int_2(Stmt, Col, Int, !IO).

:- pred column_int_2(stmt::in, int::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_int_2(Stmt::in, Col::in, Int::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Int = sqlite3_column_int(Stmt, Col);
").

%-----------------------------------------------------------------------------%

/*
column_int64(Stmt, column(Col), Int, !IO) :-
    column_int64_2(Stmt, Col, Int, !IO).

:- pred column_int64_2(stmt::in, int::in, int64::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_int64_2(Stmt::in, Col::in, Int::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Int = sqlite3_column_int64(Stmt, Col);
").
*/

%-----------------------------------------------------------------------------%

column_text(Stmt, column(Col), Str, !IO) :-
    column_text_2(Stmt, Col, Str, !IO).

:- pred column_text_2(stmt::in, int::in, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_text_2(Stmt::in, Col::in, Str::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    const unsigned char *s =  sqlite3_column_text(Stmt, Col);
    if (s) {
        MR_make_aligned_string_copy_msg(Str, (const char *)s, MR_ALLOC_ID);
    } else {
        Str = MR_make_string_const("""");
    }
").

column_maybe_text(Stmt, Col, MaybeStr, !IO) :-
    column_is_null(Stmt, Col, IsNull, !IO),
    (
        IsNull = yes,
        MaybeStr = no
    ;
        IsNull = no,
        column_text(Stmt, Col, Str, !IO),
        MaybeStr = yes(Str)
    ).

%-----------------------------------------------------------------------------%

column_blob(Stmt, column(Col), Pointer, SizeBytes, !IO) :-
    column_blob_2(Stmt, Col, Pointer, SizeBytes, !IO).

:- pred column_blob_2(stmt::in, int::in, c_pointer::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    column_blob_2(Stmt::in, Col::in, Pointer::out, SizeBytes::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Pointer = (MR_Word) sqlite3_column_blob(Stmt, Col);
    SizeBytes = sqlite3_column_bytes(Stmt, Col);
").

%-----------------------------------------------------------------------------%

escape_LIKE_argument(EscChar, S0) = S :-
    string.to_char_list(S0, Cs0),
    list.foldl(escape_LIKE_argument_2(EscChar), Cs0, [], RevCs),
    string.from_rev_char_list(RevCs, S).

:- pred escape_LIKE_argument_2(char::in, char::in,
    list(char)::in, list(char)::out) is det.

escape_LIKE_argument_2(EscChar, C, RevCs0, RevCs) :-
    ( C = EscChar ->
        RevCs = [EscChar, EscChar | RevCs0]
    ; C = ('%') ->
        RevCs = ['%', EscChar | RevCs0]
    ; C = ('_') ->
        RevCs = ['_', EscChar | RevCs0]
    ;
        RevCs = [C | RevCs0]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% High-level interface

with_stmt(Pred, Db, Sql, Bindings, Result, !IO) :-
    sqlite3.prepare(Db, Sql, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        promise_equivalent_solutions [Result, !:IO]
        (try [io(!IO)] (
            bind_checked(Db, Stmt, Bindings, !IO),
            Pred(Db, Stmt, Result, !IO),
            keep_alive(Bindings, !IO)
         )
         then
            sqlite3.finalize(Stmt, !IO)
         catch_any Excp ->
            sqlite3.finalize(Stmt, !IO),
            throw(Excp)
        )
    ;
        ResStmt = error(Error),
        throw(sqlite_error(Error))
    ).

with_prepared_stmt(Pred, Db, Stmt, Bindings, Result, !IO) :-
    promise_equivalent_solutions [Result, !:IO]
    (try [io(!IO)] (
        bind_checked(Db, Stmt, Bindings, !IO),
        Pred(Db, Stmt, Result, !IO),
        keep_alive(Bindings, !IO)
     )
     then
        sqlite3.reset(Db, Stmt, _, !IO)
     catch_any Excp ->
        sqlite3.reset(Db, Stmt, _, !IO),
        throw(Excp)
    ).

with_stmt_acc(Pred, Db, Sql, Bindings, !Acc, !IO) :-
    sqlite3.prepare(Db, Sql, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        promise_equivalent_solutions [!:Acc, !:IO]
        (try [io(!IO)] (
            bind_checked(Db, Stmt, Bindings, !IO),
            Pred(Db, Stmt, !Acc, !IO),
            keep_alive(Bindings, !IO)
         )
         then
            sqlite3.finalize(Stmt, !IO)
         catch_any Excp ->
            sqlite3.finalize(Stmt, !IO),
            throw(Excp)
        )
    ;
        ResStmt = error(Error),
        throw(sqlite_error(Error))
    ).

with_stmt_acc3(Pred, Db, Sql, Bindings, Res, !A, !B, !C, !IO) :-
    sqlite3.prepare(Db, Sql, ResStmt, !IO),
    (
        ResStmt = ok(Stmt),
        promise_equivalent_solutions [Res, !:A, !:B, !:C, !:IO]
        (try [io(!IO)] (
            bind_checked(Db, Stmt, Bindings, !IO),
            Pred(Db, Stmt, Res, !A, !B, !C, !IO),
            keep_alive(Bindings, !IO)
         )
         then
            sqlite3.finalize(Stmt, !IO)
         catch_any Excp ->
            sqlite3.finalize(Stmt, !IO),
            throw(Excp)
        )
    ;
        ResStmt = error(Error),
        throw(sqlite_error(Error))
    ).

bind_checked(Db, Stmt, Bindings, !IO) :-
    (
        Bindings = []
    ;
        Bindings = [IndexType - Value | RestBindings],
        bind(Db, Stmt, IndexType, Value, BindRes, !IO),
        (
            BindRes = ok
        ;
            BindRes = error(Error),
            throw(sqlite_error(Error))
        ),
        bind_checked(Db, Stmt, RestBindings, !IO)
    ).

step_ok(Db, Stmt, StepResult, !IO) :-
    sqlite3.step(Db, Stmt, StepResult, !IO),
    (
        StepResult = done
    ;
        StepResult = row
    ;
        StepResult = error(Error),
        throw(sqlite_error(Error))
    ).

step_ok_keep_alive(Db, Stmt, KeepAlive, StepResult, !IO) :-
    sqlite3.step(Db, Stmt, StepResult, !IO),
    keep_alive(KeepAlive, !IO),
    (
        StepResult = done
    ;
        StepResult = row
    ;
        StepResult = error(Error),
        throw(sqlite_error(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
