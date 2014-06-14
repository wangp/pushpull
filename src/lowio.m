%-----------------------------------------------------------------------------%

:- module lowio.
:- interface.

:- import_module io.
:- import_module maybe.

:- type filedes
    --->    filedes(int).

:- type open_output_result
    --->    ok(filedes)
    ;       already_exists
    ;       error(string).

:- pred open_output_excl(string::in, open_output_result::out, io::di, io::uo)
    is det.

:- pred close(filedes::in, maybe_error::out, io::di, io::uo) is det.

:- pred write_string(filedes::in, string::in, maybe_error::out, io::di, io::uo)
    is det.

:- pred fsync(filedes::in, maybe_error::out, io::di, io::uo) is det.

:- pred fsync_dir(string::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module string.

:- pragma foreign_decl("C", local, "
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

open_output_excl(FileName, Res, !IO) :-
    open_output_2(FileName, Fd, Error, Exists, !IO),
    ( Fd = -1 ->
        (
            Exists = yes,
            Res = already_exists
        ;
            Exists = no,
            Res = error(Error)
        )
    ;
        Res = ok(filedes(Fd))
    ).

:- pred open_output_2(string::in, int::out, string::out, bool::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_output_2(FileName::in, Fd::out, Error::out, Exists::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Fd = open(FileName, O_WRONLY | O_CREAT | O_EXCL, 0666);
    if (Fd == -1) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
        Exists = (errno == EEXIST) ? MR_YES : MR_NO;
    } else {
        Error = MR_make_string_const("""");
        Exists = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

close(filedes(Fd), Res, !IO) :-
    close_2(Fd, RC, Error, !IO),
    ( RC = -1 ->
        Res = error(Error)
    ;
        Res = ok
    ).

:- pred close_2(int::in, int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_2(Fd::in, RC::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    RC = close(Fd);
    if (Fd == -1) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Error = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

write_string(filedes(Fd), String, Res, !IO) :-
    string.count_code_units(String, Length),
    write_string_2(Fd, String, Length, RC, Error, !IO),
    ( RC = Length ->
        Res = ok
    ; RC = -1 ->
        Res = error(Error)
    ;
        Res = error("incomplete write")
    ).

:- pred write_string_2(int::in, string::in, int::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_string_2(Fd::in, String::in, Length::in, RC::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    /* XXX handle EINTR after some partial bytes written */
    do {
        RC = write(Fd, String, Length);
    } while (errno == EINTR);
    if (Fd == -1) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Error = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

fsync(filedes(Fd), Res, !IO) :-
    fsync_2(Fd, RC, Error, !IO),
    ( RC = -1 ->
        Res = error(Error)
    ;
        Res = ok
    ).

:- pred fsync_2(int::in, int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fsync_2(Fd::in, RC::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    RC = fsync(Fd);
    if (RC == -1) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Error = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

fsync_dir(DirName, Res, !IO) :-
    open_dir_rdonly(DirName, ResOpen, !IO),
    (
        ResOpen = ok(Fd),
        lowio.fsync(Fd, Res, !IO),
        lowio.close(Fd, _ResClose, !IO)
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

:- pred open_dir_rdonly(string::in, maybe_error(filedes)::out, io::di, io::uo)
    is det.

open_dir_rdonly(DirName, Res, !IO) :-
    open_dir_rdonly_2(DirName, Fd, Error, !IO),
    ( Fd = -1 ->
        Res = error(Error)
    ;
        Res = ok(filedes(Fd))
    ).

:- pred open_dir_rdonly_2(string::in, int::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    open_dir_rdonly_2(DirName::in, Fd::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int flags = O_RDONLY;

#ifdef O_DIRECTORY
    /* Linux-specific and not strictly necessary. */
    flags |= O_DIRECTORY;
#endif

    Fd = open(DirName, flags);
    if (Fd == -1) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    } else {
        Error = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
