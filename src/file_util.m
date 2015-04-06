%-----------------------------------------------------------------------------%

:- module file_util.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

:- pred file_exists(string::in, maybe_error(bool)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <unistd.h>
").

%-----------------------------------------------------------------------------%

file_exists(Path, Res, !IO) :-
    file_exists_2(Path, RC, Error, !IO),
    ( RC = 0 ->
        Res = ok(yes)
    ; RC = 1 ->
        Res = ok(no)
    ;
        Res = error(Error)
    ).

:- pred file_exists_2(string::in, int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    file_exists_2(Path::in, RC::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct stat stbuf;
    char        errbuf[MR_STRERROR_BUF_SIZE];
    const char  *errno_msg;

    RC = stat(Path, &stbuf);
    if (RC == 0) {
        Error = MR_make_string_const("""");
    } else if (errno == ENOENT) {
        RC = 1;
        Error = MR_make_string_const("""");
    } else {
        RC = -1;
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_make_aligned_string_copy_msg(Error, errno_msg, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
