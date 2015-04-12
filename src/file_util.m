% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module file_util.
:- interface.

:- import_module bool.
:- import_module io.

:- type file_exists_result
    --->    yes(bool)    % is_dir
    ;       no
    ;       error(string).

:- pred file_exists(string::in, file_exists_result::out, io::di, io::uo)
    is det.

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
    file_exists_2(Path, RC, IsDir, Error, !IO),
    ( RC = 0 ->
        Res = yes(IsDir)
    ; RC = 1 ->
        Res = no
    ;
        Res = error(Error)
    ).

:- pred file_exists_2(string::in, int::out, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    file_exists_2(Path::in, RC::out, IsDir::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct stat stbuf;
    char        errbuf[MR_STRERROR_BUF_SIZE];
    const char  *errno_msg;

    RC = stat(Path, &stbuf);
    if (RC == 0) {
        Error = MR_make_string_const("""");
        IsDir = S_ISDIR(stbuf.st_mode) ? MR_YES : MR_NO;
    } else if (errno == ENOENT) {
        RC = 1;
        IsDir = MR_NO;
        Error = MR_make_string_const("""");
    } else {
        RC = -1;
        IsDir = MR_NO;
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_make_aligned_string_copy_msg(Error, errno_msg, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
