% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module utime.
:- interface.

:- import_module io.
:- import_module time.

    % Set file modification time (and file access time as well).
    %
:- pred set_file_atime_mtime(string::in, time_t::in, io.res::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <sys/types.h>
    #include <utime.h>
").

set_file_atime_mtime(FileName, Time, Res, !IO) :-
    utime(FileName, Time, RC, Error, !IO),
    ( RC = 0 ->
        Res = ok
    ;
        Res = error(io.make_io_error(Error))
    ).

:- pred utime(string::in, time_t::in, int::out, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    utime(FileName::in, TimeT::in, RC::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct utimbuf t;
    t.actime = TimeT;
    t.modtime = TimeT;

    RC = utime(FileName, &t);
    if (RC == 0) {
        Error = MR_make_string_const("""");
    } else {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
