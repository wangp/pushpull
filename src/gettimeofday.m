% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module gettimeofday.
:- interface.

:- import_module io.

:- pred gettimeofday(int::out, int::out, io::di, io::uo) is det.

:- pred gettimeofday_float(float::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

:- pragma foreign_decl("C", "
    #include <sys/time.h>
").

:- pragma foreign_proc("C",
    gettimeofday(Sec::out, Usec::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    struct timeval tv;
    gettimeofday(&tv, NULL);
    Sec = tv.tv_sec;
    Usec = tv.tv_usec;
").

gettimeofday_float(TimeOfDay, !IO) :-
    gettimeofday(Sec, Usec, !IO),
    TimeOfDay = float(Sec) + float(Usec) * 1e-6.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
