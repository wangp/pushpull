% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module setsockopt.
:- interface.

:- import_module io.

:- import_module maybe.

:- type timeout_option
    --->    recv_timeout
    ;       send_timeout.

:- pred set_timeout(int::in, timeout_option::in, int::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- pragma foreign_decl("C", "
   #include <sys/socket.h>
   #include <sys/time.h>
   #include <sys/types.h>
").

%-----------------------------------------------------------------------------%

set_timeout(Fd, Opt, TimeoutSecs, Res, !IO) :-
    (
        Opt = recv_timeout,
        Rcv = yes
    ;
        Opt = send_timeout,
        Rcv = no
    ),
    setsockopt_2(Fd, Rcv, TimeoutSecs, Ok, Error, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred setsockopt_2(int::in, bool::in, int::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setsockopt_2(Fd::in, Rcv::in, TimeoutSecs::in, Ok::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe /* strerror */,
        tabled_for_io],
"
    int optname;
    struct timeval tv;
    int rc;

    optname = (Rcv) ? SO_RCVTIMEO : SO_SNDTIMEO;
    tv.tv_sec = TimeoutSecs;
    tv.tv_usec = 0;

    rc = setsockopt(Fd, SOL_SOCKET, optname, &tv, sizeof(tv));
    if (rc == 0) {
        Ok = MR_YES;
        Error = MR_make_string_const("""");
    } else {
        Ok = MR_NO;
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
