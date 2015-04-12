% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module select.
:- interface.

:- import_module io.
:- import_module list.

:- type fd_set.

:- type select_result
    --->    ready(int, fd_set)
    ;       timeout
    ;       interrupt
    ;       error(string).

:- pred select_read(list(int)::in, int::in, select_result::out, io::di, io::uo)
    is det.

:- pred fd_isset(fd_set::in, int::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.

:- pragma foreign_type("C", fd_set, "fd_set").

:- pragma foreign_decl("C", local, "
    #include <signal.h>
    #include <sys/select.h>
").

%-----------------------------------------------------------------------------%

:- pred fd_zero(fd_set::out) is det.

:- pragma foreign_proc("C",
    fd_zero(Set::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FD_ZERO(&Set);
").

:- pred fd_set(int::in, bool::out, fd_set::in, fd_set::out) is det.

:- pragma foreign_proc("C",
    fd_set(Fd::in, Ok::out, Set0::in, Set::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Set = Set0;
    if (Fd >= 0 && Fd < FD_SETSIZE) {
        FD_SET(Fd, &Set);
        Ok = MR_YES;
    } else {
        Ok = MR_NO;
    }
").

:- pred fd_set_list(list(int)::in, bool::out, fd_set::in, fd_set::out) is det.

fd_set_list([], yes, !Set).
fd_set_list([Fd | Fds], Ok, !Set) :-
    fd_set(Fd, Ok0, !Set),
    (
        Ok0 = yes,
        fd_set_list(Fds, Ok, !Set)
    ;
        Ok0 = no,
        Ok = no
    ).

:- pragma foreign_proc("C",
    fd_isset(Set::in, Fd::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Fd >= 0 && Fd < FD_SETSIZE) {
        SUCCESS_INDICATOR = FD_ISSET(Fd, &Set);
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%-----------------------------------------------------------------------------%

select_read(Fds, TimeoutSeconds, Res, !IO) :-
    ( TimeoutSeconds < 0 ->
        Res = timeout
    ;
        fd_zero(FdSet0),
        fd_set_list(Fds, Ok, FdSet0, FdSet1),
        (
            Ok = yes,
            list.foldl(max, Fds, 0, MaxFd),
            select_read_2(MaxFd, FdSet1, TimeoutSeconds, RC, FdSet, !IO),
            ( RC > 0 ->
                Res = ready(RC, FdSet)
            ; RC = 0 ->
                Res = timeout
            ; RC = -2 ->
                Res = interrupt
            ;
                Res = error("select failed")
            )
        ;
            Ok = no,
            Res = error("file descriptor out of range for select()")
        )
    ).

:- pred select_read_2(int::in, fd_set::in, int::in, int::out, fd_set::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    select_read_2(MaxFd::in, Set0::in, TimeoutSeconds::in, RC::out, Set::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct timespec timeout;
    sigset_t emptyset;

    Set = Set0;
    timeout.tv_sec = TimeoutSeconds;
    timeout.tv_nsec = 0;
    /* Unblock signals so we can be interrupted. */
    sigemptyset(&emptyset);
    RC = pselect(MaxFd + 1, &Set, NULL, NULL, &timeout, &emptyset);
    if (RC < 0) {
        RC = (errno == EINTR) ? -2 : -1;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
