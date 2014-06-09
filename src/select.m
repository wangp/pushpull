%-----------------------------------------------------------------------------%

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

:- pred fd_set(int::in, fd_set::in, fd_set::out) is det.

:- pragma foreign_proc("C",
    fd_set(Fd::in, Set0::in, Set::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Set = Set0;
    FD_SET(Fd, &Set);
").

:- pragma foreign_proc("C",
    fd_isset(Set::in, Fd::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = FD_ISSET(Fd, &Set);
").

%-----------------------------------------------------------------------------%

select_read(Fds, TimeoutSeconds, Res, !IO) :-
    ( TimeoutSeconds < 0 ->
        Res = timeout
    ;
        fd_zero(FdSet0),
        list.foldl(fd_set, Fds, FdSet0, FdSet1),
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
