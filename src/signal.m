%-----------------------------------------------------------------------------%

:- module signal.
:- interface.

:- import_module bool.
:- import_module io.

:- pred ignore_sigint(bool::in, io::di, io::uo) is det.

:- pred ignore_sigpipe(bool::in, io::di, io::uo) is det.

:- pred get_sigint_count(int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <signal.h>

    static sig_atomic_t sigint_count;
    static void sigint_handler(int sig);

    static sig_atomic_t sigpipe_count;
    static void sigpipe_handler(int sig);
").

:- pragma foreign_code("C", "
static void
sigint_handler(int sig)
{
    (void) sig;
    sigint_count++;
}

static void
sigpipe_handler(int sig)
{
    (void) sig;
    sigpipe_count++;
}
").

:- pragma foreign_proc("C",
    ignore_sigint(Ignore::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = (Ignore ? SIG_IGN : sigint_handler);
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(SIGINT, &act, NULL);
").

:- pragma foreign_proc("C",
    ignore_sigpipe(Ignore::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = (Ignore ? SIG_IGN : sigpipe_handler);
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(SIGPIPE, &act, NULL);
").

:- pragma foreign_proc("C",
    get_sigint_count(N::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    N = sigint_count;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
