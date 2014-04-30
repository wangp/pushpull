%-----------------------------------------------------------------------------%

:- module signal.
:- interface.

:- import_module bool.
:- import_module io.

:- pred ignore_sigpipe(bool::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <signal.h>

    static sig_atomic_t sigpipe_count;
    static void sigpipe_handler(int sig);
").

:- pragma foreign_code("C", "
static void
sigpipe_handler(int sig)
{
    (void) sig;
    sigpipe_count++;
}
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
