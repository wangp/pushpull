%-----------------------------------------------------------------------------%

:- module signal.
:- interface.

:- import_module io.

:- type signal
    --->    sighup
    ;       sigint
    ;       sigpipe.

:- type handler
    --->    ignore
    ;       count.

:- pred install_signal_handler(signal::in, handler::in, io::di, io::uo) is det.

:- pred get_sighup_count(int::out, io::di, io::uo) is det.
:- pred get_sigint_count(int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- pragma foreign_decl("C", "
#include <signal.h>
").

:- pragma foreign_enum("C", signal/0, [
    sighup - "SIGHUP",
    sigint - "SIGINT",
    sigpipe - "SIGPIPE"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "
static sig_atomic_t sighup_count;
static sig_atomic_t sigint_count;
static sig_atomic_t sigpipe_count;

static void
count_signal(int sig)
{
    switch (sig) {
        case SIGHUP:
            sighup_count++;
            break;
        case SIGINT:
            sigint_count++;
            break;
        case SIGPIPE:
            sigpipe_count++;
            break;
    }
}
").

install_signal_handler(Signal, Handler, !IO) :-
    (
        Handler = ignore,
        Ignore = yes
    ;
        Handler = count,
        Ignore = no
    ),
    install_signal_handler_2(Signal, Ignore, !IO).

:- pred install_signal_handler_2(signal::in, bool::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    install_signal_handler_2(Signal::in, Ignore::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct sigaction act;

    act.sa_handler = Ignore ? SIG_IGN : count_signal;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(Signal, &act, NULL);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_sighup_count(N::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    N = sighup_count;
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
