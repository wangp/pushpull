%-----------------------------------------------------------------------------%

:- module terminal_attr.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

:- pred set_echo(bool::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include <termios.h>
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

set_echo(Echo, Res, !IO) :-
    set_echo_2(Echo, Ok, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error("failed to change terminal attribute")
    ).

:- pred set_echo_2(bool::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_echo_2(Echo::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct termios termios;

    if (0 == tcgetattr(STDIN_FILENO, &termios)) {
        if (Echo) {
            termios.c_lflag |= ECHO;
        } else {
            termios.c_lflag &= ~ECHO;
        }
        if (0 == tcsetattr(STDIN_FILENO, TCSANOW, &termios)) {
            Ok = MR_YES;
        } else {
            Ok = MR_NO;
        }
    } else {
        Ok = MR_NO;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
