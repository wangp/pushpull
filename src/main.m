%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module imap.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = [HostPort, UserName, Password] ->
        imap.open(HostPort, ResOpen, !IO),
        (
            ResOpen = ok(IMAP),
            login(IMAP, username(UserName), password(Password), ResLogin, !IO),
            (
                ResLogin = ok(RespText),
                io.write(string(RespText), !IO),
                io.nl(!IO),
                logged_in(IMAP, !IO)
            ;
                ResLogin = no(ResponseText),
                report_error(string(ResponseText), !IO)
            ;
                ResLogin = bad(ResponseText),
                report_error(string(ResponseText), !IO)
            ;
                ResLogin = fatal(ResponseText),
                report_error(string(ResponseText), !IO)
            ;
                ResLogin = error(Error),
                report_error(Error, !IO)
            ),
            logout(IMAP, ResLogout, !IO),
            io.write(ResLogout, !IO),
            io.nl(!IO)
        ;
            ResOpen = error(Error),
            report_error(Error, !IO)
        )
    ;
        report_error("unexpected arguments", !IO)
    ).

:- pred logged_in(imap::in, io::di, io::uo) is det.

logged_in(IMAP, !IO) :-
    examine(IMAP, mailbox("INBOX"), ResExamine, !IO),
    (
        ResExamine = ok(RespText),
        io.write(string(RespText), !IO),
        io.nl(!IO)
    ;
        ResExamine = no(RespText),
        report_error(string(RespText), !IO)
    ;
        ResExamine = bad(RespText),
        report_error(string(RespText), !IO)
    ;
        ResExamine = fatal(RespText),
        report_error(string(RespText), !IO)
    ;
        ResExamine = error(Error),
        report_error(Error, !IO)
    ).

:- pred report_error(string::in, io::di, io::uo) is det.

report_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
