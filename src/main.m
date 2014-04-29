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
    examine(IMAP, mailbox("INBOX"), ResExamine, Alerts, !IO),
    report_alerts(Alerts, !IO),
    (
        ResExamine = ok(Text),
        io.write_string(Text, !IO),
        io.nl(!IO)
    ;
        ResExamine = no(Text),
        report_error(Text, !IO)
    ;
        ResExamine = bad(Text),
        report_error(Text, !IO)
    ;
        ResExamine = fatal(Text),
        report_error(Text, !IO)
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

:- pred report_alerts(list(alert)::in, io::di, io::uo) is det.

report_alerts(Alerts, !IO) :-
    list.foldl(report_alert, Alerts, !IO).

:- pred report_alert(alert::in, io::di, io::uo) is det.

report_alert(alert(Alert), !IO) :-
    io.write_string("ALERT: ", !IO),
    io.write_string(Alert, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
