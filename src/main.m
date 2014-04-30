%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module maybe.

:- import_module imap.
:- import_module signal.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ignore_sigpipe(yes, !IO),
    io.command_line_arguments(Args, !IO),
    ( Args = [HostPort, UserName, Password] ->
        imap.open(HostPort, ResOpen, OpenAlerts, !IO),
        report_alerts(OpenAlerts, !IO),
        (
            ResOpen = ok(IMAP),
            login(IMAP, username(UserName), password(Password),
                result(ResLogin, LoginMessage, LoginAlerts), !IO),
            report_alerts(LoginAlerts, !IO),
            (
                ResLogin = ok,
                io.write_string(LoginMessage, !IO),
                io.nl(!IO),
                logged_in(IMAP, !IO)
            ;
                ( ResLogin = no
                ; ResLogin = bad
                ; ResLogin = bye
                ; ResLogin = error
                ),
                report_error(LoginMessage, !IO)
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
    examine(IMAP, mailbox("INBOX"), result(ResExamine, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResExamine = ok,
        io.write_string(Text, !IO),
        io.nl(!IO)
    ;
        ( ResExamine = no
        ; ResExamine = bad
        ; ResExamine = bye
        ; ResExamine = error
        ),
        report_error(Text, !IO)
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
