%-----------------------------------------------------------------------------%

:- module log.
:- interface.

:- import_module io.
:- import_module list.

:- import_module imap.

:- pred report_error(string::in, io::di, io::uo) is det.

:- pred report_alerts(list(alert)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

report_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).

report_alerts(Alerts, !IO) :-
    list.foldl(report_alert, Alerts, !IO).

:- pred report_alert(alert::in, io::di, io::uo) is det.

report_alert(alert(Alert), !IO) :-
    io.write_string("ALERT: ", !IO),
    io.write_string(Alert, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
