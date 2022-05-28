% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module log_help.
:- interface.

:- import_module io.
:- import_module list.

:- import_module log.
:- import_module imap.

:- pred report_error(log::in, string::in, io::di, io::uo) is det.

:- pred report_alerts(log::in, list(alert)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

report_error(Log, Error, !IO) :-
    log_error(Log, Error, !IO),
    io.set_exit_status(1, !IO).

report_alerts(Log, Alerts, !IO) :-
    list.foldl(report_alert(Log), Alerts, !IO).

:- pred report_alert(log::in, alert::in, io::di, io::uo) is det.

report_alert(Log, alert(Alert), !IO) :-
    log_info(Log, Alert, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
