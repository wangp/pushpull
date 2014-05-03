%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module pretty_printer.

:- import_module imap.
:- import_module imap.types.
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
        io.nl(!IO),
        do_uid_search(IMAP, UIDs, !IO),
        do_uid_fetch(IMAP, UIDs, !IO)
    ;
        ( ResExamine = no
        ; ResExamine = bad
        ; ResExamine = bye
        ; ResExamine = error
        ),
        report_error(Text, !IO)
    ).

:- pred do_uid_search(imap::in, list(uid)::out, io::di, io::uo) is det.

do_uid_search(IMAP, UIDs, !IO) :-
    uid_search(IMAP, modseq(mod_seq_valzer(det_from_string("10"))),
        result(ResSearch, Text, Alerts), !IO),
    report_alerts(Alerts, !IO),
    (
        ResSearch = ok_with_data(UIDs - MaybeModSeqValue),
        io.write_string(Text, !IO),
        io.nl(!IO),
        io.write_list(UIDs, ", ", write_uid, !IO),
        (
            MaybeModSeqValue = yes(mod_seq_value(ModSeqValue)),
            io.write_string(" (MODSEQ ", !IO),
            io.write_string(to_string(ModSeqValue), !IO),
            io.write_string(")", !IO)
        ;
            MaybeModSeqValue = no
        ),
        io.nl(!IO)
    ;
        ( ResSearch = no
        ; ResSearch = bad
        ; ResSearch = bye
        ; ResSearch = error
        ),
        report_error(Text, !IO),
        UIDs = []
    ).

:- pred do_uid_fetch(imap::in, list(uid)::in, io::di, io::uo) is det.

do_uid_fetch(IMAP, UIDs, !IO) :-
    ( make_sequence_set(UIDs, Set) ->
        Items = atts(rfc822, [flags, envelope]),
        uid_fetch(IMAP, Set, Items, result(ResFetch, Text, Alerts), !IO),
        report_alerts(Alerts, !IO)
    ;
        ResFetch = ok_with_data([]),
        Text = ""
    ),
    (
        ResFetch = ok_with_data(FetchResults),
        list.foldl(write_fetch_result, FetchResults, !IO)
    ;
        ( ResFetch = no
        ; ResFetch = bad
        ; ResFetch = bye
        ; ResFetch = error
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

:- pred write_uid(uid::in, io::di, io::uo) is det.

write_uid(uid(N), !IO) :-
    io.write_string(to_string(N), !IO).

:- pred write_fetch_result(pair(uid, msg_atts)::in, io::di, io::uo) is det.

write_fetch_result(UID - Atts, !IO) :-
    io.write_string("Fetched UID ", !IO),
    write_uid(UID, !IO),
    io.write_string(":\n", !IO),
    list.foldl(write_msg_att, Atts, !IO),
    io.nl(!IO).

:- pred write_msg_att(msg_att::in, io::di, io::uo) is det.

write_msg_att(Att, !IO) :-
    io.write_string("- ", !IO),
    pretty_printer.write_doc(format(Att), !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
