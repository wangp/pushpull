%-----------------------------------------------------------------------------%

:- module test_oauth2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module imap.
:- import_module oauth2.

%-----------------------------------------------------------------------------%

main(!IO) :-
    make_imap_xoauth2_sasl_string(username("USERNAME"), "ACCESS_TOKEN",
        SASLString),
    io.print_line(SASLString, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
