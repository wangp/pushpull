%-----------------------------------------------------------------------------%

:- module test_oauth2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module imap.
:- import_module imap.oauth2.

%-----------------------------------------------------------------------------%

main(!IO) :-
    Host = "imap.example.com",
    Port = 993,
    UserName = username("USERNAME"),
    AccessToken = oauth2_access_token("ACCESS_TOKEN"),

    make_oauthbearer_sasl_string(Host, Port, UserName, AccessToken,
        OauthbearerString),
    io.print_line(OauthbearerString, !IO),

    make_xoauth2_sasl_string(UserName, AccessToken, XoauthString),
    io.print_line(XoauthString, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
