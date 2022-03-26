% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module imap.oauth2.
:- interface.

    % OAuth2 bearer token, base64 encoded.
    %
:- type sasl_string
    --->    sasl_string(string).

:- pred make_oauthbearer_sasl_string(string::in, int::in, username::in,
    oauth2_access_token::in, sasl_string::out) is det.

:- pred make_xoauth2_sasl_string(username::in, oauth2_access_token::in,
    sasl_string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module string.builder.

:- import_module base64.

% See RFC 7628
% and
% https://developers.google.com/gmail/imap/xoauth2-protocol#the_sasl_xoauth2_mechanism

make_oauthbearer_sasl_string(Host, Port, UserName, AccessToken, SASLString) :-
    UserName = username(UserNameStr),
    AccessToken = oauth2_access_token(AccessTokenStr),
    BearerToken = string.format(
        "n,a=%s,\x1\host=%s\x1\port=%d\x1\auth=Bearer %s\x1\\x1\",
        [s(UserNameStr), s(Host), i(Port), s(AccessTokenStr)]),
    SASLString = sasl_string(base64(BearerToken)).

make_xoauth2_sasl_string(UserName, AccessToken, SASLString) :-
    UserName = username(UserNameStr),
    AccessToken = oauth2_access_token(AccessTokenStr),
    BearerToken = string.format(
        "user=%s\x1\auth=Bearer %s\x1\\x1\",
        [s(UserNameStr), s(AccessTokenStr)]),
    SASLString = sasl_string(base64(BearerToken)).

:- func base64(string) = string.

base64(Unencoded) = Encoded :-
    Stream = string.builder.handle,
    State0 = string.builder.init,
    base64.encode(Unencoded, 0, length(Unencoded), Stream, State0, State),
    Encoded = string.builder.to_string(State).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
