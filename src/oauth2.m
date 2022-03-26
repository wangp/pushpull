% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module oauth2.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module imap.
:- import_module log.
:- import_module shell_word.

:- pred get_imap_oauth2_sasl_string(log::in, username::in, list(word)::in,
    maybe_error(sasl_string)::out, io::di, io::uo) is det.

:- pred make_imap_xoauth2_sasl_string(username::in, string::in,
    sasl_string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module string.builder.

:- import_module base64.
:- import_module call_command.
:- import_module process.

%-----------------------------------------------------------------------------%

get_imap_oauth2_sasl_string(Log, UserName, Command, Res, !IO) :-
    get_oauth2_access_token(Log, Command, Res0, !IO),
    (
        Res0 = ok(AccessToken),
        make_imap_xoauth2_sasl_string(UserName, AccessToken, SASLString),
        Res = ok(SASLString)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred get_oauth2_access_token(log::in, list(word)::in,
    maybe_error(string)::out, io::di, io::uo) is det.

get_oauth2_access_token(Log, CommandWords, Res, !IO) :-
    CommandStrings = list.map(word_string, CommandWords),
    (
        CommandStrings = [Command | Args],
        log_info(Log, "Calling command: " ++ Command, !IO),
        call_command_capture_stdout(Command, Args, environ([]), CallRes, !IO),
        (
            CallRes = ok(String0),
            String = string.strip(String0),
            ( String = "" ->
                Res = error("auth_oauth2_command returned empty string")
            ;
                Res = ok(String)
            )
        ;
            CallRes = error(Error),
            Res = error("auth_oauth2_command error: " ++ io.error_message(Error))
        )
    ;
        CommandStrings = [],
        % Should not happen.
        Res = error("empty command")
    ).

    % See RFC 7628
    % and
    % https://developers.google.com/gmail/imap/xoauth2-protocol#the_sasl_xoauth2_mechanism
    %
make_imap_xoauth2_sasl_string(UserName, AccessToken, SASLString) :-
    UserName = username(UserNameStr),
    Unencoded =
        "user=" ++ UserNameStr ++ "\x1\" ++
        "auth=Bearer " ++ AccessToken ++ "\x1\\x1\",
    Stream = string.builder.handle,
    State0 = string.builder.init,
    base64.encode(Unencoded, 0, length(Unencoded), Stream, State0, State),
    Encoded = string.builder.to_string(State),
    SASLString = sasl_string(Encoded).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
