%-----------------------------------------------------------------------------%

:- module imap.command.
:- interface.

:- type command
    --->    tag - command_in_state.

:- type command_in_state
    --->    command_any(command_any)
    ;       command_auth(command_auth)
    ;       command_nonauth(command_nonauth)
    ;       command_select(command_select).

    % Valid in all states.
:- type command_any
    --->    capability
    ;       logout
    ;       noop.

    % Valid only in Authenticated or Selected state.
:- type command_auth
    --->    append
    ;       create
    ;       delete
    ;       examine
    ;       list
    ;       lsub
    ;       rename
    ;       select
    ;       status
    ;       subscribe
    ;       unsubscribe.

    % Valid only when in Not Authenticated state.
:- type command_nonauth
    --->    login(astring, astring). % userid, password
    %;       authenticate
    %;       starttls.

    % Valid only when in Selected state.
:- type command_select
    --->    check
    ;       close
    ;       expunge
    ;       copy
    ;       fetch
    ;       store
    ;       uid
    ;       search.

:- pred make_command_stream(command::in, list(string)::out) is det.

:- func crlf = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module require.
:- import_module string.

:- type acc == cord(string). % could probably be reverse list

:- type sp ---> sp.
:- type dquote ---> dquote.

:- typeclass add(T) where [
    pred add(T::in, acc::in, acc::out) is det
].

%-----------------------------------------------------------------------------%

make_command_stream(Command, List) :-
    add(Command, init, Acc),
    List = list(Acc).

:- instance add(string) where [
    add(S, A, snoc(A, S))
].

:- instance add(sp) where [
    add(sp, !Acc) :- add(" ", !Acc)
].

:- instance add(dquote) where [
    add(dquote, !Acc) :- add("\"", !Acc)
].

:- instance add(command) where [
    add(Tag - Command, !Acc) :-
    (
        add(Tag, !Acc),
        add(sp, !Acc),
        add(Command, !Acc)
    )
].

:- instance add(tag) where [
    add(tag(Tag), !Acc) :- add(Tag, !Acc)
].

:- instance add(command_in_state) where [
    add(Command, !Acc) :-
    (
        Command = command_any(X),
        add(X, !Acc)
    ;
        Command = command_auth(_),
        sorry($module, $pred, "command_auth")
    ;
        Command = command_nonauth(X),
        add(X, !Acc)
    ;
        Command = command_select(_),
        sorry($module, $pred, "command_select")
    )
].

:- instance add(command_any) where [
    add(capability, !Acc) :-
    (
        add("CAPABILITY", !Acc)
    ),
    add(logout, !Acc) :-
    (
        add("LOGOUT", !Acc)
    ),
    add(noop, !Acc) :-
    (
        add("NOOP", !Acc)
    )
].

:- instance add(command_nonauth) where [
    add(login(UserId, Password), !Acc) :-
    (
        add("LOGIN", !Acc),
        add(sp, !Acc),
        add(UserId, !Acc),
        add(sp, !Acc),
        add(Password, !Acc)
    )
].

:- instance add(astring) where [
    add(AString, !Acc) :-
    (
        AString = astring(S),
        add(S, !Acc)
    ;
        AString = imap_string(IString),
        add(IString, !Acc)
    )
].

:- instance add(imap_string) where [
    add(String, !Acc) :-
    (
        String = quoted(Q),
        add(dquote, !Acc),
        add(escape_for_quoted_string(Q), !Acc),
        add(dquote, !Acc)
    ;
        String = literal(L),
        BraceCount = string.format("{%d}", [i(count_code_units(L))]),
        add(BraceCount, !Acc),
        add(crlf, !Acc), % Need to wait for server response.
        add(L, !Acc)
    )
].

:- func escape_for_quoted_string(string) = string.

escape_for_quoted_string(S0) = S :-
    string.replace_all(S0, "\\", "\\\\", S1),
    string.replace_all(S1, """", "\\\"", S).

crlf = "\r\n".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
