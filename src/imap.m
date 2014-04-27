%-----------------------------------------------------------------------------%

:- module imap.
:- interface.

:- import_module io.
:- import_module maybe.

    % for resp_text at the moment
:- include_module imap.response.
:- import_module imap.response.

%-----------------------------------------------------------------------------%

:- type imap.

:- type username
    --->    username(string).

:- type password
    --->    password(string).

:- type imap_result
    --->    ok(resp_text)
    ;       no(resp_text)
    ;       bad(resp_text)
    ;       fatal(resp_text)
    ;       error(string).

    % open("host:port", Res)
    %
:- pred open(string::in, maybe_error(imap)::out, io::di, io::uo) is det.

:- pred login(imap::in, username::in, imap.password::in, imap_result::out,
    io::di, io::uo) is det.

:- pred logout(imap::in, imap_result::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module store.
:- import_module string.

:- include_module imap.charclass.
:- include_module imap.command.
:- include_module imap.io.
:- include_module imap.parsing.
:- include_module imap.types.

:- import_module imap.command.
:- import_module imap.io.
:- import_module imap.parsing.
:- import_module imap.types.
:- import_module subprocess.

:- type imap
    --->    imap(
                pipe        :: subprocess,
                tag_counter :: io_mutvar(int)
            ).

%-----------------------------------------------------------------------------%

open(HostPort, Res, !IO) :-
    subprocess.spawn("/usr/bin/openssl",
        ["s_client", "-quiet", "-connect", HostPort], ResSpawn, !IO),
    (
        ResSpawn = ok(Proc),
        make_imap(Proc, IMAP, !IO),
        wait_for_greeting(IMAP, ResGreeting, !IO),
        (
            ResGreeting = ok(Greeting),
            (
                Greeting = ok(_RespText), % XXX use this
                Res = ok(IMAP)
            ;
                Greeting = preauth(_RespText), % XXX use this
                Res = ok(IMAP)
            ;
                Greeting = bye(_RespText), % XXX use this
                close_pipes(Proc, !IO),
                wait_pid(Proc, blocking, _WaitRes, !IO),
                Res = error("greeted with BYE")
            )
        ;
            ResGreeting = error(Error),
            close_pipes(Proc, !IO),
            wait_pid(Proc, blocking, _WaitRes, !IO),
            Res = error(Error)
        )
    ;
        ResSpawn = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred make_imap(subprocess::in, imap::out, io::di, io::uo) is det.

make_imap(Proc, imap(Proc, TagMutvar), !IO) :-
    store.new_mutvar(1, TagMutvar, !IO).

:- pred get_new_tag(imap::in, tag::out, io::di, io::uo) is det.

get_new_tag(imap(_Proc, TagMutvar), tag(Tag), !IO) :-
    get_mutvar(TagMutvar, N, !IO),
    set_mutvar(TagMutvar, N + 1, !IO),
    Tag = string.from_int(N).

%-----------------------------------------------------------------------------%

login(IMAP, username(UserName), password(Password), Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar),
    % XXX check capabilities first
    get_new_tag(IMAP, Tag, !IO),
    Login = login(make_astring(UserName), make_astring(Password)),
    make_command_stream(Tag - command_nonauth(Login), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_response_done(IMAP, Tag, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred wait_for_greeting(imap::in, maybe_error(greeting)::out, io::di, io::uo)
    is det.

wait_for_greeting(IMAP, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar),
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        Src = src,
        PS0 = Bytes,
        (
            greeting(Src, Greeting, PS0, PS),
            eof(Src, PS)
        ->
            Res = ok(Greeting)
        ;
            Res = error("expected server greeting")
        )
    ;
        ResRead = eof,
        Res = error("unexpected eof")
    ;
        ResRead = error(Error),
        Res = error(io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%

logout(IMAP, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar),
    get_new_tag(IMAP, Tag, !IO),
    make_command_stream(Tag - command_any(logout), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_response_done(IMAP, Tag, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred wait_for_response_done(imap::in, tag::in, imap_result::out,
    io::di, io::uo) is det.

wait_for_response_done(IMAP, Tag, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar),
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        parse_response_single(Bytes, ParseResult),
        (
            ParseResult = ok(ResponseSingle),
            (
                ResponseSingle = continue_req(_),
                Res = error("unexpected continue request")
            ;
                ResponseSingle = response_data(_ResponseData),
                % XXX Handle ResponseData.
                wait_for_response_done(IMAP, Tag, Res, !IO)
            ;
                ResponseSingle = response_done(ResponseDone),
                ResponseDone = response_tagged(ResponseTag, Cond,
                    ResponseText),
                ( Tag = ResponseTag ->
                    (
                        Cond = ok,
                        Res = ok(ResponseText)
                    ;
                        Cond = no,
                        Res = no(ResponseText)
                    ;
                        Cond = bad,
                        Res = bad(ResponseText)
                    )
                ;
                    % XXX handle mismatched tagged response
                    wait_for_response_done(IMAP, Tag, Res, !IO)
                )
            )
        ;
            ParseResult = error(Error),
            Res = error(Error)
        )
    ;
        ResRead = eof,
        Res = error("unexpected eof")
    ;
        ResRead = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred parse_response_single(list(int)::in, maybe_error(response_single)::out)
    is det.

parse_response_single(Input, Res) :-
    Src = src,
    PS0 = Input,
    (
        response_single(Src, Response, PS0, PS),
        eof(Src, PS)
    ->
        Res = ok(Response)
    ;
        Res = error("failed to parse response")
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
