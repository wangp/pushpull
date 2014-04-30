%-----------------------------------------------------------------------------%

:- module imap.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type imap.

:- type username
    --->    username(string).

:- type password
    --->    password(string).

:- type mailbox.

:- type imap_result
    --->    result(imap_res, string, list(alert)).

:- type imap_res
    --->    ok
    ;       no
    ;       bad
    ;       bye
    ;       error.

:- type alert
    --->    alert(string).

    % open("host:port", Res, Alerts)
    %
:- pred open(string::in, maybe_error(imap)::out, list(alert)::out,
    io::di, io::uo) is det.

:- pred login(imap::in, username::in, imap.password::in, imap_result::out,
    io::di, io::uo) is det.

:- pred logout(imap::in, imap_result::out, io::di, io::uo) is det.

:- func mailbox(string) = mailbox.

:- pred examine(imap::in, mailbox::in, imap_result::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module require.
:- import_module store.
:- import_module string.

:- include_module imap.charclass.
:- include_module imap.command.
:- include_module imap.io.
:- include_module imap.parsing.
:- include_module imap.response.
:- include_module imap.types.

:- import_module imap.command.
:- import_module imap.io.
:- import_module imap.parsing.
:- import_module imap.response.
:- import_module imap.types.
:- import_module subprocess.

:- type imap
    --->    imap(
                pipe :: io_mutvar(pipe),
                tag_counter :: io_mutvar(int),
                imap_state :: io_mutvar(imap_state)
            ).

:- type pipe
    --->    open(subprocess)
    ;       closed.

:- type imap_state
    --->    imap_state(
                connection_state :: connection_state,
                capabilities :: maybe(capability_data),
                selected :: maybe(selected_mailbox)
            ).

:- type connection_state
    --->    not_authenticated
    ;       authenticated
    ;       selected
    ;       logout.

:- type selected_mailbox
    --->    selected_mailbox(
                selected_mailbox :: command.mailbox,
                access :: access,
                flags :: list(flag),
                % Defined flags in the mailbox.
                exists :: integer,
                % The number of messages in the mailbox.
                recent :: integer,
                % The number of messages with the \Recent flag set.
                unseen :: maybe(message_seq_nr),
                % If this is missing, the client can not make any
                % assumptions about the first unseen message in the
                % mailbox.
                permanent_flags :: maybe(permanent_flags),
                % If this is missing, the client should assume that
                % all flags can be changed permanently.
                uidnext :: maybe(uid),
                % If this is missing, the client can not make any
                % assumptions about the next unique identifier
                % value.
                uidvalidity :: maybe(uidvalidity)
                % If this is missing, the server does not support
                % unique identifiers.
            ).

:- type access
    --->    read_only
    ;       read_write.

:- type mailbox == command.mailbox.

%-----------------------------------------------------------------------------%

open(HostPort, Res, Alerts, !IO) :-
    subprocess.spawn("/usr/bin/openssl",
        ["s_client", "-quiet", "-connect", HostPort], ResSpawn, !IO),
    (
        ResSpawn = ok(Proc),
        Pipe = open(Proc),
        wait_for_greeting(Pipe, ResGreeting, !IO),
        (
            ResGreeting = ok(Greeting),
            (
                Greeting = ok(RespText),
                handle_greeting_resp_text(RespText, MaybeCaps, Alerts),
                make_imap(Proc, not_authenticated, MaybeCaps, IMAP, !IO),
                Res = ok(IMAP)
            ;
                Greeting = preauth(RespText),
                handle_greeting_resp_text(RespText, MaybeCaps, Alerts),
                make_imap(Proc, authenticated, MaybeCaps, IMAP, !IO),
                Res = ok(IMAP)
            ;
                Greeting = bye(RespText),
                handle_greeting_resp_text(RespText, _MaybeCaps, Alerts),
                close_pipes(Proc, !IO),
                wait_pid(Proc, blocking, _WaitRes, !IO),
                Res = error("greeted with BYE")
            )
        ;
            ResGreeting = error(Error),
            close_pipes(Proc, !IO),
            wait_pid(Proc, blocking, _WaitRes, !IO),
            Res = error(Error),
            Alerts = []
        )
    ;
        ResSpawn = error(Error),
        Res = error(Error),
        Alerts = []
    ).

:- pred wait_for_greeting(pipe::in, maybe_error(greeting)::out,
    io::di, io::uo) is det.

wait_for_greeting(Pipe, Res, !IO) :-
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

:- pred handle_greeting_resp_text(resp_text::in, maybe(capability_data)::out,
    list(alert)::out) is det.

handle_greeting_resp_text(RespText, MaybeCaps, Alerts) :-
    RespText = resp_text(MaybeResponseCode, Text),
    (
        MaybeResponseCode = yes(ResponseCode),
        (
            ResponseCode = alert,
            Alerts = [alert(Text)],
            MaybeCaps = no
        ;
            ResponseCode = capability_data(Caps),
            MaybeCaps = yes(Caps),
            Alerts = []
        ;
            ( ResponseCode = badcharset(_)
            ; ResponseCode = parse
            ; ResponseCode = permanent_flags(_)
            ; ResponseCode = read_only
            ; ResponseCode = read_write
            ; ResponseCode = trycreate
            ; ResponseCode = uidnext(_)
            ; ResponseCode = uidvalidity(_)
            ; ResponseCode = unseen(_)
            ; ResponseCode = other(_, _)
            ),
            MaybeCaps = no,
            Alerts = []
        )
    ;
        MaybeResponseCode = no,
        MaybeCaps = no,
        Alerts = []
    ).

:- pred make_imap(subprocess::in, connection_state::in,
    maybe(capability_data)::in, imap::out, io::di, io::uo) is det.

make_imap(Proc, ConnState, MaybeCaps, IMAP, !IO) :-
    store.new_mutvar(open(Proc), PipeMutvar, !IO),
    store.new_mutvar(1, TagMutvar, !IO),
    store.new_mutvar(imap_state(ConnState, MaybeCaps, no), StateMutvar, !IO),
    IMAP = imap(PipeMutvar, TagMutvar, StateMutvar).

%-----------------------------------------------------------------------------%

:- pred command_wrapper(pred(imap, imap_result, io, io),
    list(connection_state), imap, imap_result, io, io).
:- mode command_wrapper(in(pred(in, out, di, uo) is det),
    in, in, out, di, uo) is det.

command_wrapper(Pred, ValidConnectionStates, IMAP, Res, !IO) :-
    get_connection_state(IMAP, ConnState, !IO),
    ( list.member(ConnState, ValidConnectionStates) ->
        Pred(IMAP, Res, !IO),
        close_pipe_on_logout(IMAP, !IO)
    ;
        Res = wrong_state_result(ConnState)
    ).

:- pred close_pipe_on_logout(imap::in, io::di, io::uo) is det.

close_pipe_on_logout(IMAP, !IO) :-
    get_connection_state(IMAP, ConnStateAfter, !IO),
    (
        ConnStateAfter = logout,
        IMAP = imap(PipeMutvar, _TagMutvar, _StateMutvar),
        get_mutvar(PipeMutvar, MaybePipe, !IO),
        (
            MaybePipe = open(Proc),
            close_pipes(Proc, !IO),
            wait_pid(Proc, blocking, _WaitRes, !IO),
            set_mutvar(PipeMutvar, closed, !IO)
        ;
            MaybePipe = closed
        )
    ;
        ( ConnStateAfter = not_authenticated
        ; ConnStateAfter = authenticated
        ; ConnStateAfter = selected
        )
    ).

:- pred get_connection_state(imap::in, connection_state::out, io::di, io::uo)
    is det.

get_connection_state(IMAP, ConnState, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State, !IO),
    ConnState = State ^ connection_state.

:- pred get_new_tag(imap::in, pipe::out, tag::out, io::di, io::uo) is det.

get_new_tag(IMAP, Pipe, tag(Tag), !IO) :-
    IMAP = imap(PipeMutvar, TagMutvar, _StateMutvar),
    get_mutvar(PipeMutvar, Pipe, !IO),
    get_mutvar(TagMutvar, N, !IO),
    set_mutvar(TagMutvar, N + 1, !IO),
    Tag = string.from_int(N).

:- pred get_capabilities(imap::in, maybe(capability_data)::out, io::di, io::uo)
    is det.

get_capabilities(IMAP, MaybeCaps, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State, !IO),
    MaybeCaps = State ^ capabilities.

:- pred update_state(pred(T, imap_state, imap_state, A, A, io, io),
    imap, T, A, A, io, io).
:- mode update_state(in(pred(in, in, out, in, out, di, uo) is det),
    in, in, in, out, di, uo) is det.

update_state(Pred, IMAP, X, !Acc, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    Pred(X, State0, State, !Acc, !IO),
    set_mutvar(StateMutvar, State, !IO).

:- pred make_result(tagged_response_or_bye::in, resp_text::in, list(alert)::in,
    imap_result::out) is det.

make_result(MaybeTagCond, RespText, Alerts, Result) :-
    (
        MaybeTagCond = tagged(_, ok),
        Res = ok
    ;
        MaybeTagCond = tagged(_, no),
        Res = no
    ;
        MaybeTagCond = tagged(_, bad),
        Res = bad
    ;
        MaybeTagCond = bye,
        Res = bye
    ),
    RespText = resp_text(_MaybeResponseCode, Text),
    Result = result(Res, Text, Alerts).

:- func error_result(string) = imap_result.

error_result(Text) = result(error, Text, []).

:- func wrong_state_result(connection_state) = imap_result.

wrong_state_result(not_authenticated) =
    error_result("command invalid in Not Authenticated state").
wrong_state_result(authenticated) =
    error_result("command invalid in Authenticated state").
wrong_state_result(selected) =
    error_result("command invalid in Selected state").
wrong_state_result(logout) =
    error_result("command invalid in Logout state").

%-----------------------------------------------------------------------------%

:- pred capability(imap::in, imap_result::out, io::di, io::uo) is det.

capability(IMAP, Res, !IO) :-
    command_wrapper(do_capability,
        [not_authenticated, authenticated, selected, logout],
        IMAP, Res, !IO).

:- pred do_capability(imap::in, imap_result::out, io::di, io::uo) is det.

do_capability(IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    make_command_stream(Tag - command_any(capability), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_complete_response, IMAP, Response, [], Alerts,
                !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            MaybeResponse = error(Error),
            Res = result(error, Error, [])
        )
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

%-----------------------------------------------------------------------------%

login(IMAP, UserName, Password, Res, !IO) :-
    get_capabilities(IMAP, MaybeCaps0, !IO),
    (
        MaybeCaps0 = yes(Caps),
        check_login(IMAP, Caps, UserName, Password, Res, !IO)
    ;
        MaybeCaps0 = no,
        capability(IMAP, Res0, !IO),
        (
            % I guess we should try to preserve Alerts0 through to the login.
            Res0 = result(ok, _Text, _Alerts0)
        ->
            get_capabilities(IMAP, MaybeCaps, !IO),
            (
                MaybeCaps = yes(Caps)
            ;
                MaybeCaps = no,
                Caps = []
            ),
            check_login(IMAP, Caps, UserName, Password, Res, !IO)
        ;
            Res = Res0
        )
    ).

:- pred check_login(imap::in, capability_data::in, username::in, password::in,
    imap_result::out, io::di, io::uo) is det.

check_login(IMAP, Caps, UserName, Password, Res, !IO) :-
    (
        list.member(atom("AUTH=PLAIN"), Caps),
        not list.member(atom("LOGINDISABLED"), Caps)
    ->
        do_login(UserName, Password, IMAP, Res, !IO)
    ;
        Res = error_result("cannot login due to capabilities")
    ).

:- pred do_login(username::in, password::in, imap::in, imap_result::out,
    io::di, io::uo) is det.

do_login(username(UserName), password(Password), IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    Login = login(make_astring(UserName), make_astring(Password)),
    make_command_stream(Tag - command_nonauth(Login), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_login_response, IMAP, Response, [], Alerts,
                !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            MaybeResponse = error(Error),
            Res = result(error, Error, [])
        )
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

:- pred apply_login_response(complete_response::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_login_response(Response, !State, !Alerts, !IO) :-
    apply_complete_response(Response, !State, !Alerts, !IO),
    Response = complete_response(_, FinalMaybeTag, _),
    (
        FinalMaybeTag = tagged(_, ok),
        !State ^ connection_state := authenticated
    ;
        FinalMaybeTag = tagged(_, no)
    ;
        FinalMaybeTag = tagged(_, bad)
    ;
        FinalMaybeTag = bye,
        !State ^ connection_state := logout
    ).

%-----------------------------------------------------------------------------%

logout(IMAP, Res, !IO) :-
    command_wrapper(do_logout, [not_authenticated, authenticated, selected],
        IMAP, Res, !IO).

:- pred do_logout(imap::in, imap_result::out, io::di, io::uo) is det.

do_logout(IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    make_command_stream(Tag - command_any(logout), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_logout_response, IMAP, Response, [], Alerts,
                !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            MaybeResponse = error(Error),
            Res = result(error, Error, [])
        )
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

:- pred apply_logout_response(complete_response::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_logout_response(Response, !State, !Alerts, !IO) :-
    apply_complete_response(Response, !State, !Alerts, !IO),
    Response = complete_response(_, FinalMaybeTag, _),
    (
        FinalMaybeTag = tagged(_, ok),
        !State ^ connection_state := logout
    ;
        FinalMaybeTag = tagged(_, no)
    ;
        FinalMaybeTag = tagged(_, bad)
    ;
        FinalMaybeTag = bye,
        !State ^ connection_state := logout
    ).

%-----------------------------------------------------------------------------%

mailbox(S) =
    ( string.to_upper(S, "INBOX") ->
        inbox
    ;
        astring(make_astring(S))
    ).

examine(IMAP, Mailbox, Res, !IO) :-
    command_wrapper(do_examine(Mailbox), [authenticated, selected],
        IMAP, Res, !IO).

:- pred do_examine(command.mailbox::in, imap::in, imap_result::out,
    io::di, io::uo) is det.

do_examine(Mailbox, IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    make_command_stream(Tag - command_auth(examine(Mailbox)), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_examine_response(Mailbox), IMAP, Response,
                [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            MaybeResponse = error(Error),
            Res = result(error, Error, [])
        )
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

:- pred apply_examine_response(command.mailbox::in, complete_response::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_examine_response(Mailbox, Response, !State, !Alerts, !IO) :-
    Response = complete_response(_, FinalMaybeTag, _FinalRespText),
    (
        FinalMaybeTag = tagged(_, ok),
        !State ^ connection_state := selected,
        !State ^ selected := yes(new_selected_mailbox(Mailbox))
    ;
        FinalMaybeTag = tagged(_, no),
        !State ^ connection_state := authenticated,
        !State ^ selected := no
    ;
        FinalMaybeTag = tagged(_, bad)
    ;
        FinalMaybeTag = bye,
        !State ^ connection_state := logout
    ),
    apply_complete_response(Response, !State, !Alerts, !IO).

:- func new_selected_mailbox(command.mailbox) = selected_mailbox.

new_selected_mailbox(Mailbox) =
    selected_mailbox(Mailbox, read_only, [], zero, zero, no, no, no, no).

%-----------------------------------------------------------------------------%

:- pred wait_for_complete_response(pipe::in, tag::in,
    maybe_error(complete_response)::out, io::di, io::uo) is det.

wait_for_complete_response(Pipe, Tag, Res, !IO) :-
    wait_for_complete_response_2(Pipe, Tag, [], Res, !IO).

:- pred wait_for_complete_response_2(pipe::in, tag::in,
    list(untagged_response_data)::in, maybe_error(complete_response)::out,
    io::di, io::uo) is det.

wait_for_complete_response_2(Pipe, Tag, RevUntagged0, Res, !IO) :-
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        trace [runtime(env("DEBUG_IMAP")), io(!IO2)] (
            ( string.from_code_unit_list(Bytes, String) ->
                Stream = io.stderr_stream,
                io.write_string(Stream, String, !IO2),
                io.nl(Stream, !IO2)
            ;
                true
            )
        ),
        parse_response_single(Bytes, ParseResult),
        (
            ParseResult = ok(continue_req(_)),
            Res = error("unexpected continue request")
        ;
            ParseResult = ok(untagged(ResponseData)),
            RevUntagged = [ResponseData | RevUntagged0],
            wait_for_complete_response_2(Pipe, Tag, RevUntagged, Res, !IO)
        ;
            ParseResult = ok(tagged(ResponseTag, Cond, RespText)),
            ( Tag = ResponseTag ->
                list.reverse(RevUntagged0, Untagged),
                Response = complete_response(Untagged,
                    tagged(Tag, Cond), RespText),
                Res = ok(Response)
            ;
                % XXX handle mismatched tagged response
                sorry($module, $pred, "mismatching tagged response")
            )
        ;
            ParseResult = error(Error),
            Res = error(Error)
        )
    ;
        ResRead = eof,
        % Should we check for BYE in non-last position?
        (
            RevUntagged0 = [Last | RevUntagged1],
            Last = cond_or_bye(bye, ByeRespText)
        ->
            list.reverse(RevUntagged1, Untagged),
            Res = ok(complete_response(Untagged, bye, ByeRespText))
        ;
            Res = error("unexpected eof")
        )
    ;
        ResRead = error(Error),
        % XXX do we need to do anything with the RevUntagged0?
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
        Res = error("failed to parse response"),
        trace [runtime(env("DEBUG_IMAP")), io(!IO)] (
            ( string.from_code_unit_list(Input, String) ->
                Stream = io.stderr_stream,
                io.write_string(Stream, String, !IO),
                io.nl(Stream, !IO)
            ;
                true
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred apply_complete_response(complete_response::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_complete_response(Response, !State, !Alerts, !IO) :-
    Response = complete_response(UntaggedResponses, FinalMaybeTag,
        FinalRespText),
    apply_untagged_responses(UntaggedResponses, !State, !Alerts),
    apply_cond_or_bye(cond_bye_1(FinalMaybeTag), FinalRespText,
        !State, !Alerts).

:- pred apply_untagged_responses(list(untagged_response_data)::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out) is det.

apply_untagged_responses(ResponseData, !State, !Alerts) :-
    list.foldl2(apply_untagged_response, ResponseData, !State, !Alerts).

:- pred apply_untagged_response(untagged_response_data::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out) is det.

apply_untagged_response(ResponseData, !State, !Alerts) :-
    (
        ResponseData = cond_or_bye(Cond, RespText),
        apply_cond_or_bye(Cond, RespText, !State, !Alerts)
    ;
        ResponseData = mailbox_data(MailboxData),
        apply_mailbox_data(MailboxData, !State)
    ;
        ResponseData = capability_data(_),
        sorry($module, $pred, "capability_data")
    ).

:- pred apply_cond_or_bye(cond_bye::in, resp_text::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out) is det.

apply_cond_or_bye(Cond, RespText, !State, !Alerts) :-
    (
        RespText = resp_text(yes(ResponseCode), Text),
        apply_cond_or_bye_2(Cond, ResponseCode, Text, !State, !Alerts)
    ;
        RespText = resp_text(no, _Text)
    ),
    (
        ( Cond = ok
        ; Cond = no
        ; Cond = bad
        )
    ;
        Cond = bye,
        !State ^ connection_state := logout
    ).

:- pred apply_cond_or_bye_2(cond_bye::in, resp_text_code::in, string::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out) is det.

apply_cond_or_bye_2(Cond, ResponseCode, Text, !State, !Alerts) :-
    (
        ResponseCode = alert,
        cons(alert(Text), !Alerts)
    ;
        ResponseCode = capability_data(Caps),
        (
            Cond = ok,
            !State ^ capabilities := yes(Caps)
        ;
            Cond = no
        ;
            Cond = bad
        ;
            Cond = bye
        )
    ;
        ( ResponseCode = unseen(_)
        ; ResponseCode = permanent_flags(_)
        ; ResponseCode = read_only
        ; ResponseCode = read_write
        ; ResponseCode = uidnext(_)
        ; ResponseCode = uidvalidity(_)
        ; ResponseCode = other(_, _)
        ),
        (
            Cond = ok,
            !.State ^ selected = yes(Sel0),
            apply_selected_mailbox_response_code(ResponseCode, Sel0, Sel),
            !State ^ selected := yes(Sel)
        ;
            Cond = ok,
            !.State ^ selected = no
        ;
            Cond = no
        ;
            Cond = bad
        ;
            Cond = bye
        )
    ;
        ( ResponseCode = badcharset(_)
        ; ResponseCode = parse
        ; ResponseCode = trycreate
        )
    ).

:- pred apply_mailbox_data(mailbox_data::in, imap_state::in, imap_state::out)
    is det.

apply_mailbox_data(_MailboxData, State, State) :-
    State ^ selected = no.
apply_mailbox_data(MailboxData, State0, State) :-
    State0 ^ selected = yes(Sel0),
    (
        MailboxData = flags(Flags),
        Sel = Sel0 ^ flags := Flags
    ;
        MailboxData = exists(Exists),
        % This is not supposed to decrease except after EXPUNGE.
        Sel = Sel0 ^ exists := Exists
    ;
        MailboxData = recent(Recent),
        Sel = Sel0 ^ recent := Recent
    ;
        ( MailboxData = list(_)
        ; MailboxData = lsub(_)
        ; MailboxData = search(_)
        ; MailboxData = status(_, _)
        ),
        sorry($module, $pred, "MailboxData=" ++ string(MailboxData))
    ),
    State = State0 ^ selected := yes(Sel).

:- pred apply_selected_mailbox_response_code(
    resp_text_code::in(mailbox_response_code),
    selected_mailbox::in, selected_mailbox::out) is det.

apply_selected_mailbox_response_code(ResponseCode, !Sel) :-
    (
        ResponseCode = unseen(Unseen),
        !Sel ^ unseen := yes(Unseen)
    ;
        ResponseCode = permanent_flags(PermanentFlags),
        !Sel ^ permanent_flags := yes(PermanentFlags)
    ;
        ResponseCode = read_only,
        !Sel ^ access := read_only
    ;
        ResponseCode = read_write,
        !Sel ^ access := read_write
    ;
        ResponseCode = uidnext(UID),
        !Sel ^ uidnext := yes(UID)
    ;
        ResponseCode = uidvalidity(UIDValidity),
        !Sel ^ uidvalidity := yes(UIDValidity)
    ;
        ResponseCode = other(Atom, _MaybeString),
        ( Atom = atom("HIGHESTMODSEQ") ->
            % TODO
            true
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
