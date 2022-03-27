% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module imap.
:- interface.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.

:- include_module imap.charclass.
:- include_module imap.date_time.
:- include_module imap.oauth2.      % exported for testing only
:- include_module imap.types.

:- import_module binary_string.
:- import_module imap.types.
:- import_module maybe_result.
:- import_module openssl.

%-----------------------------------------------------------------------------%

:- type imap.

:- type username
    --->    username(string).

:- type password
    --->    password(string).

:- type oauth2_access_token
    --->    oauth2_access_token(string).

:- type imap_result == maybe_result(imap_res).

:- type imap_res
    --->    result(status, string, list(alert)).

:- type status
    --->    ok
    ;       no
    ;       bad
    ;       bye
    ;       continue.

:- type imap_result(T) == maybe_result(imap_res(T)).

:- type imap_res(T)
    --->    result(status(T), string, list(alert)).

:- type status(T)
    --->    ok_with_data(T)
    ;       no
    ;       bad
    ;       bye
    ;       continue.

:- type alert
    --->    alert(string).

:- type highestmodseq
    --->    unknown
    ;       nomodseq
    ;       highestmodseq(mod_seq_value).

    % The ownership of the BIO handle is transferred.
    %
:- pred open(openssl.bio::in, maybe_error(imap)::out, list(alert)::out,
    io::di, io::uo) is det.

:- pred close(imap::in, io::di, io::uo) is det.

:- pred login(imap::in, username::in, imap.password::in, imap_result::out,
    io::di, io::uo) is det.

:- pred authenticate_oauth2(imap::in, string::in, int::in, username::in,
    oauth2_access_token::in, imap_result::out, io::di, io::uo) is det.

:- pred noop(imap::in, imap_result::out, io::di, io::uo) is det.

:- pred logout(imap::in, imap_result::out, io::di, io::uo) is det.

:- func mailbox(string) = mailbox.

:- pred select(imap::in, mailbox::in, list(select_param)::in,
    imap_result::out, io::di, io::uo) is det.

:- pred examine(imap::in, mailbox::in, list(select_param)::in,
    imap_result::out, io::di, io::uo) is det.

:- pred get_selected_mailbox_uidvalidity(imap::in, maybe(uidvalidity)::out,
    io::di, io::uo) is det.

:- pred get_selected_mailbox_highest_modseqvalue(imap::in,
    maybe(highestmodseq)::out, io::di, io::uo) is det.

:- pred update_selected_mailbox_highest_modseqvalue_from_fetches(imap::in,
    io::di, io::uo) is det.

:- pred clear_exists_seen_flag(imap::in, bool::out, io::di, io::uo) is det.

:- pred set_expunge_seen_flag(imap::in, bool::in, io::di, io::uo) is det.
:- pred read_expunge_seen_flag(imap::in, bool::out, io::di, io::uo) is det.
:- pred clear_expunge_seen_flag(imap::in, bool::out, io::di, io::uo) is det.

:- pred append(imap::in, mailbox::in, list(flag)::in, maybe(date_time)::in,
    binary_string::in, imap_result(maybe(appenduid))::out, io::di, io::uo)
    is det.

:- type uid_search_result
    --->    uid_search_result(
                uids :: list(uid),
                search_highestmodseq :: maybe(mod_seq_value),
                return_datas :: list(search_return_data(uid))
            ).

:- pred uid_search(imap::in, search_key::in,
    maybe(list(search_return_option))::in, imap_result(uid_search_result)::out,
    io::di, io::uo) is det.

:- pred uid_fetch(imap::in, sequence_set(uid)::in, fetch_items::in,
    maybe(fetch_modifier)::in,
    imap_result(assoc_list(message_seq_nr, msg_atts))::out, io::di, io::uo)
    is det.

:- pred uid_store(imap::in, sequence_set(uid)::in, store_operation::in,
    store_silence::in, list(flag)::in,
    imap_result(assoc_list(message_seq_nr, msg_atts))::out, io::di, io::uo)
    is det.

    % Expected result is continue, not ok.
    %
:- pred idle(imap::in, imap_result::out, io::di, io::uo) is det.

:- type stop_or_continue
    --->    stop_idling
    ;       continue_idling
    ;       bye.

    % Read an untagged message that the server may send while in IDLE state.
    %
:- pred read_single_idle_response(imap::in,
    maybe_result(pair(stop_or_continue, list(alert)))::out, io::di, io::uo)
    is det.

    % Must call this after successful idle,
    % before issuing another command.
    %
:- pred idle_done(imap::in, imap_result::out, io::di, io::uo) is det.

:- pred get_read_filedes(imap::in, maybe_error(int)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module require.
:- import_module store.
:- import_module string.
:- import_module unit.

:- include_module imap.command.
:- include_module imap.parsing.
:- include_module imap.read_write.
:- include_module imap.response.

:- import_module imap.command.
:- import_module imap.oauth2.
:- import_module imap.parsing.
:- import_module imap.read_write.
:- import_module imap.response.

:- type imap
    --->    imap(
                pipe :: io_mutvar(pipe),
                tag_counter :: io_mutvar(int),
                imap_state :: io_mutvar(imap_state)
            ).

:- type pipe
    --->    open(bio)
    ;       closed.

:- type imap_state
    --->    imap_state(
                connection_state :: connection_state,
                capabilities :: maybe(capability_data),
                selected :: maybe(selected_mailbox),
                expunge_seen :: bool
            ).

:- type connection_state
    --->    not_authenticated
    ;       authenticated
    ;       selected
    ;       logout
    ;       idle_authenticated(tag)
    ;       idle_selected(tag).

:- type selected_mailbox
    --->    selected_mailbox(
                selected_mailbox :: mailbox,
                access :: access,
                % Defined flags in the mailbox.
                flags :: list(flag),
                % The number of messages in the mailbox.
                exists :: integer,
                exists_seen :: bool,
                % The number of messages with the \Recent flag set.
                recent :: integer,
                % If this is missing, the client can not make any
                % assumptions about the first unseen message in the
                % mailbox.
                unseen :: maybe(message_seq_nr),
                % If this is missing, the client should assume that
                % all flags can be changed permanently.
                permanent_flags :: maybe(permanent_flags),
                % If this is missing, the client can not make any
                % assumptions about the next unique identifier
                % value.
                uidnext :: maybe(uid),
                % If this is missing, the server does not support
                % unique identifiers.
                uidvalidity :: maybe(uidvalidity),
                highestmodseq :: highestmodseq,

                % [RFC 5162] Whenever the client receives a tagged response to
                % a command, it calculates the highest value among all MODSEQ
                % FETCH data items received since the last tagged response.
                % If this value is bigger than the client's copy of the
                % HIGHESTMODSEQ value, then the client MUST use this value as
                % its new HIGHESTMODSEQ value.
                %
                % Note: It is not safe to update the client's copy of the
                % HIGHESTMODSEQ value with a MODSEQ FETCH data item value as
                % soon as it is received because servers are not required to
                % send MODSEQ FETCH data items in increasing modseqence order.
                % This can lead to the client missing some changes in case of
                % connectivity loss.
                fetch_highestmodseq :: maybe(mod_seq_value)
            ).

:- type access
    --->    read_only
    ;       read_write.

:- typeclass handle_results(T) where [
    pred handle_search_results(list(integer)::in, maybe(mod_seq_value)::in,
        T::in, T::out) is det,
    pred handle_esearch_msgseqnrs(maybe(tag)::in,
        list(search_return_data(message_seq_nr))::in, T::in, T::out) is det,
    pred handle_esearch_uids(maybe(tag)::in,
        list(search_return_data(uid))::in, T::in, T::out) is det,
    pred handle_fetch_results(message_seq_nr::in, msg_atts::in, T::in, T::out)
        is det,
    pred handle_appenduid(appenduid::in, T::in, T::out) is det
].

%-----------------------------------------------------------------------------%

open(Bio, Res, Alerts, !IO) :-
    wait_for_greeting(open(Bio), ResGreeting, !IO),
    (
        ResGreeting = ok(Greeting),
        (
            Greeting = ok(RespText),
            handle_greeting_resp_text(RespText, MaybeCaps, Alerts),
            make_imap(Bio, not_authenticated, MaybeCaps, IMAP, !IO),
            Res = ok(IMAP)
        ;
            Greeting = preauth(RespText),
            handle_greeting_resp_text(RespText, MaybeCaps, Alerts),
            make_imap(Bio, authenticated, MaybeCaps, IMAP, !IO),
            Res = ok(IMAP)
        ;
            Greeting = bye(RespText),
            handle_greeting_resp_text(RespText, _MaybeCaps, Alerts),
            Res = error("greeted with BYE")
        )
    ;
        ResGreeting = eof,
        Res = error("unexpected eof in " ++ $pred),
        Alerts = []
    ;
        ResGreeting = error(Error),
        Res = error(Error),
        Alerts = []
    ),
    (
        Res = ok(_)
    ;
        Res = error(_),
        bio_destroy(Bio, !IO)
    ).

:- pred wait_for_greeting(pipe::in, maybe_result(greeting)::out,
    io::di, io::uo) is det.

wait_for_greeting(Pipe, Res, !IO) :-
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        Src = src(Pipe),
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
        Res = eof
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
            ; ResponseCode = highestmodseq(_)
            ; ResponseCode = nomodseq
            ; ResponseCode = appenduid(_)
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

:- pred make_imap(bio::in, connection_state::in,
    maybe(capability_data)::in, imap::out, io::di, io::uo) is det.

make_imap(Bio, ConnState, MaybeCaps, IMAP, !IO) :-
    store.new_mutvar(open(Bio), PipeMutvar, !IO),
    store.new_mutvar(1, TagMutvar, !IO),
    store.new_mutvar(imap_state(ConnState, MaybeCaps, no, no), StateMutvar,
        !IO),
    IMAP = imap(PipeMutvar, TagMutvar, StateMutvar).

%-----------------------------------------------------------------------------%

close(IMAP, !IO) :-
    close_pipe(IMAP, !IO).

:- pred close_pipe(imap::in, io::di, io::uo) is det.

close_pipe(IMAP, !IO) :-
    IMAP = imap(PipeMutvar, _TagMutvar, _StateMutvar),
    get_mutvar(PipeMutvar, MaybePipe, !IO),
    (
        MaybePipe = open(Bio),
        bio_destroy(Bio, !IO),
        set_mutvar(PipeMutvar, closed, !IO)
    ;
        MaybePipe = closed
    ).

%-----------------------------------------------------------------------------%

:- pred command_wrapper_low(pred(imap, T, io, io),
    list(connection_state), imap, maybe_error(T), io, io).
:- mode command_wrapper_low(in(pred(in, out, di, uo) is det),
    in, in, out, di, uo) is det.

command_wrapper_low(Pred, ValidConnectionStates, IMAP, MaybeRes, !IO) :-
    get_connection_state(IMAP, ConnState, !IO),
    ( list.member(ConnState, ValidConnectionStates) ->
        Pred(IMAP, Res, !IO),
        MaybeRes = ok(Res),
        close_pipe_on_logout(IMAP, !IO)
    ;
        MaybeRes = error(wrong_state_message(ConnState))
    ).

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
        Res = error(wrong_state_message(ConnState))
    ).

:- pred close_pipe_on_logout(imap::in, io::di, io::uo) is det.

close_pipe_on_logout(IMAP, !IO) :-
    get_connection_state(IMAP, ConnStateAfter, !IO),
    (
        ConnStateAfter = logout,
        close_pipe(IMAP, !IO)
    ;
        ( ConnStateAfter = not_authenticated
        ; ConnStateAfter = authenticated
        ; ConnStateAfter = selected
        ; ConnStateAfter = idle_authenticated(_)
        ; ConnStateAfter = idle_selected(_)
        )
    ).

:- pred get_connection_state(imap::in, connection_state::out, io::di, io::uo)
    is det.

get_connection_state(IMAP, ConnState, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State, !IO),
    ConnState = State ^ connection_state.

:- pred get_pipe(imap::in, pipe::out, io::di, io::uo) is det.

get_pipe(IMAP, Pipe, !IO) :-
    IMAP = imap(PipeMutvar, _TagMutvar, _StateMutvar),
    get_mutvar(PipeMutvar, Pipe, !IO).

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

:- pred update_state(pred(T, U, imap_state, imap_state, A, A, io, io),
    imap, T, U, A, A, io, io).
:- mode update_state(in(pred(in, out, in, out, in, out, di, uo) is det),
    in, in, out, in, out, di, uo) is det.

update_state(Pred, IMAP, X, Y, !A, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    Pred(X, Y, State0, State, !A, !IO),
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
    ;
        MaybeTagCond = continue,
        Res = continue
    ),
    RespText = resp_text(_MaybeResponseCode, Text),
    Result = ok(result(Res, Text, Alerts)).

:- func wrong_state_message(connection_state) = string.

wrong_state_message(not_authenticated) =
    "command invalid in Not Authenticated state".
wrong_state_message(authenticated) = "command invalid in Authenticated state".
wrong_state_message(selected) = "command invalid in Selected state".
wrong_state_message(logout) = "command invalid in Logout state".
wrong_state_message(idle_authenticated(_)) = "command invalid while idling".
wrong_state_message(idle_selected(_)) = "command invalid while idling".

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
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_capability_response, IMAP, Response, _ : unit,
                [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_capability_response(complete_response::in, unit::out,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_capability_response(Response, unit, !State, !Alerts, !IO) :-
    apply_complete_response(Response, !State, !Alerts, !IO).

%-----------------------------------------------------------------------------%

login(IMAP, UserName, Password, Res, !IO) :-
    get_capabilities(IMAP, MaybeCaps0, !IO),
    (
        MaybeCaps0 = yes(Caps),
        login_with_capabilities(IMAP, Caps, UserName, Password, Res, !IO)
    ;
        MaybeCaps0 = no,
        capability(IMAP, Res0, !IO),
        (
            % I guess we should try to preserve Alerts0 through to the login.
            Res0 = ok(result(ok, _Text, _Alerts0))
        ->
            get_capabilities(IMAP, MaybeCaps, !IO),
            (
                MaybeCaps = yes(Caps)
            ;
                MaybeCaps = no,
                Caps = []
            ),
            login_with_capabilities(IMAP, Caps, UserName, Password, Res, !IO)
        ;
            Res = Res0
        )
    ).

:- pred login_with_capabilities(imap::in, capability_data::in, username::in,
    password::in, imap_result::out, io::di, io::uo) is det.

login_with_capabilities(IMAP, Caps, UserName, Password, Res, !IO) :-
    ( list.contains(Caps, atom("LOGINDISABLED")) ->
        Res = error("server has disabled plaintext login")
    ; list.contains(Caps, atom("AUTH=PLAIN")) ->
        do_login(UserName, Password, IMAP, Res, !IO)
    ;
        Res = error("server does not support plaintext login")
    ).

:- pred do_login(username::in, password::in, imap::in, imap_result::out,
    io::di, io::uo) is det.

do_login(username(UserName), password(Password), IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    Login = login(make_astring(UserName), make_astring(Password)),
    make_command_stream(Tag - command_nonauth(Login), CommandStream),
    write_command_stream_sensitive(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_login_response, IMAP, Response, _ : unit,
                [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_login_response(complete_response::in, unit::out,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_login_response(Response, unit, !State, !Alerts, !IO) :-
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
        ( FinalMaybeTag = bye
        ; FinalMaybeTag = continue % unexpected
        ),
        !State ^ connection_state := logout
    ).

%-----------------------------------------------------------------------------%

authenticate_oauth2(IMAP, Host, Port, UserName, AccessToken, Res, !IO) :-
    get_capabilities(IMAP, MaybeCaps0, !IO),
    (
        MaybeCaps0 = yes(Caps),
        authenticate_oauth2_with_capabilities(IMAP, Caps, Host, Port,
            UserName, AccessToken, Res, !IO)
    ;
        MaybeCaps0 = no,
        capability(IMAP, Res0, !IO),
        ( Res0 = ok(result(ok, _Text, _Alerts0)) ->
            get_capabilities(IMAP, MaybeCaps, !IO),
            (
                MaybeCaps = yes(Caps)
            ;
                MaybeCaps = no,
                Caps = []
            ),
            authenticate_oauth2_with_capabilities(IMAP, Caps, Host, Port,
                UserName, AccessToken, Res, !IO)
        ;
            Res = Res0
        )
    ).

:- pred authenticate_oauth2_with_capabilities(imap::in, list(capability)::in,
    string::in, int::in, username::in, oauth2_access_token::in,
    imap_result::out, io::di, io::uo) is det.

authenticate_oauth2_with_capabilities(IMAP, Caps, Host, Port, UserName,
        AccessToken, Res, !IO) :-
    ( not list.contains(Caps, atom("SASL-IR")) ->
        Res = error("missing SASL-IR capability")
    ; list.contains(Caps, atom("AUTH=OAUTHBEARER")) ->
        make_oauthbearer_sasl_string(Host, Port, UserName, AccessToken,
            SASLString),
        do_authenticate(IMAP, "OAUTHBEARER", SASLString, Res, !IO)
    ; list.contains(Caps, atom("AUTH=XOAUTH2")) ->
        make_xoauth2_sasl_string(UserName, AccessToken, SASLString),
        do_authenticate(IMAP, "XOAUTH2", SASLString, Res, !IO)
    ;
        Res = error("server does not support OAuth2 authentication")
    ).

:- pred do_authenticate(imap::in, string::in, sasl_string::in,
    imap_result::out, io::di, io::uo) is det.

do_authenticate(IMAP, AuthMechName, SASLString, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    SASLString = sasl_string(InitialClientResponse),
    Authenticate = authenticate(astring(AuthMechName),
        astring(InitialClientResponse)),
    make_command_stream(Tag - command_nonauth(Authenticate), CommandStream),
    write_command_stream_sensitive(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_authenticate_response, IMAP, Response, _ : unit,
                [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_authenticate_response(complete_response::in, unit::out,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_authenticate_response(Response, unit, !State, !Alerts, !IO) :-
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
        ( FinalMaybeTag = bye
        ; FinalMaybeTag = continue
        ),
        !State ^ connection_state := logout
    ).

%-----------------------------------------------------------------------------%

noop(IMAP, Res, !IO) :-
    command_wrapper(do_noop, [not_authenticated, authenticated, selected],
        IMAP, Res, !IO).

:- pred do_noop(imap::in, imap_result::out, io::di, io::uo) is det.

do_noop(IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    make_command_stream(Tag - command_any(noop), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_noop_response, IMAP, Response, _ : unit,
                [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_noop_response(complete_response::in, unit::out,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_noop_response(Response, unit, !State, !Alerts, !IO) :-
    apply_complete_response(Response, !State, !Alerts, !IO),
    Response = complete_response(_, FinalMaybeTag, _),
    (
        FinalMaybeTag = tagged(_, ok)
    ;
        FinalMaybeTag = tagged(_, no)
    ;
        FinalMaybeTag = tagged(_, bad)
    ;
        ( FinalMaybeTag = bye
        ; FinalMaybeTag = continue % unexpected
        ),
        !State ^ connection_state := logout
    ).

%-----------------------------------------------------------------------------%

logout(IMAP, Res, !IO) :-
    command_wrapper(do_logout, [not_authenticated, authenticated, selected],
        IMAP, Res, !IO),
    close_pipe(IMAP, !IO).

:- pred do_logout(imap::in, imap_result::out, io::di, io::uo) is det.

do_logout(IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    make_command_stream(Tag - command_any(logout), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_logout_response, IMAP, Response, _ : unit,
                [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_logout_response(complete_response::in, unit::out,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_logout_response(Response, unit, !State, !Alerts, !IO) :-
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
        ( FinalMaybeTag = bye
        ; FinalMaybeTag = continue % unexpected
        ),
        !State ^ connection_state := logout
    ).

%-----------------------------------------------------------------------------%

mailbox(S) =
    ( string.to_upper(S, "INBOX") ->
        inbox
    ;
        astring(make_astring(S))
    ).

select(IMAP, Mailbox, Params, Res, !IO) :-
    command_wrapper(do_select_or_examine(yes, Mailbox, Params),
        [authenticated, selected], IMAP, Res, !IO).

examine(IMAP, Mailbox, Params, Res, !IO) :-
    command_wrapper(do_select_or_examine(no, Mailbox, Params),
        [authenticated, selected], IMAP, Res, !IO).

:- pred do_select_or_examine(bool::in, mailbox::in, list(select_param)::in,
    imap::in, imap_result::out, io::di, io::uo) is det.

do_select_or_examine(DoSelect, Mailbox, Params, IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    (
        DoSelect = yes,
        Command = select(Mailbox, Params)
    ;
        DoSelect = no,
        Command = examine(Mailbox, Params)
    ),
    make_command_stream(Tag - command_auth(Command), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_select_or_examine_response(Mailbox),
                IMAP, Response, _ : unit, [], Alerts, !IO),
            Response = complete_response(_, FinalMaybeTag, FinalRespText),
            make_result(FinalMaybeTag, FinalRespText, Alerts, Res)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_select_or_examine_response(mailbox::in, complete_response::in,
    unit::out, imap_state::in, imap_state::out,
    list(alert)::in, list(alert)::out, io::di, io::uo) is det.

apply_select_or_examine_response(Mailbox, Response, unit, !State, !Alerts, !IO)
        :-
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
        ( FinalMaybeTag = bye
        ; FinalMaybeTag = continue % unexpected
        ),
        !State ^ connection_state := logout
    ),
    apply_complete_response(Response, !State, !Alerts, !IO).

:- func new_selected_mailbox(mailbox) = selected_mailbox.

new_selected_mailbox(Mailbox) =
    selected_mailbox(Mailbox, read_only, [], zero, no, zero, no, no, no, no,
        unknown, no).

%-----------------------------------------------------------------------------%

get_selected_mailbox_uidvalidity(IMAP, MaybeUIDValidity, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State, !IO),
    MaybeSel = State ^ selected,
    (
        MaybeSel = yes(Sel),
        MaybeUIDValidity = Sel ^ uidvalidity
    ;
        MaybeSel = no,
        MaybeUIDValidity = no
    ).

%-----------------------------------------------------------------------------%

get_selected_mailbox_highest_modseqvalue(IMAP, Res, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State, !IO),
    MaybeSel = State ^ selected,
    (
        MaybeSel = yes(Sel),
        Res = yes(Sel ^ highestmodseq)
    ;
        MaybeSel = no,
        Res = no
    ).

    % XXX maybe we can do this implicitly now
update_selected_mailbox_highest_modseqvalue_from_fetches(IMAP, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    some [!Sel]
    (
        State0 ^ selected = yes(!:Sel),
        !.Sel ^ fetch_highestmodseq = yes(FetchModSeq)
    ->
        (
            !.Sel ^ highestmodseq = unknown,
            !Sel ^ highestmodseq := highestmodseq(FetchModSeq)
        ;
            !.Sel ^ highestmodseq = nomodseq,
            % Weird.
            !Sel ^ highestmodseq := highestmodseq(FetchModSeq)
        ;
            !.Sel ^ highestmodseq = highestmodseq(PrevHighest),
            ( FetchModSeq > PrevHighest ->
                !Sel ^ highestmodseq := highestmodseq(FetchModSeq)
            ;
                true
            )
        ),
        !Sel ^ fetch_highestmodseq := no,
        State = State0 ^ selected := yes(!.Sel),
        set_mutvar(StateMutvar, State, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

clear_exists_seen_flag(IMAP, WasExistsSeen, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    (
        State0 ^ selected = yes(Sel0),
        WasExistsSeen = Sel0 ^ exists_seen,
        Sel = Sel0 ^ exists_seen := no,
        State = State0 ^ selected := yes(Sel),
        set_mutvar(StateMutvar, State, !IO)
    ;
        State0 ^ selected = no,
        WasExistsSeen = no
    ).

%-----------------------------------------------------------------------------%

set_expunge_seen_flag(IMAP, ExpungeSeen, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    State = State0 ^ expunge_seen := ExpungeSeen,
    set_mutvar(StateMutvar, State, !IO).

read_expunge_seen_flag(IMAP, WasExpungeSeen, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    WasExpungeSeen = State0 ^ expunge_seen.

clear_expunge_seen_flag(IMAP, WasExpungeSeen, !IO) :-
    IMAP = imap(_PipeMutvar, _TagMutvar, StateMutvar),
    get_mutvar(StateMutvar, State0, !IO),
    WasExpungeSeen = State0 ^ expunge_seen,
    State = State0 ^ expunge_seen := no,
    set_mutvar(StateMutvar, State, !IO).

%-----------------------------------------------------------------------------%

append(IMAP, Mailbox, Flags, MaybeDateTime, Content, Res, !IO) :-
    command_wrapper_low(do_append(Mailbox, Flags, MaybeDateTime, Content),
        [authenticated, selected], IMAP, MaybeRes, !IO),
    (
        MaybeRes = ok(Res)
    ;
        MaybeRes = error(Error),
        Res = error(Error)
    ).

:- pred do_append(mailbox::in, list(flag)::in, maybe(date_time)::in,
    binary_string::in, imap::in, imap_result(maybe(appenduid))::out,
    io::di, io::uo) is det.

do_append(Mailbox, Flags, MaybeDateTime, Content, IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    Command = append(Mailbox, Flags, MaybeDateTime, Content),
    make_command_stream(Tag - command_auth(Command), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_append_response, IMAP, Response, Res,
                unit, _ : unit, !IO)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_append_response(complete_response::in,
    imap_result(maybe(appenduid))::out, imap_state::in, imap_state::out,
    unit::in, unit::out, io::di, io::uo) is det.

apply_append_response(Response, Result, !State, unit, unit, !IO) :-
    apply_complete_response(Response, !State, [], Alerts,
        accept_appenduid(no), accept_appenduid(MaybeAppendUID), !IO),
    Response = complete_response(_, FinalMaybeTag, FinalRespText),
    (
        FinalMaybeTag = tagged(_, ok),
        Status = ok_with_data(MaybeAppendUID)
    ;
        FinalMaybeTag = tagged(_, no),
        Status = no
    ;
        FinalMaybeTag = tagged(_, bad),
        Status = bad
    ;
        FinalMaybeTag = bye,
        Status = bye
    ;
        FinalMaybeTag = continue,
        Status = continue
    ),
    FinalRespText = resp_text(_MaybeResponseCode, Text),
    Result = ok(result(Status, Text, Alerts)).

%-----------------------------------------------------------------------------%

uid_search(IMAP, SearchKey, MaybeResultOptions, Res, !IO) :-
    command_wrapper_low(do_uid_search(SearchKey, MaybeResultOptions),
        [selected], IMAP, MaybeRes, !IO),
    (
        MaybeRes = ok(Res)
    ;
        MaybeRes = error(Error),
        Res = error(Error)
    ).

:- pred do_uid_search(search_key::in, maybe(list(search_return_option))::in,
    imap::in, imap_result(uid_search_result)::out, io::di, io::uo) is det.

do_uid_search(SearchKey, MaybeResultOptions, IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    Command = uid_search(search(no, SearchKey, MaybeResultOptions)),
    make_command_stream(Tag - command_select(Command), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_uid_search_response, IMAP, Response, Res,
                unit, _ : unit, !IO)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_uid_search_response(complete_response::in,
    imap_result(uid_search_result)::out, imap_state::in, imap_state::out,
    unit::in, unit::out, io::di, io::uo) is det.

apply_uid_search_response(Response, Result, !State, unit, unit, !IO) :-
    apply_complete_response(Response, !State, [], Alerts,
        uid_search_result([], no, []), SearchResult, !IO),
    Response = complete_response(_, FinalMaybeTag, FinalRespText),
    (
        FinalMaybeTag = tagged(_, ok),
        Status = ok_with_data(SearchResult)
    ;
        FinalMaybeTag = tagged(_, no),
        Status = no
    ;
        FinalMaybeTag = tagged(_, bad),
        Status = bad
    ;
        FinalMaybeTag = bye,
        Status = bye
    ;
        FinalMaybeTag = continue,
        Status = continue
    ),
    FinalRespText = resp_text(_MaybeResponseCode, Text),
    Result = ok(result(Status, Text, Alerts)).

%-----------------------------------------------------------------------------%

uid_fetch(IMAP, SequenceSet, Items, MaybeModifier, Res, !IO) :-
    command_wrapper_low(do_uid_fetch(SequenceSet, Items, MaybeModifier),
        [selected], IMAP, MaybeRes, !IO),
    (
        MaybeRes = ok(Res)
    ;
        MaybeRes = error(Error),
        Res = error(Error)
    ).

:- pred do_uid_fetch(sequence_set(uid)::in, fetch_items::in,
    maybe(fetch_modifier)::in, imap::in,
    imap_result(assoc_list(message_seq_nr, msg_atts))::out, io::di, io::uo)
    is det.

do_uid_fetch(SequenceSet, Items, MaybeModifier, IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    Command = command_select(uid_fetch(SequenceSet, Items, MaybeModifier)),
    make_command_stream(Tag - Command, CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_uid_fetch_response, IMAP, Response, Result,
                unit, _ : unit, !IO),
            Res = ok(Result)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_uid_fetch_response(complete_response::in,
    imap_res(assoc_list(message_seq_nr, msg_atts))::out,
    imap_state::in, imap_state::out, unit::in, unit::out, io::di, io::uo)
    is det.

apply_uid_fetch_response(Response, Result, !State, unit, unit, !IO) :-
    apply_complete_response(Response, !State, [], Alerts,
        accept_fetch_results([]), accept_fetch_results(RevFetchResults), !IO),
    Response = complete_response(_, FinalMaybeTag, FinalRespText),
    (
        FinalMaybeTag = tagged(_, ok),
        list.reverse(RevFetchResults, FetchResults),
        Status = ok_with_data(FetchResults)
    ;
        FinalMaybeTag = tagged(_, no),
        Status = no
    ;
        FinalMaybeTag = tagged(_, bad),
        Status = bad
    ;
        FinalMaybeTag = bye,
        Status = bye
    ;
        FinalMaybeTag = continue,
        Status = continue
    ),
    FinalRespText = resp_text(_MaybeResponseCode, Text),
    Result = result(Status, Text, Alerts).

%-----------------------------------------------------------------------------%

uid_store(IMAP, SequenceSet, Operation, Silence, Flags, Res, !IO) :-
    command_wrapper_low(do_uid_store(SequenceSet, Operation, Silence, Flags),
        [selected], IMAP, MaybeRes, !IO),
    (
        MaybeRes = ok(Res)
    ;
        MaybeRes = error(Error),
        Res = error(Error)
    ).

:- pred do_uid_store(sequence_set(uid)::in, store_operation::in,
    store_silence::in, list(flag)::in, imap::in,
    imap_result(assoc_list(message_seq_nr, msg_atts))::out, io::di, io::uo)
    is det.

do_uid_store(SequenceSet, Operation, Silence, Flags, IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    Command = uid_store(SequenceSet, Operation, Silence, Flags),
    make_command_stream(Tag - command_select(Command), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_uid_fetch_response, IMAP, Response, Result,
                unit, _ : unit, !IO),
            Res = ok(Result)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

%-----------------------------------------------------------------------------%

idle(IMAP, Res, !IO) :-
    command_wrapper_low(do_idle, [authenticated, selected], IMAP, Res0, !IO),
    (
        Res0 = ok(Res)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred do_idle(imap::in, imap_result::out, io::di, io::uo) is det.

do_idle(IMAP, Res, !IO) :-
    get_new_tag(IMAP, Pipe, Tag, !IO),
    make_command_stream(Tag - command_auth(idle), CommandStream),
    write_command_stream(Pipe, Tag, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_idle_response(Tag), IMAP, Response, Result,
                unit, _ : unit, !IO),
            Res = ok(Result)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_idle_response(tag::in, complete_response::in, imap_res::out,
    imap_state::in, imap_state::out, unit::in, unit::out, io::di, io::uo)
    is det.

apply_idle_response(Tag, Response, Result, !State, unit, unit, !IO) :-
    apply_complete_response(Response, !State, [], Alerts, !IO),
    Response = complete_response(_, FinalMaybeTag, FinalRespText),
    (
        % Unexpected response.
        FinalMaybeTag = tagged(_, ok),
        Status = ok
    ;
        FinalMaybeTag = tagged(_, no),
        Status = no
    ;
        FinalMaybeTag = tagged(_, bad),
        Status = bad
    ;
        FinalMaybeTag = bye,
        Status = bye
    ;
        % Expected response.
        FinalMaybeTag = continue,
        Status = continue,
        ConnState = !.State ^ connection_state,
        (
            ConnState = authenticated,
            !State ^ connection_state := idle_authenticated(Tag)
        ;
            ConnState = selected,
            !State ^ connection_state := idle_selected(Tag)
        ;
            ( ConnState = not_authenticated
            ; ConnState = logout
            ; ConnState = idle_authenticated(_)
            ; ConnState = idle_selected(_)
            ),
            unexpected($module, $pred, "unexpected connection state")
        )
    ),
    FinalRespText = resp_text(_MaybeResponseCode, Text),
    Result = result(Status, Text, Alerts).

%-----------------------------------------------------------------------------%

read_single_idle_response(IMAP, Res, !IO) :-
    get_pipe(IMAP, Pipe, !IO),
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        parse_response_single(Pipe, Bytes, ParseResult, !IO),
        (
            ParseResult = yes(continue_req(_)),
            Res = error("unexpected continue request")
        ;
            ParseResult = yes(untagged(ResponseData)),
            update_state(apply_idle_untagged_response, IMAP, ResponseData,
                Stop - Alerts, unit, _ : unit, !IO),
            Res = ok(Stop - Alerts)
        ;
            ParseResult = yes(tagged(_, _, _)),
            Res = error("unexpected tagged response")
        ;
            ParseResult = no,
            Res = error("failed to parse response")
        )
    ;
        ResRead = eof,
        Res = eof
    ;
        ResRead = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred apply_idle_untagged_response(untagged_response_data::in,
    pair(stop_or_continue, list(alert))::out, imap_state::in, imap_state::out,
    unit::in, unit::out, io::di, io::uo) is det.

apply_idle_untagged_response(ResponseData, Stop - Alerts, !State, !R, !IO) :-
    apply_untagged_response(ResponseData, !State, [], Alerts, !R),
    ( stop_idling(ResponseData, StopPrime) ->
        Stop = StopPrime
    ;
        Stop = continue_idling
    ).

:- pred stop_idling(untagged_response_data::in, stop_or_continue::out)
    is semidet.

stop_idling(ResponseData, Stop) :-
    require_complete_switch [ResponseData]
    (
        ResponseData = cond_or_bye(Cond, _),
        require_complete_switch [Cond]
        (
            Cond = ok,
            % Dovecot sends "OK Still here" which we can ignore.
            fail
        ;
            Cond = no,
            Stop = stop_idling
        ;
            Cond = bad,
            Stop = stop_idling
        ;
            Cond = bye,
            Stop = bye
        ;
            Cond = continue, % unexpected
            fail
        )
    ;
        ( ResponseData = mailbox_data(_)
        ; ResponseData = message_data(_)
        ; ResponseData = capability_data(_)
        ),
        Stop = stop_idling
    ).

%-----------------------------------------------------------------------------%

idle_done(IMAP, Res, !IO) :-
    get_connection_state(IMAP, ConnState, !IO),
    (
        (
            ConnState = idle_authenticated(Tag),
            ResumeConnState = authenticated
        ;
            ConnState = idle_selected(Tag),
            ResumeConnState = selected
        ),
        do_idle_done(IMAP, Tag, ResumeConnState, Res, !IO),
        close_pipe_on_logout(IMAP, !IO)
    ;
        ( ConnState = not_authenticated
        ; ConnState = authenticated
        ; ConnState = selected
        ; ConnState = logout
        ),
        Res = error(wrong_state_message(ConnState))
    ).

:- pred do_idle_done(imap::in, tag::in, connection_state::in,
    imap_result::out, io::di, io::uo) is det.

do_idle_done(IMAP, Tag, ResumeConnState, Res, !IO) :-
    get_pipe(IMAP, Pipe, !IO),
    Sensitive = no,
    write_command_stream_inner(Pipe, Tag, idle_done_command_stream, Sensitive,
        Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(Pipe, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(Response),
            update_state(apply_idle_done_response(ResumeConnState),
                IMAP, Response, Result, unit, _ : unit, !IO),
            Res = ok(Result)
        ;
            ( MaybeResponse = eof
            ; MaybeResponse = error(_)
            ),
            Res = convert(MaybeResponse)
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- pred apply_idle_done_response(connection_state::in, complete_response::in,
    imap_res::out, imap_state::in, imap_state::out, unit::in, unit::out,
    io::di, io::uo) is det.

apply_idle_done_response(ResumeConnState, Response, Result, !State, unit, unit,
        !IO) :-
    apply_complete_response(Response, !State, [], Alerts, !IO),
    Response = complete_response(_, FinalMaybeTag, FinalRespText),
    (
        (
            FinalMaybeTag = tagged(_, ok),
            Status = ok
        ;
            FinalMaybeTag = tagged(_, no),
            Status = no
        ;
            FinalMaybeTag = tagged(_, bad),
            Status = bad
        ),
        !State ^ connection_state := ResumeConnState
    ;
        FinalMaybeTag = bye,
        Status = bye
    ;
        FinalMaybeTag = continue, % unexpected
        Status = continue
    ),
    FinalRespText = resp_text(_MaybeResponseCode, Text),
    Result = result(Status, Text, Alerts).

%-----------------------------------------------------------------------------%

get_read_filedes(IMAP, Res, !IO) :-
    get_pipe(IMAP, Pipe, !IO),
    (
        Pipe = open(Bio),
        bio_get_fd(Bio, Res, !IO)
    ;
        Pipe = closed,
        Res = error("connection closed")
    ).

%-----------------------------------------------------------------------------%

:- pred wait_for_complete_response(pipe::in, tag::in,
    maybe_result(complete_response)::out, io::di, io::uo) is det.

wait_for_complete_response(Pipe, Tag, Res, !IO) :-
    wait_for_complete_response_2(Pipe, Tag, [], Res, !IO).

:- pred wait_for_complete_response_2(pipe::in, tag::in,
    list(untagged_response_data)::in, maybe_result(complete_response)::out,
    io::di, io::uo) is det.

wait_for_complete_response_2(Pipe, Tag, RevUntagged0, Res, !IO) :-
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        parse_response_single(Pipe, Bytes, ParseResult, !IO),
        (
            ParseResult = yes(continue_req(ContinueReq)),
            list.reverse(RevUntagged0, Untagged),
            ContinueReq = continue_req_resp_text(RespText),
            Response = complete_response(Untagged, continue, RespText),
            Res = ok(Response)
        ;
            ParseResult = yes(untagged(ResponseData)),
            RevUntagged = [ResponseData | RevUntagged0],
            wait_for_complete_response_2(Pipe, Tag, RevUntagged, Res, !IO)
        ;
            ParseResult = yes(tagged(ResponseTag, Cond, RespText)),
            ( Tag = ResponseTag ->
                list.reverse(RevUntagged0, Untagged),
                Response = complete_response(Untagged,
                    tagged(Tag, Cond), RespText),
                Res = ok(Response)
            ;
                % XXX handle mismatched tagged response
                % Note that if we stopped reading at a parse error then there
                % will server responses due to a preceding command in the
                % buffer, which is a source of mismatching tags.
                sorry($module, $pred, "mismatching tagged response")
            )
        ;
            ParseResult = no,
            Res = error("failed to parse response")
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
            Res = eof
        )
    ;
        ResRead = error(Error),
        % XXX do we need to do anything with the RevUntagged0?
        Res = error(error_message(Error))
    ).

%-----------------------------------------------------------------------------%

:- pred apply_complete_response(complete_response::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    io::di, io::uo) is det.

apply_complete_response(Response, !State, !Alerts, !IO) :-
    apply_complete_response(Response, !State, !Alerts, unit, _ : unit, !IO).

:- pred apply_complete_response(complete_response::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    R::in, R::out, io::di, io::uo) is det <= handle_results(R).

apply_complete_response(Response, !State, !Alerts, !R, !IO) :-
    Response = complete_response(UntaggedResponses, FinalMaybeTag,
        FinalRespText),
    apply_untagged_responses(UntaggedResponses, !State, !Alerts, !R),
    apply_cond_or_bye(cond_bye_1(FinalMaybeTag), FinalRespText,
        !State, !Alerts, !R).

:- pred apply_untagged_responses(list(untagged_response_data)::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    R::in, R::out) is det <= handle_results(R).

apply_untagged_responses(ResponseData, !State, !Alerts, !R) :-
    list.foldl3(apply_untagged_response, ResponseData, !State, !Alerts, !R).

:- pred apply_untagged_response(untagged_response_data::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    R::in, R::out) is det <= handle_results(R).

apply_untagged_response(ResponseData, !State, !Alerts, !R) :-
    (
        ResponseData = cond_or_bye(Cond, RespText),
        apply_cond_or_bye(Cond, RespText, !State, !Alerts, !R)
    ;
        ResponseData = mailbox_data(MailboxData),
        apply_mailbox_data(MailboxData, !State, !R)
    ;
        ResponseData = message_data(MessageData),
        apply_message_data(MessageData, !State, !R)
    ;
        ResponseData = capability_data(Caps),
        !State ^ capabilities := yes(Caps)
    ).

:- pred apply_cond_or_bye(cond_bye::in, resp_text::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    R::in, R::out) is det <= handle_results(R).

apply_cond_or_bye(Cond, RespText, !State, !Alerts, !R) :-
    (
        RespText = resp_text(yes(ResponseCode), Text),
        apply_cond_or_bye_2(Cond, ResponseCode, Text, !State, !Alerts, !R)
    ;
        RespText = resp_text(no, _Text)
    ),
    (
        ( Cond = ok
        ; Cond = no
        ; Cond = bad
        ; Cond = continue
        )
    ;
        Cond = bye,
        !State ^ connection_state := logout
    ).

:- pred apply_cond_or_bye_2(cond_bye::in, resp_text_code::in, string::in,
    imap_state::in, imap_state::out, list(alert)::in, list(alert)::out,
    R::in, R::out) is det <= handle_results(R).

apply_cond_or_bye_2(Cond, ResponseCode, Text, !State, !Alerts, !R) :-
    (
        ResponseCode = alert,
        cons(alert(Text), !Alerts)
    ;
        ResponseCode = capability_data(Caps),
        (
            Cond = ok,
            !State ^ capabilities := yes(Caps)
        ;
            ( Cond = no
            ; Cond = bad
            ; Cond = bye
            ; Cond = continue
            )
        )
    ;
        ( ResponseCode = unseen(_)
        ; ResponseCode = permanent_flags(_)
        ; ResponseCode = read_only
        ; ResponseCode = read_write
        ; ResponseCode = uidnext(_)
        ; ResponseCode = uidvalidity(_)
        ; ResponseCode = highestmodseq(_)
        ; ResponseCode = nomodseq
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
            ( Cond = no
            ; Cond = bad
            ; Cond = bye
            ; Cond = continue
            )
        )
    ;
        ResponseCode = appenduid(AppendUID),
        handle_appenduid(AppendUID, !R)
    ;
        ( ResponseCode = badcharset(_)
        ; ResponseCode = parse
        ; ResponseCode = trycreate
        ; ResponseCode = other(_, _)
        )
    ).

:- pred apply_mailbox_data(mailbox_data::in, imap_state::in, imap_state::out,
    R::in, R::out) is det <= handle_results(R).

apply_mailbox_data(_MailboxData, State, State, !R) :-
    State ^ selected = no.
apply_mailbox_data(MailboxData, State0, State, !R) :-
    State0 ^ selected = yes(Sel0),
    (
        MailboxData = flags(Flags),
        Sel = Sel0 ^ flags := Flags
    ;
        MailboxData = exists(Exists),
        % This is not supposed to decrease except after EXPUNGE.
        Sel1 = Sel0 ^ exists := Exists,
        Sel = Sel1 ^ exists_seen := yes
    ;
        MailboxData = recent(Recent),
        Sel = Sel0 ^ recent := Recent
    ;
        MailboxData = search(Numbers, MaybeModSeqValue),
        handle_search_results(Numbers, MaybeModSeqValue, !R),
        Sel = Sel0
    ;
        MailboxData = esearch_msgseqnrs(MaybeTag, ReturnData),
        handle_esearch_msgseqnrs(MaybeTag, ReturnData, !R),
        Sel = Sel0
    ;
        MailboxData = esearch_uids(MaybeTag, ReturnData),
        handle_esearch_uids(MaybeTag, ReturnData, !R),
        Sel = Sel0
    ;
        ( MailboxData = list(_)
        ; MailboxData = lsub(_)
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
        ResponseCode = highestmodseq(ModSeqValue),
        !Sel ^ highestmodseq := highestmodseq(ModSeqValue)
    ;
        ResponseCode = nomodseq,
        !Sel ^ highestmodseq := nomodseq
    ).

:- pred apply_message_data(message_data::in, imap_state::in, imap_state::out,
    R::in, R::out) is det <= handle_results(R).

apply_message_data(MessageData, !State, !R) :-
    (
        MessageData = expunge(_),
        (
            !.State ^ selected = yes(Sel0),
            decrement_exists(Sel0, Sel),
            !State ^ selected := yes(Sel)
        ;
            !.State ^ selected = no
        ),
        !State ^ expunge_seen := yes
    ;
        MessageData = fetch(MsgSeqNr, Atts),
        (
            !.State ^ selected = yes(Sel0),
            list.foldl(bump_fetch_highestmodseq, Atts, Sel0, Sel),
            !State ^ selected := yes(Sel)
        ;
            !.State ^ selected = no
        ),
        handle_fetch_results(MsgSeqNr, Atts, !R)
    ).

:- pred decrement_exists(selected_mailbox::in, selected_mailbox::out) is det.

decrement_exists(Sel0, Sel) :-
    Exists = Sel0 ^ exists,
    ( Exists > one ->
        Sel = Sel0 ^ exists := Exists - one
    ;
        % Shouldn't happen.
        Sel = Sel0
    ).

:- pred bump_fetch_highestmodseq(msg_att::in,
    selected_mailbox::in, selected_mailbox::out) is det.

bump_fetch_highestmodseq(Att, !Sel) :-
    (
        Att = modseq(ModSeqValue),
        (
            !.Sel ^ fetch_highestmodseq = no
        ;
            !.Sel ^ fetch_highestmodseq = yes(Old),
            ModSeqValue > Old
        )
    ->
        !Sel ^ fetch_highestmodseq := yes(ModSeqValue)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- instance handle_results(uid_search_result) where [
    handle_search_results(Numbers, MaybeModSeqValue, !Results) :-
    (
        !Results ^ uids := map(from_nz_number, Numbers),
        !Results ^ search_highestmodseq := MaybeModSeqValue
    ),
    handle_esearch_uids(_MaybeTag, ReturnDatas, !Results) :-
    (
        !.Results ^ return_datas = ReturnDatas0,
        !Results ^ return_datas := ReturnDatas0 ++ ReturnDatas
    ),

    handle_esearch_msgseqnrs(_, _, !Results),
    handle_fetch_results(_, _, !Results),
    handle_appenduid(_, !Results)
].

%-----------------------------------------------------------------------------%

:- type accept_fetch_results
    --->    accept_fetch_results(assoc_list(message_seq_nr, msg_atts)).

:- instance handle_results(accept_fetch_results) where [
    handle_search_results(_, _, !Results),
    handle_esearch_msgseqnrs(_, _, !Results),
    handle_esearch_uids(_, _, !Results),

    handle_fetch_results(MsgSeqNr, Atts,
        accept_fetch_results(Results0), accept_fetch_results(Results)) :-
        cons(MsgSeqNr - Atts, Results0, Results),

    handle_appenduid(_, !Results)
].

%-----------------------------------------------------------------------------%

:- type accept_appenduid
    --->    accept_appenduid(maybe(appenduid)).

:- instance handle_results(accept_appenduid) where [
    handle_search_results(_, _, !Results),
    handle_esearch_msgseqnrs(_, _, !Results),
    handle_esearch_uids(_, _, !Results),
    handle_fetch_results(_, _, !Results),
    handle_appenduid(AppendUID, _, accept_appenduid(yes(AppendUID)))
].

%-----------------------------------------------------------------------------%

:- instance handle_results(unit) where [
    handle_search_results(_, _, !Results),
    handle_esearch_msgseqnrs(_, _, !Results),
    handle_esearch_uids(_, _, !Results),
    handle_fetch_results(_, _, !Results),
    handle_appenduid(_, !Results)
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
