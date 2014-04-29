%-----------------------------------------------------------------------------%

:- module imap.
:- interface.

:- import_module io.
:- import_module list.
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

:- type mailbox.

:- type imap_result_ll
    --->    ok(resp_text)
    ;       no(resp_text)
    ;       bad(resp_text)
    ;       fatal(resp_text)
    ;       error(string).

:- type imap_result
    --->    ok(string)
    ;       no(string)
    ;       bad(string)
    ;       fatal(string)
    ;       error(string).

:- type alert
    --->    alert(string).

    % open("host:port", Res)
    %
:- pred open(string::in, maybe_error(imap)::out, io::di, io::uo) is det.

:- pred login(imap::in, username::in, imap.password::in, imap_result::out,
    list(alert)::out, io::di, io::uo) is det.

:- pred logout(imap::in, imap_result_ll::out, io::di, io::uo) is det.

:- func mailbox(string) = mailbox.

:- pred examine(imap::in, mailbox::in, imap_result::out, list(alert)::out,
    io::di, io::uo) is det.

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
:- include_module imap.types.

:- import_module imap.command.
:- import_module imap.io.
:- import_module imap.parsing.
:- import_module imap.types.
:- import_module subprocess.

:- type imap
    --->    imap(
                pipe :: subprocess,
                tag_counter :: io_mutvar(int),
                capabilities :: io_mutvar(maybe(capability_data)),
                selected :: io_mutvar(maybe(selected_mailbox))
            ).

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

:- pred make_imap(subprocess::in, imap::out, io::di, io::uo) is det.

make_imap(Proc, IMAP, !IO) :-
    store.new_mutvar(1, TagMutvar, !IO),
    store.new_mutvar(no, CapsMutvar, !IO),
    store.new_mutvar(no, SelMutvar, !IO),
    IMAP = imap(Proc, TagMutvar, CapsMutvar, SelMutvar).

:- pred wait_for_greeting(imap::in, maybe_error(greeting)::out, io::di, io::uo)
    is det.

wait_for_greeting(IMAP, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
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

:- pred get_new_tag(imap::in, tag::out, io::di, io::uo) is det.

get_new_tag(IMAP, tag(Tag), !IO) :-
    IMAP = imap(_Proc, TagMutvar, _CapsMutvar, _SelMutvar),
    get_mutvar(TagMutvar, N, !IO),
    set_mutvar(TagMutvar, N + 1, !IO),
    Tag = string.from_int(N).

%-----------------------------------------------------------------------------%

login(IMAP, username(UserName), password(Password), Res, Alerts, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    % XXX check capabilities first
    get_new_tag(IMAP, Tag, !IO),
    Login = login(make_astring(UserName), make_astring(Password)),
    make_command_stream(Tag - command_nonauth(Login), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_response_done(IMAP, Tag, ServerData, Res1, !IO),
        handle_login_response(IMAP, ServerData, Res1, Res, Alerts, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error),
        Alerts = []
    ).

:- pred handle_login_response(imap::in, list(response_data)::in,
    imap_result_ll::in, imap_result::out, list(alert)::out, io::di, io::uo)
    is det.

handle_login_response(IMAP, ServerData, Res0, Res, Alerts, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, CapsMutvar, _SelMutvar),
    get_mutvar(CapsMutvar, MaybeCaps0, !IO),
    list.foldl2(apply_login_response_data, ServerData, MaybeCaps0, MaybeCaps1,
        [], Alerts1),
    (
        Res0 = ok(RespText),
        apply_login_resp_text(ok, RespText, MaybeCaps1, MaybeCaps,
            Alerts1, Alerts),
        Res = ok(RespText ^ human_text)
    ;
        Res0 = no(RespText),
        apply_login_resp_text(no, RespText, MaybeCaps1, MaybeCaps,
            Alerts1, Alerts),
        Res = no(RespText ^ human_text)
    ;
        Res0 = bad(RespText),
        apply_login_resp_text(bad, RespText, MaybeCaps1, MaybeCaps,
            Alerts1, Alerts),
        Res = bad(RespText ^ human_text)
    ;
        Res0 = fatal(_RespText),
        sorry($module, $pred, "fatal")
    ;
        Res0 = error(Error),
        Res = error(Error),
        MaybeCaps = MaybeCaps1,
        Alerts = []
    ),
    set_mutvar(CapsMutvar, MaybeCaps, !IO).

:- pred apply_login_response_data(response_data::in,
    maybe(capability_data)::in, maybe(capability_data)::out,
    list(alert)::in, list(alert)::out) is det.

apply_login_response_data(ResponseData, !MaybeCaps, !Alerts) :-
    (
        ResponseData = mailbox_data(_)
    ;
        ResponseData = cond_state(Cond, RespText),
        apply_login_resp_text(Cond, RespText, !MaybeCaps, !Alerts)
    ;
        ResponseData = bye(_RespText),
        sorry($module, $pred, "bye")
    ;
        ResponseData = capability_data(_),
        sorry($module, $pred, "capability_data")
    ).

:- pred apply_login_resp_text(cond::in, resp_text::in,
    maybe(capability_data)::in, maybe(capability_data)::out,
    list(alert)::in, list(alert)::out) is det.

apply_login_resp_text(Cond, RespText, !MaybeCaps, !Alerts) :-
    RespText = resp_text(MaybeResponseCode, Text),
    (
        MaybeResponseCode = yes(ResponseCode),
        apply_login_resp_text(Cond, ResponseCode, Text, !MaybeCaps, !Alerts)
    ;
        MaybeResponseCode = no
    ).

:- pred apply_login_resp_text(cond::in, resp_text_code::in, string::in,
    maybe(capability_data)::in, maybe(capability_data)::out,
    list(alert)::in, list(alert)::out) is det.

apply_login_resp_text(Cond, ResponseCode, Text, !MaybeCaps, !Alerts) :-
    (
        ResponseCode = alert,
        cons(alert(Text), !Alerts)
    ;
        ResponseCode = capability_data(Caps),
        (
            Cond = ok,
            !:MaybeCaps = yes(Caps)
        ;
            Cond = no
        ;
            Cond = bad
        )
    ;
        ( ResponseCode = badcharset(_)
        ; ResponseCode = other(_, _)
        ; ResponseCode = parse
        ; ResponseCode = permanent_flags(_)
        ; ResponseCode = read_only
        ; ResponseCode = read_write
        ; ResponseCode = trycreate
        ; ResponseCode = uidnext(_)
        ; ResponseCode = uidvalidity(_)
        ; ResponseCode = unseen(_)
        )
    ).

%-----------------------------------------------------------------------------%

logout(IMAP, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    get_new_tag(IMAP, Tag, !IO),
    make_command_stream(Tag - command_any(logout), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        % XXX handle ServerData
        wait_for_response_done(IMAP, Tag, _ServerData, Res, !IO)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

mailbox(S) =
    ( string.to_upper(S, "INBOX") ->
        inbox
    ;
        astring(make_astring(S))
    ).

examine(IMAP, Mailbox, Res, Alerts, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    get_new_tag(IMAP, Tag, !IO),
    make_command_stream(Tag - command_auth(examine(Mailbox)), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_response_done(IMAP, Tag, ServerData, Res1, !IO),
        handle_examine_response(IMAP, Mailbox, ServerData, Res1, Res, Alerts,
            !IO)
    ;
        Res0 = error(Error),
        Res = error(Error),
        Alerts = []
    ).

:- pred handle_examine_response(imap::in, command.mailbox::in,
    list(response_data)::in, imap_result_ll::in, imap_result::out,
    list(alert)::out, io::di, io::uo) is det.

handle_examine_response(IMAP, Mailbox, ServerData, Res0, Res, Alerts, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, _CapsMutvar, SelMutvar),
    get_mutvar(SelMutvar, MaybeSel0, !IO),
    (
        Res0 = ok(RespText),
        Sel1 = new_selected_mailbox(Mailbox),
        list.foldl2(apply_examine_response_data, ServerData,
            yes(Sel1), MaybeSel2, [], Alerts1),
        apply_examine_resp_text(ok, RespText, MaybeSel2, MaybeSel,
            Alerts1, Alerts),
        Res = ok(RespText ^ human_text)
    ;
        Res0 = no(RespText),
        apply_examine_resp_text(no, RespText, MaybeSel0, MaybeSel, [], Alerts),
        Res = no(RespText ^ human_text)
    ;
        Res0 = bad(RespText),
        apply_examine_resp_text(bad, RespText, MaybeSel0, MaybeSel,
            [], Alerts),
        Res = no(RespText ^ human_text)
    ;
        Res0 = fatal(_RespText),
        sorry($module, $pred, "fatal")
    ;
        Res0 = error(Error),
        Res = error(Error),
        MaybeSel = MaybeSel0,
        Alerts = []
    ),
    set_mutvar(SelMutvar, MaybeSel, !IO).

:- func new_selected_mailbox(command.mailbox) = selected_mailbox.

new_selected_mailbox(Mailbox) =
    selected_mailbox(Mailbox, read_only, [], zero, zero, no, no, no, no).

:- pred apply_examine_response_data(response_data::in,
    maybe(selected_mailbox)::in, maybe(selected_mailbox)::out,
    list(alert)::in, list(alert)::out) is det.

apply_examine_response_data(ResponseData, MaybeSel0, MaybeSel, !Alerts) :-
    (
        ResponseData = mailbox_data(MailboxData),
        (
            MaybeSel0 = yes(Sel0),
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
            MaybeSel = yes(Sel)
        ;
            MaybeSel0 = no,
            MaybeSel = no
        )
    ;
        ResponseData = cond_state(Cond, RespText),
        apply_examine_resp_text(Cond, RespText, MaybeSel0, MaybeSel, !Alerts)
    ;
        ResponseData = bye(_RespText),
        sorry($module, $pred, "bye")
    ;
        ResponseData = capability_data(_),
        sorry($module, $pred, "capability_data")
    ).

:- pred apply_examine_resp_text(cond::in, resp_text::in,
    maybe(selected_mailbox)::in, maybe(selected_mailbox)::out,
    list(alert)::in, list(alert)::out) is det.

apply_examine_resp_text(Cond, RespText, !MaybeSel, !Alerts) :-
    RespText = resp_text(MaybeResponseCode, Text),
    (
        MaybeResponseCode = yes(ResponseCode),
        apply_examine_resp_text(Cond, ResponseCode, Text, !MaybeSel, !Alerts)
    ;
        MaybeResponseCode = no
    ).

:- pred apply_examine_resp_text(cond::in, resp_text_code::in, string::in,
    maybe(selected_mailbox)::in, maybe(selected_mailbox)::out,
    list(alert)::in, list(alert)::out) is det.

apply_examine_resp_text(Cond, ResponseCode, Text, !MaybeSel, !Alerts) :-
    (
        ResponseCode = alert,
        cons(alert(Text), !Alerts)
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
            !.MaybeSel = yes(Sel0),
            apply_selected_mailbox_response_code(ResponseCode, Sel0, Sel),
            !:MaybeSel = yes(Sel)
        ;
            Cond = ok,
            !.MaybeSel = no
        ;
            Cond = no
        ;
            Cond = bad
        )
    ;
        ( ResponseCode = badcharset(_)
        ; ResponseCode = capability_data(_)
        ; ResponseCode = parse
        ; ResponseCode = trycreate
        ),
        sorry($module, $pred, "ResponseCode=" ++ string(ResponseCode))
    ).

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

:- pred wait_for_response_done(imap::in, tag::in, list(response_data)::out,
    imap_result_ll::out, io::di, io::uo) is det.

wait_for_response_done(IMAP, Tag, ServerData, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
        parse_response_single(Bytes, ParseResult),
        (
            ParseResult = ok(ResponseSingle),
            (
                ResponseSingle = continue_req(_),
                Res = error("unexpected continue request"),
                ServerData = []
            ;
                ResponseSingle = response_data(ResponseData),
                wait_for_response_done(IMAP, Tag, ServerDataTail, Res, !IO),
                ServerData = [ResponseData | ServerDataTail] % lcmc
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
                    ),
                    ServerData = []
                ;
                    % XXX handle mismatched tagged response
                    wait_for_response_done(IMAP, Tag, ServerData, Res, !IO)
                )
            )
        ;
            ParseResult = error(Error),
            Res = error(Error),
            ServerData = []
        )
    ;
        ResRead = eof,
        Res = error("unexpected eof"),
        ServerData = []
    ;
        ResRead = error(Error),
        Res = error(io.error_message(Error)),
        ServerData = []
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
% vim: ft=mercury ts=4 sts=4 sw=4 et
