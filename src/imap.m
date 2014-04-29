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

open(HostPort, Res, Alerts, !IO) :-
    subprocess.spawn("/usr/bin/openssl",
        ["s_client", "-quiet", "-connect", HostPort], ResSpawn, !IO),
    (
        ResSpawn = ok(Proc),
        wait_for_greeting(Proc, ResGreeting, !IO),
        (
            ResGreeting = ok(Greeting),
            (
                Greeting = ok(RespText),
                handle_greeting_resp_text(RespText, MaybeCaps, Alerts),
                make_imap(Proc, MaybeCaps, IMAP, !IO),
                Res = ok(IMAP)
            ;
                Greeting = preauth(RespText),
                handle_greeting_resp_text(RespText, MaybeCaps, Alerts),
                make_imap(Proc, MaybeCaps, IMAP, !IO),
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

:- pred wait_for_greeting(subprocess::in, maybe_error(greeting)::out,
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

:- pred make_imap(subprocess::in, maybe(capability_data)::in, imap::out,
    io::di, io::uo) is det.

make_imap(Proc, MaybeCaps, IMAP, !IO) :-
    store.new_mutvar(1, TagMutvar, !IO),
    store.new_mutvar(MaybeCaps, CapsMutvar, !IO),
    store.new_mutvar(no, SelMutvar, !IO),
    IMAP = imap(Proc, TagMutvar, CapsMutvar, SelMutvar).

%-----------------------------------------------------------------------------%

:- pred get_new_tag(imap::in, tag::out, io::di, io::uo) is det.

get_new_tag(IMAP, tag(Tag), !IO) :-
    IMAP = imap(_Proc, TagMutvar, _CapsMutvar, _SelMutvar),
    get_mutvar(TagMutvar, N, !IO),
    set_mutvar(TagMutvar, N + 1, !IO),
    Tag = string.from_int(N).

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

%-----------------------------------------------------------------------------%

login(IMAP, username(UserName), password(Password), Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    % XXX check capabilities first
    get_new_tag(IMAP, Tag, !IO),
    Login = login(make_astring(UserName), make_astring(Password)),
    make_command_stream(Tag - command_nonauth(Login), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(IMAP, Tag, MaybeResponse, !IO),
        handle_login_response(IMAP, MaybeResponse, Res, !IO)
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

:- pred handle_login_response(imap::in, maybe_error(complete_response)::in,
    imap_result::out, io::di, io::uo) is det.

handle_login_response(_IMAP, error(Error), Res, !IO) :-
    Res = result(error, Error, []).
handle_login_response(IMAP, ok(Response), Res, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, CapsMutvar, _SelMutvar),
    get_mutvar(CapsMutvar, MaybeCaps0, !IO),

    Response = complete_response(UntaggedResponses, FinalMaybeTag,
        FinalRespText),
    list.foldl2(apply_login_untagged_responses, UntaggedResponses,
        MaybeCaps0, MaybeCaps1, [], Alerts1),
    apply_login_cond_or_bye(cond_bye_1(FinalMaybeTag), FinalRespText,
        MaybeCaps1, MaybeCaps, Alerts1, Alerts),
    make_result(FinalMaybeTag, FinalRespText, Alerts, Res),

    set_mutvar(CapsMutvar, MaybeCaps, !IO).

:- pred apply_login_untagged_responses(untagged_response_data::in,
    maybe(capability_data)::in, maybe(capability_data)::out,
    list(alert)::in, list(alert)::out) is det.

apply_login_untagged_responses(ResponseData, !MaybeCaps, !Alerts) :-
    (
        ResponseData = mailbox_data(_)
    ;
        ResponseData = cond_or_bye(Cond, RespText),
        apply_login_cond_or_bye(Cond, RespText, !MaybeCaps, !Alerts)
    ;
        ResponseData = capability_data(_),
        sorry($module, $pred, "capability_data")
    ).

:- pred apply_login_cond_or_bye(cond_bye::in, resp_text::in,
    maybe(capability_data)::in, maybe(capability_data)::out,
    list(alert)::in, list(alert)::out) is det.

apply_login_cond_or_bye(Cond, RespText, !MaybeCaps, !Alerts) :-
    (
        RespText = resp_text(yes(ResponseCode), Text),
        apply_login_cond_or_bye_2(Cond, ResponseCode, Text,
            !MaybeCaps, !Alerts)
    ;
        RespText = resp_text(no, _Text)
    ).

:- pred apply_login_cond_or_bye_2(cond_bye::in, resp_text_code::in, string::in,
    maybe(capability_data)::in, maybe(capability_data)::out,
    list(alert)::in, list(alert)::out) is det.

apply_login_cond_or_bye_2(Cond, ResponseCode, Text, !MaybeCaps, !Alerts) :-
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
        ;
            Cond = bye
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
        % XXX handle UntaggedResponses and FinalRespText
        wait_for_complete_response(IMAP, Tag, MaybeResponse, !IO),
        (
            MaybeResponse = ok(complete_response(_UntaggedResponses,
                FinalMaybeTag, FinalRespText)),
            make_result(FinalMaybeTag, FinalRespText, [], Res)
        ;
            MaybeResponse = error(Error),
            Res = result(error, Error, [])
        )
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

%-----------------------------------------------------------------------------%

mailbox(S) =
    ( string.to_upper(S, "INBOX") ->
        inbox
    ;
        astring(make_astring(S))
    ).

examine(IMAP, Mailbox, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    get_new_tag(IMAP, Tag, !IO),
    make_command_stream(Tag - command_auth(examine(Mailbox)), CommandStream),
    write_command_stream(Pipe, CommandStream, Res0, !IO),
    (
        Res0 = ok,
        wait_for_complete_response(IMAP, Tag, MaybeResponse, !IO),
        handle_examine_response(IMAP, Mailbox, MaybeResponse, Res, !IO)
    ;
        Res0 = error(Error),
        Res = result(error, Error, [])
    ).

:- pred handle_examine_response(imap::in, command.mailbox::in,
    maybe_error(complete_response)::in, imap_result::out, io::di, io::uo)
    is det.

handle_examine_response(_IMAP, _Mailbox, error(Error), Res, !IO) :-
    Res = result(error, Error, []).
handle_examine_response(IMAP, Mailbox, ok(Response), Res, !IO) :-
    IMAP = imap(_Pipe, _TagMutvar, _CapsMutvar, SelMutvar),
    get_mutvar(SelMutvar, MaybeSel0, !IO),

    Response = complete_response(UntaggedResponses, FinalMaybeTag,
        FinalRespText),
    (
        ( FinalMaybeTag = tagged(_, ok)
        ; FinalMaybeTag = tagged(_, no)
        ),
        MaybeSel1 = yes(new_selected_mailbox(Mailbox))
    ;
        ( FinalMaybeTag = tagged(_, bad)
        ; FinalMaybeTag = bye
        ),
        MaybeSel1 = MaybeSel0
    ),
    list.foldl2(apply_examine_untagged_response, UntaggedResponses,
        MaybeSel1, MaybeSel2, [], Alerts1),
    apply_examine_cond_or_bye(cond_bye_1(FinalMaybeTag), FinalRespText,
        MaybeSel2, MaybeSel, Alerts1, Alerts),
    make_result(FinalMaybeTag, FinalRespText, Alerts, Res),

    set_mutvar(SelMutvar, MaybeSel, !IO).

:- func new_selected_mailbox(command.mailbox) = selected_mailbox.

new_selected_mailbox(Mailbox) =
    selected_mailbox(Mailbox, read_only, [], zero, zero, no, no, no, no).

:- pred apply_examine_untagged_response(untagged_response_data::in,
    maybe(selected_mailbox)::in, maybe(selected_mailbox)::out,
    list(alert)::in, list(alert)::out) is det.

apply_examine_untagged_response(ResponseData, !MaybeSel, !Alerts) :-
    (
        ResponseData = cond_or_bye(Cond, RespText),
        apply_examine_cond_or_bye(Cond, RespText, !MaybeSel, !Alerts)
    ;
        ResponseData = mailbox_data(MailboxData),
        !.MaybeSel = yes(Sel0),
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
        !:MaybeSel = yes(Sel)
    ;
        ResponseData = mailbox_data(_),
        !.MaybeSel = no
    ;
        ResponseData = capability_data(_),
        sorry($module, $pred, "capability_data")
    ).

:- pred apply_examine_cond_or_bye(cond_bye::in, resp_text::in,
    maybe(selected_mailbox)::in, maybe(selected_mailbox)::out,
    list(alert)::in, list(alert)::out) is det.

apply_examine_cond_or_bye(Cond, RespText, !MaybeSel, !Alerts) :-
    RespText = resp_text(MaybeResponseCode, Text),
    (
        MaybeResponseCode = yes(ResponseCode),
        apply_examine_cond_or_bye_2(Cond, ResponseCode, Text,
            !MaybeSel, !Alerts)
    ;
        MaybeResponseCode = no
    ).

:- pred apply_examine_cond_or_bye_2(cond_bye::in, resp_text_code::in,
    string::in, maybe(selected_mailbox)::in, maybe(selected_mailbox)::out,
    list(alert)::in, list(alert)::out) is det.

apply_examine_cond_or_bye_2(Cond, ResponseCode, Text, !MaybeSel, !Alerts) :-
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
        ;
            Cond = bye
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

:- pred wait_for_complete_response(imap::in, tag::in,
    maybe_error(complete_response)::out, io::di, io::uo) is det.

wait_for_complete_response(IMAP, Tag, Res, !IO) :-
    IMAP = imap(Pipe, _TagMutvar, _CapsMutvar, _SelMutvar),
    wait_for_complete_response_2(Pipe, Tag, [], Res, !IO).

:- pred wait_for_complete_response_2(subprocess::in, tag::in,
    list(untagged_response_data)::in, maybe_error(complete_response)::out,
    io::di, io::uo) is det.

wait_for_complete_response_2(Pipe, Tag, RevUntagged0, Res, !IO) :-
    read_crlf_line_chop(Pipe, ResRead, !IO),
    (
        ResRead = ok(Bytes),
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
        % XXX for logout we should read until the connection is closed
        Res = error("unexpected eof")
    ;
        ResRead = error(Error),
        % XXX for logout we should read until the connection is closed
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
% vim: ft=mercury ts=4 sts=4 sw=4 et
