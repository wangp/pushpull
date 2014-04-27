%-----------------------------------------------------------------------------%

:- module imap.response.
:- interface.

:- type greeting
    --->    ok(resp_text)
    ;       preauth(resp_text)
    ;       bye(resp_text).

:- type response_single
    --->    continue_req(continue_req)
    ;       response_data(response_data)
    ;       response_done(response_done).

:- type continue_req
    --->    continue_req_resp_text(resp_text).
    %;       continue_req_base64(base64).

:- type response_data
    --->    cond_state(cond, resp_text)
    ;       bye(resp_text)
    %;      mailbox_data(...)
    %;      message_data(...)
    ;       capability_data(capability_data).

:- type response_done
    --->    response_tagged(tag, cond, resp_text).
    %       response_fatal(resp_cond_bye).

:- type cond
    --->    ok
    ;       no
    ;       bad.

:- type resp_text
    --->    resp_text(maybe(resp_text_code), string).

:- type resp_text_code
    --->    alert
    ;       badcharset(list(astring))
    ;       capability_data(capability_data)
    ;       parse
    ;       permanentflags % (list(flag_perm))
    ;       read_only
    ;       read_write
    ;       trycreate
    ;       uidnext(uid)
    ;       uidvalidity(uidvalidity)
    ;       unseen(int)
    ;       other(atom, maybe(string)).

:- type capability_data == list(capability).

:- type capability == atom.

%-----------------------------------------------------------------------------%

:- pred greeting(src::in, greeting::out, ps::in, ps::out) is semidet.

:- pred response_single(src::in, response_single::out, ps::in, ps::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module require.
:- import_module string.

:- import_module imap.charclass.

%-----------------------------------------------------------------------------%

:- pred atom(src::in, atom::out, ps::in, ps::out) is semidet.

atom(Src, atom(Atom), !PS) :-
    one_or_more_chars('ATOM-CHAR', Src, Chars, !PS),
    string.from_char_list(Chars, Atom0),
    string.to_upper(Atom0, Atom).

:- pred tag(src::in, tag::out, ps::in, ps::out) is semidet.

tag(Src, tag(Tag), !PS) :-
    one_or_more_chars(tag_char, Src, Chars, !PS),
    string.from_char_list(Chars, Tag).

:- pred text(src::in, string::out, ps::in, ps::out) is semidet.

text(Src, Text, !PS) :-
    one_or_more_chars('TEXT-CHAR', Src, Chars, !PS),
    string.from_char_list(Chars, Text).

%-----------------------------------------------------------------------------%

greeting(Src, Greeting, !PS) :-
    next_char(Src, '*', !PS),
    sp(Src, !PS),
    ( chars_ci("OK", Src, !PS) ->
        sp(Src, !PS),
        resp_text(Src, RespText, !PS),
        Greeting = ok(RespText)
    ; chars_ci("PREAUTH", Src, !PS) ->
        sp(Src, !PS),
        resp_text(Src, RespText, !PS),
        Greeting = preauth(RespText)
    ;
        resp_cond_bye(Src, RespText, !PS),
        Greeting = bye(RespText)
    ).

%-----------------------------------------------------------------------------%

response_single(Src, Response, !PS) :-
    ( next_char(Src, '+', !PS) ->
        continue_req(Src, ContinueReq, !PS),
        Response = continue_req(ContinueReq)
    ; next_char(Src, '*', !PS) ->
        response_data(Src, ResponseData, !PS),
        Response = response_data(ResponseData)
    ;
        response_done(Src, ResponseDone, !PS),
        Response = response_done(ResponseDone)
    ).

:- pred continue_req(src::in, continue_req::out, ps::in, ps::out) is semidet.

continue_req(Src, ContinueReq, !PS) :-
    sp(Src, !PS),
    ( resp_text(Src, RespText, !PS) ->
        ContinueReq = continue_req_resp_text(RespText)
    ;
        % XXX base64
        fail
    ).

:- pred response_data(src::in, response_data::out, ps::in, ps::out) is semidet.

response_data(Src, ResponseData, !PS) :-
    sp(Src, !PS),
    ( resp_cond_state(Src, Cond, RespText, !PS) ->
        ResponseData = cond_state(Cond, RespText)
    ; resp_cond_bye(Src, RespText, !PS) ->
        ResponseData = bye(RespText)
    % mailbox-data
    % message-data
    ; capability_data(Src, Caps, !PS) ->
        ResponseData = capability_data(Caps)
    ;
        fail
    ).

:- pred response_done(src::in, response_done::out, ps::in, ps::out) is semidet.

response_done(Src, response_tagged(Tag, Cond, RespText), !PS) :-
    tag(Src, Tag, !PS),
    sp(Src, !PS),
    resp_cond_state(Src, Cond, RespText, !PS).

:- pred resp_cond_bye(src::in, resp_text::out, ps::in, ps::out) is semidet.

resp_cond_bye(Src, RespText, !PS) :-
    chars_ci("BYE", Src, !PS),
    sp(Src, !PS),
    resp_text(Src, RespText, !PS).

:- pred resp_cond_state(src::in, cond::out, resp_text::out, ps::in, ps::out)
    is semidet.

resp_cond_state(Src, Cond, RespText, !PS) :-
    ( chars_ci("OK", Src, !PS) ->
        Cond = ok
    ; chars_ci("NO", Src, !PS) ->
        Cond = no
    ; chars_ci("BAD", Src, !PS) ->
        Cond = bad
    ;
        fail
    ),
    sp(Src, !PS),
    resp_text(Src, RespText, !PS).

:- pred resp_text(src::in, resp_text::out, ps::in, ps::out) is semidet.

resp_text(Src, resp_text(MaybeCode, Text), !PS) :-
    ( next_char(Src, '[', !PS) ->
        resp_text_code(Src, Code, !PS),
        next_char(Src, ']', !PS),
        sp(Src, !PS),
        MaybeCode = yes(Code)
    ;
        MaybeCode = no
    ),
    text(Src, Text, !PS).

:- pred resp_text_code(src::in, resp_text_code::out, ps::in, ps::out)
    is semidet.

resp_text_code(Src, Code, !PS) :-
    atom(Src, Atom, !PS),
    ( standard_resp_text_code(Src, Atom, Code0, !PS) ->
        Code = Code0
    ;
        other_resp_text_code(Src, Atom, Code, !PS)
    ).

:- pred standard_resp_text_code(src::in, atom::in, resp_text_code::out,
    ps::in, ps::out) is semidet.

standard_resp_text_code(Src, atom(Atom), Code, !PS) :-
    (
        Atom = "ALERT",
        Code = alert
    ;
        Atom = "CAPABILITY",
        sp_capabilities(Src, Caps, !PS),
        Code = capability_data(Caps)
    ;
        ( Atom = "BADCHARSET"
        ; Atom = "PARSE"
        ; Atom = "PERMANENTFLAGS"
        ; Atom = "READ-ONLY"
        ; Atom = "READ-WRITE"
        ; Atom = "TRYCREATE"
        ; Atom = "UIDNEXT"
        ; Atom = "UIDVALIDITY"
        ; Atom = "UNSEEN"
        ),
        sorry($module, $pred, Atom)
    ).

:- pred other_resp_text_code(src::in, atom::in, resp_text_code::out,
    ps::in, ps::out) is semidet.

other_resp_text_code(Src, Atom, other(Atom, MaybeText), !PS) :-
    ( sp(Src, !PS) ->
        one_or_more_chars(text_char_not_rsqbkt, Src, Chars, !PS),
        string.from_char_list(Chars, Text),
        MaybeText = yes(Text)
    ;
        MaybeText = no
    ).

:- pred text_char_not_rsqbkt(char::in) is semidet.

text_char_not_rsqbkt(C) :-
    'TEXT-CHAR'(C),
    C \= ']'.

:- pred capability_data(src::in, capability_data::out, ps::in, ps::out)
    is semidet.

capability_data(Src, Caps, !PS) :-
    atom(Src, atom("CAPABILITY"), !PS),
    sp_capabilities(Src, Caps, !PS).

:- pred sp_capabilities(src::in, capability_data::out, ps::in, ps::out)
    is semidet.

sp_capabilities(Src, Caps, !PS) :-
    % At least one of the Caps must be "IMAP4rev1".
    one_or_more(sp_capability, Src, Caps, !PS).

:- pred sp_capability(src::in, capability::out, ps::in, ps::out) is semidet.

sp_capability(Src, Cap, !PS) :-
    sp(Src, !PS),
    capability(Src, Cap, !PS).

:- pred capability(src::in, capability::out, ps::in, ps::out) is semidet.

capability(Src, Cap, !PS) :-
    atom(Src, Cap, !PS).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
