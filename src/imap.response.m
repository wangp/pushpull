%-----------------------------------------------------------------------------%

:- module imap.response.
:- interface.

:- import_module char.
:- import_module integer.
:- import_module map.

:- type greeting
    --->    ok(resp_text)
    ;       preauth(resp_text)
    ;       bye(resp_text).

:- type response_single
    --->    continue_req(continue_req)
    ;       untagged(untagged_response_data)
    ;       tagged(tag, cond, resp_text).

:- type continue_req
    --->    continue_req_resp_text(resp_text).
    %;       continue_req_base64(base64).

:- type untagged_response_data
    --->    cond_or_bye(cond_bye, resp_text)
            % BYE is not final if part of LOGOUT.
    ;       mailbox_data(mailbox_data)
    %;      message_data(...)
    ;       capability_data(capability_data).

:- type complete_response
    --->    complete_response(
                untagged        :: list(untagged_response_data),
                final_tag_cond  :: tagged_response_or_bye,
                final_resp_text :: resp_text
            ).

:- type tagged_response_or_bye
    --->    tagged(tag, cond)
    ;       bye.

:- type cond
    --->    ok
    ;       no
    ;       bad.

:- type cond_bye
    --->    ok
    ;       no
    ;       bad
    ;       bye.

:- type resp_text
    --->    resp_text(
                maybe_code :: maybe(resp_text_code),
                human_text :: string
            ).

:- type resp_text_code % really response_code
    --->    alert
    ;       badcharset(list(astring))
    ;       capability_data(capability_data)
    ;       parse
    ;       permanent_flags(permanent_flags)
    ;       read_only
    ;       read_write
    ;       trycreate
    ;       uidnext(uid)
    ;       uidvalidity(uidvalidity)
    ;       unseen(message_seq_nr)
    ;       other(atom, maybe(string)).

:- inst mailbox_response_code
    --->    unseen(ground)
    ;       permanent_flags(ground)
    ;       read_only
    ;       read_write
    ;       uidnext(ground)
    ;       uidvalidity(ground)
    ;       other(ground, ground).

:- type capability_data == list(capability).

:- type capability == atom.

:- type permanent_flags
    --->    permanent_flags(list(flag), can_create_keyword_flags).

:- type can_create_keyword_flags
    --->    can_create_keyword_flags
    ;       cannot_create_keyword_flags.

:- type flag
    --->    system(system_flag)
    ;       keyword(atom).

:- type system_flag
    --->    answered
    ;       flagged
    ;       deleted
    ;       seen
    ;       draft
    % does not include \Recent
    ;       extension(atom).

:- type mailbox_data
    --->    flags(list(flag))
    ;       list(mailbox_list)
    ;       lsub(mailbox_list)
    ;       search(list(message_seq_nr))
    ;       status(command.mailbox, status_att_list)
    ;       exists(integer)
    ;       recent(integer).

:- type mailbox_list
    --->    mailbox_list(
                maybe(mailbox_sflag),
                list(mailbox_oflag),
                maybe(hierarchy_separator),
                command.mailbox
            ).

:- type mailbox_sflag
    --->    noselect
    ;       marked
    ;       unmarked.

:- type mailbox_oflag
    --->    noinferiors
    ;       extension(atom).

:- type hierarchy_separator
    --->    hierarchy_separator(char).

:- type status_att_list == map(status_att, integer).

:- type status_att
    --->    messages
    ;       recent
    ;       uidnext
    ;       uidvalidity
    ;       unseen.

%-----------------------------------------------------------------------------%

:- func cond_bye_1(tagged_response_or_bye) = cond_bye.

:- func cond_bye_2(cond) = cond_bye.

%-----------------------------------------------------------------------------%

:- pred greeting(src::in, greeting::out, ps::in, ps::out) is semidet.

:- pred response_single(src::in, response_single::out, ps::in, ps::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

:- import_module imap.charclass.

%-----------------------------------------------------------------------------%

cond_bye_1(tagged(_Tag, Cond)) = cond_bye_2(Cond).
cond_bye_1(bye) = bye.

cond_bye_2(ok) = ok.
cond_bye_2(no) = no.
cond_bye_2(bad) = bad.

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

:- pred number(src::in, integer::out, ps::in, ps::out) is semidet.

number(Src, Integer, !PS) :-
    one_or_more_chars(char.is_digit, Src, Chars, !PS),
    string.from_char_list(Chars, String),
    integer.from_string(String) = Integer,
    zero =< Integer, Integer < det_from_string("4294967296").

:- pred nz_number(src::in, integer::out, ps::in, ps::out) is semidet.

nz_number(Src, Integer, !PS) :-
    digit_nz(Src, Char, !PS),
    zero_or_more_chars(char.is_digit, Src, Chars, !PS),
    string.from_char_list([Char | Chars], String),
    integer.from_string(String) = Integer,
    Integer < det_from_string("4294967296").

:- pred digit_nz(src::in, char::out, ps::in, ps::out) is semidet.

digit_nz(Src, C, !PS) :-
    next_char(Src, C, !PS),
    ( C = '1' ; C = '2' ; C = '3' ; C = '4' ; C = '5'
    ; C = '6' ; C = '7' ; C = '8' ; C = '9'
    ).

%-----------------------------------------------------------------------------%

greeting(Src, Greeting, !PS) :-
    next_char(Src, '*', !PS),
    sp(Src, !PS),
    atom(Src, Atom, !PS),
    sp(Src, !PS),
    resp_text(Src, RespText, !PS),
    (
        Atom = atom("OK"),
        Greeting = ok(RespText)
    ;
        Atom = atom("PREAUTH"),
        Greeting = preauth(RespText)
    ;
        Atom = atom("BYE"),
        Greeting = bye(RespText)
    ).

%-----------------------------------------------------------------------------%

response_single(Src, Response, !PS) :-
    ( next_char(Src, '+', !PS) ->
        continue_req(Src, ContinueReq, !PS),
        Response = continue_req(ContinueReq)
    ; next_char(Src, '*', !PS) ->
        % This includes response-fatal.
        response_data(Src, ResponseData, !PS),
        Response = untagged(ResponseData)
    ;
        response_tagged(Src, Tag, Cond, RespText, !PS),
        Response = tagged(Tag, Cond, RespText)
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

:- pred response_data(src::in, untagged_response_data::out, ps::in, ps::out)
    is semidet.

response_data(Src, ResponseData, !PS) :-
    sp(Src, !PS),
    ( resp_cond_state_or_bye(Src, Cond, RespText, !PS) ->
        ResponseData = cond_or_bye(Cond, RespText)
    ; mailbox_data(Src, MailboxData, !PS) ->
        ResponseData = mailbox_data(MailboxData)
    % message-data
    ; capability_data(Src, Caps, !PS) ->
        ResponseData = capability_data(Caps)
    ;
        fail
    ).

:- pred response_tagged(src::in, tag::out, cond::out, resp_text::out,
    ps::in, ps::out) is semidet.

response_tagged(Src, Tag, Cond, RespText, !PS) :-
    tag(Src, Tag, !PS),
    sp(Src, !PS),
    resp_cond_state(Src, Cond, RespText, !PS).

:- pred resp_cond_state(src::in, cond::out, resp_text::out,
    ps::in, ps::out) is semidet.

resp_cond_state(Src, Cond, RespText, !PS) :-
    atom(Src, atom(Atom), !PS),
    ( Atom = "OK", Cond = ok
    ; Atom = "NO", Cond = no
    ; Atom = "BAD", Cond = bad
    ),
    sp(Src, !PS),
    resp_text(Src, RespText, !PS).

:- pred resp_cond_state_or_bye(src::in, cond_bye::out, resp_text::out,
    ps::in, ps::out) is semidet.

resp_cond_state_or_bye(Src, Cond, RespText, !PS) :-
    atom(Src, atom(Atom), !PS),
    ( Atom = "OK", Cond = ok
    ; Atom = "NO", Cond = no
    ; Atom = "BAD", Cond = bad
    ; Atom = "BYE", Cond = bye
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
        Atom = "BADCHARSET",
        sorry($module, $pred, Atom)
    ;
        Atom = "CAPABILITY",
        sp_capabilities(Src, Caps, !PS),
        Code = capability_data(Caps)
    ;
        Atom = "PARSE",
        Code = parse
    ;
        Atom = "PERMANENTFLAGS",
        permanent_flags(Src, PermanentFlags, !PS),
        Code = permanent_flags(PermanentFlags)
    ;
        Atom = "READ-ONLY",
        Code = read_only
    ;
        Atom = "READ-WRITE",
        Code = read_write
    ;
        Atom = "TRYCREATE",
        Code = trycreate
    ;
        Atom = "UNSEEN",
        sp(Src, !PS),
        nz_number(Src, Number, !PS),
        Code = unseen(message_seq_nr(Number))
    ;
        Atom = "UIDNEXT",
        sp(Src, !PS),
        nz_number(Src, Number, !PS),
        Code = uidnext(uid(Number))
    ;
        Atom = "UIDVALIDITY",
        sp(Src, !PS),
        nz_number(Src, Number, !PS),
        Code = uidvalidity(uidvalidity(Number))
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

:- pred permanent_flags(src::in, permanent_flags::out,
    ps::in, ps::out) is semidet.

permanent_flags(Src, permanent_flags(Flags, CanCreateKeywordFlags), !PS) :-
    sp(Src, !PS),
    next_char(Src, '(', !PS),
    (
        permanent_flags_inner(Src, [], RevFlags,
            cannot_create_keyword_flags, CanCreateKeywordFlags0, !PS)
    ->
        list.reverse(RevFlags, Flags),
        CanCreateKeywordFlags = CanCreateKeywordFlags0
    ;
        Flags = [],
        CanCreateKeywordFlags = cannot_create_keyword_flags
    ),
    next_char(Src, ')', !PS).

:- pred permanent_flags_inner(src::in, list(flag)::in, list(flag)::out,
    can_create_keyword_flags::in, can_create_keyword_flags::out,
    ps::in, ps::out) is semidet.

permanent_flags_inner(Src, !AccFlags, !CanCreateKeywordFlags, !PS) :-
    (
        next_char(Src, '\\', !PS),
        next_char(Src, '*', !PS)
    ->
        !:CanCreateKeywordFlags = can_create_keyword_flags
    ;
        flag(Src, Flag, !PS),
        cons(Flag, !AccFlags)
    ),
    ( sp(Src, !PS) ->
        permanent_flags_inner(Src, !AccFlags, !CanCreateKeywordFlags, !PS)
    ;
        true
    ).

:- pred flag(src::in, flag::out, ps::in, ps::out) is semidet.

flag(Src, Flag, !PS) :-
    ( next_char(Src, '\\', !PS) ->
        atom(Src, Atom, !PS),
        system_flag(Atom, SystemFlag),
        Flag = system(SystemFlag)
    ;
        atom(Src, Atom, !PS),
        Flag = keyword(Atom)
    ).

:- pred system_flag(atom::in, system_flag::out) is semidet.

system_flag(Atom, Flag) :-
    ( Atom = atom("ANSWERED") ->
        Flag = answered
    ; Atom = atom("FLAGGED") ->
        Flag = flagged
    ; Atom = atom("DELETED") ->
        Flag = deleted
    ; Atom = atom("SEEN") ->
        Flag = seen
    ; Atom = atom("DRAFT") ->
        Flag = draft
    ;
        not Atom = atom("RECENT"),
        Flag = extension(Atom)
    ).

:- pred mailbox_data(src::in, mailbox_data::out, ps::in, ps::out) is semidet.

mailbox_data(Src, MailboxData, !PS) :-
    % atom is a superset of number so check number first.
    ( number(Src, Number, !PS) ->
        sp(Src, !PS),
        atom(Src, Atom, !PS),
        (
            Atom = atom("EXISTS"),
            MailboxData = exists(Number)
        ;
            Atom = atom("RECENT"),
            MailboxData = recent(Number)
        )
    ;
        atom(Src, Atom, !PS),
        (
            Atom = atom("FLAGS"),
            sp(Src, !PS),
            flag_list(Src, Flags, !PS),
            MailboxData = flags(Flags)
        ;
            Atom = atom("LIST"),
            sorry($module, $pred, "LIST")
        ;
            Atom = atom("LSUB"),
            sorry($module, $pred, "LSUB")
        ;
            Atom = atom("SEARCH"),
            sorry($module, $pred, "SEARCH")
        ;
            Atom = atom("STATUS"),
            sorry($module, $pred, "STATUS")
        )
    ).

:- pred flag_list(src::in, list(flag)::out, ps::in, ps::out) is semidet.

flag_list(Src, Flags, !PS) :-
    next_char(Src, '(', !PS),
    % XXX should have a helper for space-separated lists
    ( flag(Src, Flag, !PS) ->
        zero_or_more(sp_flag, Src, FlagsTail, !PS),
        Flags = [Flag | FlagsTail]
    ;
        Flags = []
    ),
    next_char(Src, ')', !PS).

:- pred sp_flag(src::in, flag::out, ps::in, ps::out) is semidet.

sp_flag(Src, Flag, !PS) :-
    sp(Src, !PS),
    flag(Src, Flag, !PS).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
