%-----------------------------------------------------------------------------%

:- module imap.response.
:- interface.

:- import_module char.
:- import_module integer.
:- import_module io.
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
    ;       message_data(message_data)
    ;       capability_data(capability_data).

:- type complete_response
    --->    complete_response(
                untagged        :: list(untagged_response_data),
                final_tag_cond  :: tagged_response_or_bye,
                final_resp_text :: resp_text
            ).

:- type tagged_response_or_bye
    --->    tagged(tag, cond)
    ;       bye
    ;       continue.

:- type cond
    --->    ok
    ;       no
    ;       bad.

:- type cond_bye
    --->    ok
    ;       no
    ;       bad
    ;       bye
    ;       continue.

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
    ;       highestmodseq(mod_seq_value)
    ;       nomodseq
    ;       appenduid(appenduid)
    ;       other(atom, maybe(string)).

:- inst mailbox_response_code
    --->    unseen(ground)
    ;       permanent_flags(ground)
    ;       read_only
    ;       read_write
    ;       uidnext(ground)
    ;       uidvalidity(ground)
    ;       highestmodseq(ground)
    ;       nomodseq.

:- type capability_data == list(capability).

:- type capability == atom.

:- type permanent_flags
    --->    permanent_flags(list(flag), can_create_keyword_flags).

:- type can_create_keyword_flags
    --->    can_create_keyword_flags
    ;       cannot_create_keyword_flags.

:- type mailbox_data
    --->    flags(list(flag))
    ;       list(mailbox_list)
    ;       lsub(mailbox_list)
    ;       search(list(integer), maybe(mod_seq_value))
    ;       esearch_msgseqnrs(maybe(tag),
                list(search_return_data(message_seq_nr)))
    ;       esearch_uids(maybe(tag),
                list(search_return_data(uid)))
    ;       status(mailbox, status_att_list)
    ;       exists(integer)
    ;       recent(integer).

:- type mailbox_list
    --->    mailbox_list(
                maybe(mailbox_sflag),
                list(mailbox_oflag),
                maybe(hierarchy_separator),
                mailbox
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

:- type uids_or_message_seq_nrs
    --->    uids
    ;       message_seq_nrs.

:- type status_att_list == map(status_att, integer).

:- type status_att
    --->    messages
    ;       recent
    ;       uidnext
    ;       uidvalidity
    ;       unseen.

:- type message_data
    --->    expunge(integer)
    ;       fetch(message_seq_nr, msg_atts). % non-empty

%-----------------------------------------------------------------------------%

:- func cond_bye_1(tagged_response_or_bye) = cond_bye.

:- func cond_bye_2(cond) = cond_bye.

%-----------------------------------------------------------------------------%

:- pred greeting(src::in, greeting::out, ps::in, ps::out) is semidet.

:- pred parse_response_single(pipe::in, list(int)::in,
    maybe(response_single)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module integer.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module imap.charclass.

%-----------------------------------------------------------------------------%

cond_bye_1(tagged(_Tag, Cond)) = cond_bye_2(Cond).
cond_bye_1(bye) = bye.
cond_bye_1(continue) = continue.

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

:- pred number_int(src::in, int::out, ps::in, ps::out) is semidet.

number_int(Src, Int, !PS) :-
    one_or_more_chars(char.is_digit, Src, Chars, !PS),
    string.from_char_list(Chars, String),
    string.to_int(String, Int),
    Int >= 0.

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

:- pred mod_seq_value(src::in, mod_seq_value::out, ps::in, ps::out) is semidet.

mod_seq_value(Src, mod_seq_value(N), !PS) :-
    mod_seq_valzer(Src, N, !PS),
    not N = zero.

:- pred mod_seq_valzer(src::in, integer::out, ps::in, ps::out) is semidet.

mod_seq_valzer(Src, Integer, !PS) :-
    one_or_more_chars(char.is_digit, Src, Chars, !PS),
    string.from_char_list(Chars, String),
    integer.from_string(String) = Integer,
    zero =< Integer, Integer < det_from_string("18446744073709551615").

:- pred astring(src::in, astring::out, ps::in, ps::out, io::di, io::uo) is det.

astring(Src, AString, !PS, !IO) :-
    ( one_or_more_chars('ASTRING-CHAR', Src, Chars, !PS) ->
        string.from_char_list(Chars, String),
        AString = astring(String)
    ;
        imap_string(Src, String, !PS, !IO),
        AString = imap_string(String)
    ).

:- pred imap_string(src::in, imap_string::out, ps::in, ps::out, io::di, io::uo)
    is det.

imap_string(Src, String, !PS, !IO) :-
    (
        quoted_string(Src, QS, !PS)
    ->
        String = QS
    ;
        literal_octet_count(Src, NumOctets, !PS),
        eof(Src, !.PS)
    ->
        literal(Src, NumOctets, String, !:PS, !IO)
    ;
        throw(fail_exception)
    ).

:- pred quoted_string(src::in, imap_string::out, ps::in, ps::out) is semidet.

quoted_string(Src, quoted(QS), !PS) :-
    next_char(Src, '"', !PS),
    zero_or_more(quoted_char, Src, Chars, !PS),
    next_char(Src, '"', !PS),
    string.from_char_list(Chars, QS).

:- pred quoted_char(src::in, char::out, ps::in, ps::out) is semidet.

quoted_char(Src, C, !PS) :-
    next_char(Src, C0, !PS),
    ( C0 = ('"') ->
        fail
    ; C0 = ('\\') ->
        next_char(Src, C, !PS),
        ( C = ('"')
        ; C = ('\\')
        )
    ;
        'TEXT-CHAR'(C0),
        C = C0
    ).

:- pred literal_octet_count(src::in, int::out, ps::in, ps::out) is semidet.

literal_octet_count(Src, NumOctets, !PS) :-
    next_char(Src, '{', !PS),
    number_int(Src, NumOctets, !PS),
    next_char(Src, '}', !PS).

:- pred literal(src::in, int::in, imap_string::out, ps::out, io::di, io::uo)
    is det.

literal(src(Pipe), NumOctets, literal(Bytes), NewPS, !IO) :-
    read_bytes(Pipe, NumOctets, ResBytes, !IO),
    (
        ResBytes = ok(Bytes),
        read_crlf_line_chop(Pipe, ResLine, !IO),
        (
            ResLine = ok(NewPS)
        ;
            ResLine = eof,
            throw(fail_exception)
        ;
            ResLine = error(_),
            throw(fail_exception)
        )
    ;
        ResBytes = eof,
        throw(fail_exception)
    ;
        ResBytes = error(_),
        throw(fail_exception)
    ).

:- pred nstring(src::in, nstring::out, ps::in, ps::out, io::di, io::uo) is det.

nstring(Src, NString, !PS, !IO) :-
    ( atom(Src, Atom, !PS) ->
        ( Atom = atom("NIL") ->
            NString = no
        ;
            throw(fail_exception)
        )
    ;
        imap_string(Src, String, !PS, !IO),
        NString = yes(String)
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

parse_response_single(Pipe, Input, Res, !IO) :-
    promise_equivalent_solutions [Res, !:IO] (
    try [io(!IO)]
        (
            Src = src(Pipe),
            PS0 = Input,
            response_single(Src, Response, PS0, PS, !IO),
            ( eof(Src, PS) ->
                ResPrime = yes(Response)
            ;
                ResPrime = no
            )
        )
    then
        Res = ResPrime
    catch fail_exception ->
        Res = no
    ).

:- pred response_single(src::in, response_single::out, ps::in, ps::out,
    io::di, io::uo) is det.

response_single(Src, Response, !PS, !IO) :-
    ( next_char(Src, '+', !PS) ->
        ( continue_req(Src, ContinueReq, !PS) ->
            Response = continue_req(ContinueReq)
        ;
            throw(fail_exception)
        )
    ; next_char(Src, '*', !PS) ->
        % This includes response-fatal.
        sp_response_data(Src, ResponseData, !PS, !IO),
        Response = untagged(ResponseData)
    ; response_tagged(Src, Tag, Cond, RespText, !PS) ->
        Response = tagged(Tag, Cond, RespText)
    ;
        throw(fail_exception)
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

:- pred sp_response_data(src::in, untagged_response_data::out, ps::in, ps::out,
    io::di, io::uo) is det.

sp_response_data(Src, ResponseData, !PS, !IO) :-
    ( sp(Src, !PS) ->
        response_data(Src, ResponseData, !PS, !IO)
    ;
        throw(fail_exception)
    ).

:- pred response_data(src::in, untagged_response_data::out, ps::in, ps::out,
    io::di, io::uo) is det.

response_data(Src, ResponseData, !PS, !IO) :-
    ( resp_cond_state_or_bye(Src, Cond, RespText, !PS) ->
        ResponseData = cond_or_bye(Cond, RespText)
    ; capability_data(Src, Caps, !PS) ->
        ResponseData = capability_data(Caps)
    ;
        mailbox_or_message_data(Src, ResponseData, !PS, !IO)
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
        ( sp(Src, !PS) ->
            text(Src, Text, !PS)
        ;
            % GMail does not send SP and text.
            Text = ""
        ),
        MaybeCode = yes(Code)
    ;
        MaybeCode = no,
        text(Src, Text, !PS)
    ).

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
    ;
        % RFC 4551
        Atom = "HIGHESTMODSEQ",
        sp(Src, !PS),
        mod_seq_valzer(Src, Number, !PS),
        ( Number = zero ->
            Code = nomodseq
        ;
            Code = highestmodseq(mod_seq_value(Number))
        )
    ;
        % RFC 4551
        Atom = "NOMODSEQ",
        Code = nomodseq
    ;
        % RFC 4315
        Atom = "APPENDUID",
        sp(Src, !PS),
        nz_number(Src, Number, !PS),
        sp(Src, !PS),
        uid_set(Src, UIDSet, !PS),
        Code = appenduid(appenduid(uidvalidity(Number), UIDSet))
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
    one_or_more(sp_then(capability), Src, Caps, !PS).

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

:- pred mailbox_or_message_data(src::in, untagged_response_data::out,
    ps::in, ps::out, io::di, io::uo) is det.

mailbox_or_message_data(Src, ResponseData, !PS, !IO) :-
    % atom is a superset of number so check number first.
    ( number(Src, Number, !PS) ->
        (
            sp(Src, !PS),
            atom(Src, Atom, !PS)
        ->
            % mailbox-data
            ( Atom = atom("EXISTS") ->
                ResponseData = mailbox_data(exists(Number))
            ; Atom = atom("RECENT") ->
                ResponseData = mailbox_data(recent(Number))
            % message-data
            ; Atom = atom("EXPUNGE"), Number > zero ->
                ResponseData = message_data(expunge(Number))
            ; Atom = atom("FETCH"), Number > zero ->
                message_data_fetch(Src, message_seq_nr(Number), MessageData,
                    !PS, !IO),
                ResponseData = message_data(MessageData)
            ;
                throw(fail_exception)
            )
        ;
                throw(fail_exception)
        )
    ; atom(Src, Atom, !PS) ->
        mailbox_data_atom(Atom, Src, MailboxData, !PS, !IO),
        ResponseData = mailbox_data(MailboxData)
    ;
        throw(fail_exception)
    ).

:- pred mailbox_data_atom(atom::in, src::in, mailbox_data::out,
    ps::in, ps::out, io::di, io::uo) is det.

mailbox_data_atom(Atom, Src, MailboxData, !PS, !IO) :-
    ( Atom = atom("FLAGS") ->
        ( sp_then(flag_list, Src, Flags, !PS) ->
            MailboxData = flags(Flags)
        ;
            throw(fail_exception)
        )
    ; Atom = atom("LIST") ->
        sorry($module, $pred, "LIST")
    ; Atom = atom("LSUB") ->
        sorry($module, $pred, "LSUB")
    ; Atom = atom("SEARCH") ->
        ( mailbox_data_search(Src, MailboxData0, !PS) ->
            MailboxData = MailboxData0
        ;
            throw(fail_exception)
        )
    ; Atom = atom("ESEARCH") ->
        % RFC 4466
        mailbox_data_esearch(Src, MailboxData, !PS, !IO)
    ; Atom = atom("STATUS") ->
        sorry($module, $pred, "STATUS")
    ;
        throw(fail_exception)
    ).

:- pred flag_list(src::in, list(flag)::out, ps::in, ps::out) is semidet.

flag_list(Src, Flags, !PS) :-
    next_char(Src, '(', !PS),
    % XXX should have a helper for space-separated lists
    ( flag(Src, Flag, !PS) ->
        zero_or_more(sp_then(flag), Src, FlagsTail, !PS),
        Flags = [Flag | FlagsTail]
    ;
        Flags = []
    ),
    next_char(Src, ')', !PS).

:- pred mailbox_data_search(src::in, mailbox_data::out, ps::in, ps::out)
    is semidet.

mailbox_data_search(Src, MailboxData, !PS) :-
    zero_or_more(sp_then(nz_number), Src, Numbers, !PS),
    % RFC 4551
    (
        sp(Src, !PS),
        next_char(Src, '(', !PS),
        atom(Src, atom("MODSEQ"), !PS)
    ->
        sp(Src, !PS),
        mod_seq_value(Src, ModSeqValue, !PS),
        next_char(Src, ')', !PS),
        MaybeModSeqValue = yes(ModSeqValue)
    ;
        MaybeModSeqValue = no
    ),
    MailboxData = search(Numbers, MaybeModSeqValue).

:- pred mailbox_data_esearch(src::in, mailbox_data::out,
    ps::in, ps::out, io::di, io::uo) is det.

mailbox_data_esearch(Src, MailboxData, !PS, !IO) :-
    ( search_correlator(Src, SearchTag, !PS) ->
        MaybeTag = yes(SearchTag)
    ;
        MaybeTag = no
    ),
    ( sp_then(atom, Src, atom("UID"), !PS) ->
        sp_then_search_return_datas(Src, ReturnDatas, !PS, !IO),
        MailboxData = esearch_uids(MaybeTag, ReturnDatas)
    ;
        sp_then_search_return_datas(Src, ReturnDatas, !PS, !IO),
        MailboxData = esearch_msgseqnrs(MaybeTag, ReturnDatas)
    ).

:- pred message_data_fetch(src::in, message_seq_nr::in, message_data::out,
    ps::in, ps::out, io::di, io::uo) is det.

message_data_fetch(Src, MsgSeqNr, MessageData, !PS, !IO) :-
    det_sp(Src, !PS),
    det_next_char(Src, '(', !PS),
    msg_att(Src, Att, !PS, !IO),
    sp_then_msg_atts(Src, Atts, !PS, !IO),
    det_next_char(Src, ')', !PS),
    MessageData = fetch(MsgSeqNr, [Att | Atts]).

:- pred sp_then_msg_atts(src::in, list(msg_att)::out,
    ps::in, ps::out, io::di, io::uo) is det.

sp_then_msg_atts(Src, Atts, !PS, !IO) :-
    ( sp(Src, !PS) ->
        msg_att(Src, Att, !PS, !IO),
        sp_then_msg_atts(Src, AttsTail, !PS, !IO),
        Atts = [Att | AttsTail] % lcmc
    ;
        Atts = []
    ).

:- pred msg_att(src::in, msg_att::out, ps::in, ps::out, io::di, io::uo) is det.

msg_att(Src, Att, !PS, !IO) :-
    ( msg_att_atom(Src, Atom, !PS) ->
        (
            Atom = atom("BODY"),
            next_char(Src, '[', !.PS, _)
        ->
            msg_att_body_section(Src, Att, !PS, !IO)
        ;
            sp(Src, !PS)
        ->
            ( msg_att_semi(Src, Atom, AttPrime, !PS) ->
                Att = AttPrime
            ;
                msg_att_io(Src, Atom, Att, !PS, !IO)
            )
        ;
            throw(fail_exception)
        )
    ;
        throw(fail_exception)
    ).

:- pred msg_att_atom(src::in, atom::out, ps::in, ps::out) is semidet.

msg_att_atom(Src, atom(Atom), !PS) :-
    one_or_more_chars(msg_att_atom_char, Src, Chars, !PS),
    string.from_char_list(Chars, Atom0),
    string.to_upper(Atom0, Atom).

:- pred msg_att_atom_char(char::in) is semidet.

msg_att_atom_char(C) :-
    'ATOM-CHAR'(C),
    C \= ('['). % stop for BODY[ ]

:- pred msg_att_semi(src::in, atom::in, msg_att::out, ps::in, ps::out)
    is semidet.

msg_att_semi(Src, Atom, Att, !PS) :-
    (
        Atom = atom("FLAGS"),
        flag_fetches(Src, Flags, !PS),
        Att = flags(Flags)
    ;
        Atom = atom("INTERNALDATE"),
        date_time(Src, InternalDate, !PS),
        Att = internaldate(InternalDate)
    ;
        Atom = atom("RFC822.SIZE"),
        number(Src, Number, !PS),
        Att = rfc822_size(Number)
    ;
        Atom = atom("UID"),
        uniqueid(Src, UID, !PS),
        Att = uid(UID)
    ;
        % RFC 4551
        Atom = atom("MODSEQ"),
        next_char(Src, '(', !PS),
        mod_seq_value(Src, ModSeqValue, !PS),
        next_char(Src, ')', !PS),
        Att = modseq(ModSeqValue)
    ).

:- pred msg_att_io(src::in, atom::in, msg_att::out, ps::in, ps::out,
    io::di, io::uo) is det.

msg_att_io(Src, Atom, Att, !PS, !IO) :-
    ( Atom = atom("ENVELOPE") ->
        envelope(Src, Env, !PS, !IO),
        Att = envelope(Env)
    ; Atom = atom("RFC822") ->
        nstring(Src, NString, !PS, !IO),
        Att = rfc822(NString)
    ; Atom = atom("RFC822.HEADER") ->
        nstring(Src, NString, !PS, !IO),
        Att = rfc822_header(NString)
    ; Atom = atom("RFC822.TEXT") ->
        nstring(Src, NString, !PS, !IO),
        Att = rfc822_text(NString)
    ; Atom = atom("BODY") ->
        sorry($module, $pred, "BODY")
    ; Atom = atom("BODYSTRUCTURE") ->
        sorry($module, $pred, "BODYSTRUCTURE")
    ;
        throw(fail_exception)
    ).

:- pred msg_att_body_section(src::in, msg_att::out, ps::in, ps::out,
    io::di, io::uo) is det.

msg_att_body_section(Src, Att, !PS, !IO) :-
    section(Src, Section, !PS, !IO),
    (
        next_char(Src, '<', !PS),
        number(Src, OriginOctet, !PS),
        next_char(Src, '>', !PS)
    ->
        MaybeOriginOctet = yes(OriginOctet)
    ;
        MaybeOriginOctet = no
    ),
    det_sp(Src, !PS),
    nstring(Src, NString, !PS, !IO),
    Att = body(Section, MaybeOriginOctet, NString).

:- pred flag_fetches(src::in, list(flag_fetch)::out, ps::in, ps::out)
    is semidet.

flag_fetches(Src, Flags, !PS) :-
    next_char(Src, '(', !PS),
    % XXX should be abstracted
    ( flag_fetch(Src, Flag, !PS) ->
        zero_or_more(sp_then(flag_fetch), Src, RestFlags, !PS),
        Flags = [Flag | RestFlags]
    ;
        Flags = []
    ),
    next_char(Src, ')', !PS).

:- pred flag_fetch(src::in, flag_fetch::out, ps::in, ps::out) is semidet.

flag_fetch(Src, Flag, !PS) :-
    (
        next_char(Src, '\\', !PS),
        atom(Src, atom("RECENT"), !PS)
    ->
        Flag = recent
    ;
        flag(Src, Flag0, !PS),
        Flag = flag(Flag0)
    ).

:- pred envelope(src::in, envelope::out, ps::in, ps::out, io::di, io::uo)
    is det.

envelope(Src, Env, !PS, !IO) :-
    det_next_char(Src, '(', !PS),
    nstring(Src, Env ^ date, !PS, !IO),
    det_sp(Src, !PS),

    nstring(Src, Env ^ subject, !PS, !IO),
    det_sp(Src, !PS),

    address_list(Src, Env ^ from, !PS, !IO),
    det_sp(Src, !PS),

    address_list(Src, Env ^ sender, !PS, !IO),
    det_sp(Src, !PS),

    address_list(Src, Env ^ reply_to, !PS, !IO),
    det_sp(Src, !PS),

    address_list(Src, Env ^ to, !PS, !IO),
    det_sp(Src, !PS),

    address_list(Src, Env ^ cc, !PS, !IO),
    det_sp(Src, !PS),

    address_list(Src, Env ^ bcc, !PS, !IO),
    det_sp(Src, !PS),

    nstring(Src, Env ^ in_reply_to, !PS, !IO),
    det_sp(Src, !PS),

    maybe_message_id(Src, Env ^ message_id,  !PS, !IO),

    det_next_char(Src, ')', !PS).

:- pred address_list(src::in, list(address)::out, ps::in, ps::out,
    io::di, io::uo) is det.

address_list(Src, Addresses, !PS, !IO) :-
    ( next_char(Src, '(', !PS) ->
        one_or_more_addresses(Src, Addresses, !PS, !IO),
        det_next_char(Src, ')', !PS)
    ; atom(Src, atom("NIL"), !PS) ->
        Addresses = []
    ;
        throw(fail_exception)
    ).

:- pred one_or_more_addresses(src::in, list(address)::out,
    ps::in, ps::out, io::di, io::uo) is det.

one_or_more_addresses(Src, Addresses, !PS, !IO) :-
    zero_or_more_addresses(Src, Addresses, !PS, !IO),
    (
        Addresses = [],
        throw(fail_exception)
    ;
        Addresses = [_ | _]
    ).

:- pred zero_or_more_addresses(src::in, list(address)::out,
    ps::in, ps::out, io::di, io::uo) is det.

zero_or_more_addresses(Src, Addresses, !PS, !IO) :-
    maybe_address(Src, MaybeAddress, !PS, !IO),
    (
        MaybeAddress = yes(Address),
        zero_or_more_addresses(Src, AddressesTail, !PS, !IO),
        Addresses = [Address | AddressesTail] % lcmc
    ;
        MaybeAddress = no,
        Addresses = []
    ).

:- pred maybe_address(src::in, maybe(address)::out, ps::in, ps::out,
    io::di, io::uo) is det.

maybe_address(Src, MaybeAddress, !PS, !IO) :-
    ( next_char(Src, '(', !PS) ->
        nstring(Src, Address ^ name, !PS, !IO),
        det_sp(Src, !PS),
        nstring(Src, Address ^ adl, !PS, !IO),
        det_sp(Src, !PS),
        nstring(Src, Address ^ mailbox, !PS, !IO),
        det_sp(Src, !PS),
        nstring(Src, Address ^ host, !PS, !IO),
        det_next_char(Src, ')', !PS),
        MaybeAddress = yes(Address)
    ;
        MaybeAddress = no
    ).

:- pred maybe_message_id(src::in, maybe_message_id::out, ps::in, ps::out,
    io::di, io::uo) is det.

maybe_message_id(Src, MessageId, !PS, !IO) :-
    nstring(Src, NString, !PS, !IO),
    (
        NString = no,
        MessageId = nil
    ;
        NString = yes(quoted(S)),
        MessageId = message_id(S)
    ;
        NString = yes(literal(_)),
        sorry($module, $pred, "literal")
    ).

:- pred date_time(src::in, date_time::out, ps::in, ps::out) is semidet.

date_time(Src, DateTime, !PS) :-
    next_char(Src, '"', !PS),
    date_day_fixed(Src, Day, !PS),
    next_char(Src, '-', !PS),
    date_month(Src, Month, !PS),
    next_char(Src, '-', !PS),
    date_year(Src, Year, !PS),
    sp(Src, !PS),
    time(Src, Time, !PS),
    sp(Src, !PS),
    zone(Src, Zone, !PS),
    next_char(Src, '"', !PS),
    DateTime = date_time(Day, Month, Year, Time, Zone).

:- pred date_day_fixed(src::in, int::out, ps::in, ps::out) is semidet.

date_day_fixed(Src, I, !PS) :-
    ( sp(Src, !PS) ->
        digit(Src, I, !PS)
    ;
        two_digit(Src, I, !PS)
    ).

:- pred date_month(src::in, month::out, ps::in, ps::out) is semidet.

date_month(Src, Month, !PS) :-
    next_char(Src, A, !PS),
    next_char(Src, B, !PS),
    next_char(Src, C, !PS),
    Chars = [to_upper(A), to_upper(B), to_upper(C)],
    string.from_char_list(Chars, String),
    month(String, Month).

:- pred date_year(src::in, int::out, ps::in, ps::out) is semidet.

date_year(Src, Year, !PS) :-
    four_digit(Src, Year, !PS).

:- pred time(src::in, time::out, ps::in, ps::out) is semidet.

time(Src, time(H, M, S), !PS) :-
    two_digit(Src, H, !PS),
    next_char(Src, ':', !PS),
    two_digit(Src, M, !PS),
    next_char(Src, ':', !PS),
    two_digit(Src, S, !PS).

:- pred zone(src::in, zone::out, ps::in, ps::out) is semidet.

zone(Src, Zone, !PS) :-
    next_char(Src, PM, !PS),
    ( PM = ('+')
    ; PM = ('-')
    ),
    digit_char(Src, A, !PS),
    digit_char(Src, B, !PS),
    digit_char(Src, C, !PS),
    digit_char(Src, D, !PS),
    string.from_char_list([PM, A, B, C, D], S),
    Zone = zone(S).

:- pred section(src::in, section::out, ps::in, ps::out, io::di, io::uo) is det.

section(Src, Section, !PS, !IO) :-
    det_next_char(Src, '[', !PS),
    ( next_char(Src, ']', !PS) ->
        Section = entire_message
    ;
        section_spec(Src, Section, !PS, !IO),
        det_next_char(Src, ']', !PS)
    ).

:- pred section_spec(src::in, section::out, ps::in, ps::out, io::di, io::uo)
    is det.

section_spec(Src, Section, !PS, !IO) :-
    ( atom(Src, Atom, !PS) ->
        ( Atom = atom("HEADER") ->
            Section = msgtext(header)
        ; Atom = atom("HEADER.FIELDS") ->
            det_sp(Src, !PS),
            header_list(Src, Field, Fields, !PS, !IO),
            Section = msgtext(header_fields(Field, Fields))
        ; Atom = atom("HEADER.FIELDS.NOT") ->
            det_sp(Src, !PS),
            header_list(Src, Field, Fields, !PS, !IO),
            Section = msgtext(header_fields_not(Field, Fields))
        ; Atom = atom("TEXT") ->
            Section = msgtext(text)
        ;
            throw(fail_exception)
        )
    ;
        throw(fail_exception)
    ).

:- pred header_list(src::in, header_field_name::out,
    list(header_field_name)::out, ps::in, ps::out, io::di, io::uo) is det.

header_list(Src, Field, Fields, !PS, !IO) :-
    det_next_char(Src, '(', !PS),
    header_field_name(Src, Field, !PS, !IO),
    sp_then_header_field_name(Src, Fields, !PS, !IO),
    det_next_char(Src, ')', !PS).

:- pred header_field_name(src::in, header_field_name::out, ps::in, ps::out,
    io::di, io::uo) is det.

header_field_name(Src, Field, !PS, !IO) :-
    astring(Src, Field, !PS, !IO).

:- pred sp_then_header_field_name(src::in, list(header_field_name)::out,
    ps::in, ps::out, io::di, io::uo) is det.

sp_then_header_field_name(Src, Fields, !PS, !IO) :-
    ( sp(Src, !PS) ->
        header_field_name(Src, Field, !PS, !IO),
        sp_then_header_field_name(Src, FieldsTail, !PS, !IO),
        Fields = [Field | FieldsTail] % lcmc
    ;
        Fields = []
    ).

:- pred uniqueid(src::in, uid::out, ps::in, ps::out) is semidet.

uniqueid(Src, uid(Number), !PS) :-
    nz_number(Src, Number, !PS).

:- pred uid_set(src::in, uid_set::out, ps::in, ps::out) is semidet.

uid_set(Src, Set, !PS) :-
    uid_set_2(Src, set.init, Set, !PS).

:- pred uid_set_2(src::in, uid_set::in, uid_set::out, ps::in, ps::out)
    is semidet.

uid_set_2(Src, !Set, !PS) :-
    uniqueid(Src, First, !PS),
    ( next_char(Src, ':', !PS) ->
        uniqueid(Src, Second, !PS),
        ( First =< Second ->
            Low = First,
            High = Second
        ;
            Low = Second,
            High = First
        ),
        Range = uid_range(Low, High)
    ;
        Range = uid_range(First, First)
    ),
    set.insert(Range, !Set),
    ( next_char(Src, ',', !PS) ->
        uid_set_2(Src, !Set, !PS)
    ;
        true
    ).

:- pred uid =< uid.
:- mode in =< in is semidet.

uid(X) =< uid(Y) :-
    X =< Y.

:- pred sequence_set(src::in, sequence_set(T)::out, ps::in, ps::out)
    is semidet <= sequence_set_number(T).

sequence_set(Src, SequenceSet, !PS) :-
    sequence_set_element(Src, Elem, !PS),
    ( next_char(Src, ',', !PS) ->
        sequence_set(Src, Elems, !PS),
        SequenceSet = cons(Elem, Elems)
    ;
        SequenceSet = last(Elem)
    ).

:- pred sequence_set_element(src::in, sequence_set_element(T)::out,
    ps::in, ps::out) is semidet <= sequence_set_number(T).

sequence_set_element(Src, Elem, !PS) :-
    seq_number(Src, First, !PS),
    ( next_char(Src, ':', !PS) ->
        seq_number(Src, Second, !PS),
        ( seq_number_le(First, Second) ->
            Low = First,
            High = Second
        ;
            Low = Second,
            High = First
        ),
        Elem = range(Low, High)
    ;
        Elem = element(First)
    ).

:- pred seq_number(src::in, seq_number(T)::out, ps::in, ps::out) is semidet
    <= sequence_set_number(T).

seq_number(Src, X, !PS) :-
    ( nz_number(Src, Integer, !PS) ->
        X = number(from_nz_number(Integer))
    ;
        next_char(Src, '*', !PS),
        X = star
    ).

:- pred seq_number_le(seq_number(T)::in, seq_number(T)::in) is semidet
    <= sequence_set_number(T).

seq_number_le(X, Y) :-
    (
        X = star,
        Y = star
    ;
        X = number(_),
        Y = star
    ;
        X = number(NX),
        Y = number(NY),
        to_nz_number(NX) =< to_nz_number(NY)
    ).

:- pred search_correlator(src::in, tag::out, ps::in, ps::out) is semidet.

search_correlator(Src, tag(Tag), !PS) :-
    sp(Src, !PS),
    next_char(Src, '(', !PS),
    atom(Src, atom("TAG"), !PS),
    sp(Src, !PS),
    % XXX strictly should be imap_string but that requires I/O
    % and who would do that anyway? Leaving it for now.
    quoted_string(Src, quoted(Tag), !PS),
    next_char(Src, ')', !PS).

:- pred sp_then_search_return_datas(src::in, list(search_return_data(T))::out,
    ps::in, ps::out, io::di, io::uo) is det <= sequence_set_number(T).

sp_then_search_return_datas(Src, ReturnDatas, !PS, !IO) :-
    ( sp(Src, !PS) ->
        search_return_data(Src, ReturnData, !PS, !IO),
        sp_then_search_return_datas(Src, ReturnDatasTail, !PS, !IO),
        ReturnDatas = [ReturnData | ReturnDatasTail] % lcmc
    ;
        ReturnDatas = []
    ).

:- pred search_return_data(src::in, search_return_data(T)::out,
    ps::in, ps::out, io::di, io::uo) is det <= sequence_set_number(T).

search_return_data(Src, ReturnData, !PS, !IO) :-
    detify(tagged_ext_label, Src, Name, !PS),
    det_sp(Src, !PS),
    Name = tagged_ext_label(Atom),
    ( Atom = atom("MIN") ->
        detify(nz_number, Src, Number, !PS),
        ReturnData = min(Number)
    ; Atom = atom("MAX") ->
        detify(nz_number, Src, Number, !PS),
        ReturnData = max(Number)
    ; Atom = atom("ALL") ->
        detify(sequence_set, Src, SequenceSet, !PS),
        ReturnData = all(SequenceSet)
    ; Atom = atom("COUNT") ->
        detify(number, Src, Number, !PS),
        ReturnData = count(Number)
    ; Atom = atom("MODSEQ") ->
        % RFC 4731
        detify(mod_seq_value, Src, ModSeqValue, !PS),
        ReturnData = modseq(ModSeqValue)
    ;
        tagged_ext_val(Src, Value, !PS, !IO),
        ReturnData = other(Name, Value)
    ).

:- pred tagged_ext_label(src::in, tagged_ext_label::out, ps::in, ps::out)
    is semidet.

tagged_ext_label(Src, tagged_ext_label(atom(Atom)), !PS) :-
    tagged_label_fchar(Src, C, !PS),
    zero_or_more_chars(tagged_label_char, Src, Cs, !PS),
    string.from_char_list([C | Cs], Atom0),
    string.to_upper(Atom0, Atom).

:- pred tagged_label_fchar(src::in, char::out, ps::in, ps::out) is semidet.

tagged_label_fchar(Src, C, !PS) :-
    next_char(Src, C, !PS),
    tagged_label_fchar(C).

:- pred tagged_label_fchar(char::in) is semidet.

tagged_label_fchar(C) :-
    ( char.is_alpha(C)
    ; C = ('-')
    ; C = ('_')
    ; C = ('.')
    ).

:- pred tagged_label_char(char::in) is semidet.

tagged_label_char(C) :-
    ( tagged_label_fchar(C)
    ; char.is_digit(C)
    ; C = (':')
    ).

:- pred tagged_ext_val(src::in, tagged_ext_val(T)::out, ps::in, ps::out,
    io::di, io::uo) is det <= sequence_set_number(T).

tagged_ext_val(Src, Value, !PS, !IO) :-
    ( sequence_set(Src, SequenceSet, !PS) ->
        Value = sequence_set(SequenceSet)
    ; number(Src, Number, !PS) ->
        Value = number(Number)
    ;
        tagged_ext_complex(Src, Complex, !PS, !IO),
        Value = complex(Complex)
    ).

    % Completely untested.
:- pred tagged_ext_complex(src::in, tagged_ext_complex::out, ps::in, ps::out,
    io::di, io::uo) is det.

tagged_ext_complex(Src, Value, !PS, !IO) :-
    tagged_ext_complex_head(Src, Head, !PS, !IO),
    sp_then_tagged_ext_complexs(Src, Tail, !PS, !IO),
    Value = list([Head | Tail]).

:- pred tagged_ext_complex_head(src::in, tagged_ext_complex::out,
    ps::in, ps::out, io::di, io::uo) is det.

tagged_ext_complex_head(Src, Value, !PS, !IO) :-
    ( next_char(Src, '(', !PS) ->
        tagged_ext_complex(Src, Value, !PS, !IO),
        det_next_char(Src, ')', !PS)
    ;
        astring(Src, AString, !PS, !IO),
        Value = astring(AString)
    ).

:- pred sp_then_tagged_ext_complexs(src::in, list(tagged_ext_complex)::out,
    ps::in, ps::out, io::di, io::uo) is det.

sp_then_tagged_ext_complexs(Src, Values, !PS, !IO) :-
    ( sp(Src, !PS) ->
        tagged_ext_complex(Src, Head, !PS, !IO),
        sp_then_tagged_ext_complexs(Src, Tail, !PS, !IO),
        Values = [Head | Tail] % lcmc
    ;
        Values = []
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
