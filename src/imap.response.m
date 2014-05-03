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
    ;       highestmodseq(mod_seq_value)
    ;       nomodseq
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
    ;       search(list(integer), maybe(mod_seq_value))
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

:- type message_data
    --->    expunge(integer)
    ;       fetch(integer, msg_atts). % non-empty

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

literal(src(Pipe), NumOctets, literal(String), NewPS, !IO) :-
    read_bytes(Pipe, NumOctets, ResBytes, !IO),
    (
        ResBytes = ok(Bytes),
        % XXX support other encodings and binaries
        ( string.from_code_unit_list(Bytes, StringPrime) ->
            String = StringPrime
        ;
            throw(fail_exception)
        ),
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
    ; mailbox_data(Src, MailboxData, !PS) ->
        ResponseData = mailbox_data(MailboxData)
    ; capability_data(Src, Caps, !PS) ->
        ResponseData = capability_data(Caps)
    ;
        message_data(Src, MessageData, !PS, !IO),
        ResponseData = message_data(MessageData)
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
            MailboxData = search(Numbers, MaybeModSeqValue)
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
        zero_or_more(sp_then(flag), Src, FlagsTail, !PS),
        Flags = [Flag | FlagsTail]
    ;
        Flags = []
    ),
    next_char(Src, ')', !PS).

:- pred message_data(src::in, message_data::out, ps::in, ps::out,
    io::di, io::uo) is det.

message_data(Src, MessageData, !PS, !IO) :-
    (
        nz_number(Src, Number, !PS),
        sp(Src, !PS),
        atom(Src, Atom, !PS)
    ->
        ( Atom = atom("EXPUNGE") ->
            MessageData = expunge(Number)
        ; Atom = atom("FETCH") ->
            message_data_fetch(Src, Number, MessageData, !PS, !IO)
        ;
            throw(fail_exception)
        )
    ;
        throw(fail_exception)
    ).

:- pred message_data_fetch(src::in, integer::in, message_data::out,
    ps::in, ps::out, io::di, io::uo) is det.

message_data_fetch(Src, Number, MessageData, !PS, !IO) :-
    det_sp(Src, !PS),
    det_next_char(Src, '(', !PS),
    msg_att(Src, Att, !PS, !IO),
    sp_then_msg_atts(Src, Atts, !PS, !IO),
    det_next_char(Src, ')', !PS),
    MessageData = fetch(Number, [Att | Atts]).

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
    (
        atom(Src, Atom, !PS),
        sp(Src, !PS)
    ->
        ( msg_att_semi(Src, Atom, AttPrime, !PS) ->
            Att = AttPrime
        ;
            msg_att_io(Src, Atom, Att, !PS, !IO)
        )
    ;
        throw(fail_exception)
    ).

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

    nstring(Src, Env ^ message_id, !PS, !IO),
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

:- pred month(string, month).
:- mode month(in, out) is semidet.
:- mode month(out, in) is det.

month("JAN", jan).
month("FEB", feb).
month("MAR", mar).
month("APR", apr).
month("MAY", may).
month("JUN", jun).
month("JUL", jul).
month("AUG", aug).
month("SEP", sep).
month("OCT", oct).
month("NOV", nov).
month("DEC", dec).

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

:- pred uniqueid(src::in, uid::out, ps::in, ps::out) is semidet.

uniqueid(Src, uid(Number), !PS) :-
    nz_number(Src, Number, !PS).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
