%-----------------------------------------------------------------------------%

:- module imap.types.
:- interface.

:- import_module diet.
:- import_module enum.
:- import_module integer.
:- import_module set.

:- import_module binary_string.

:- type atom
    --->    atom(string).           % 1*ATOM-CHAR (keep uppercased)

:- type astring
    --->    astring(string)         % 1*ASTRING-CHAR (case sensitive)
    ;       imap_string(imap_string).

:- type imap_string
    --->    quoted(string)          % *QUOTED-CHAR (kept unescaped)
    ;       literal(binary_string). % *CHAR8

:- type nstring == maybe(imap_string).

:- type mailbox
    --->    inbox
    ;       astring(astring).       % not INBOX (case-insensitive)

:- type tag
    --->    tag(string).            % 1*<any ASTRING-CHAR except "+">

:- type uid
    --->    uid(integer).           % (0 < n < 4,294,967,296)

:- type uidvalidity
    --->    uidvalidity(integer).   % (0 < n < 4,294,967,296)

:- type message_seq_nr
    --->    message_seq_nr(integer). % (0 <= n < 4,294,967,296)

:- type sequence_set(T)
    --->    cons(sequence_set_element(T), sequence_set(T))
    ;       last(sequence_set_element(T)).

:- type sequence_set_element(T)
    --->    element(seq_number(T))
    ;       range(seq_number(T), seq_number(T)).

:- type seq_number(T)
    --->    number(T)
    ;       star.                   % largest in use

:- typeclass sequence_set_number(T) where [
    func from_nz_number(integer) = T,
    func to_nz_number(T) = integer
].

:- instance sequence_set_number(message_seq_nr).
:- instance sequence_set_number(uid).

:- type mod_seq_value
    --->    mod_seq_value(integer).  % (0 < n < 18,446,744,073,709,551,615)

:- type mod_seq_valzer
    --->    mod_seq_valzer(integer). % (0 =< n < 18,446,744,073,709,551,615)

:- type search
    --->    search(
                maybe(charset),
                search_key,
                maybe(list(search_return_option))
            ).

:- type charset
    --->    charset(astring).

:- type search_key
    --->    (all)
    ;       answered
    ;       bcc(astring)
    %;      before(date)
    ;       body(astring)
    ;       cc(astring)
    ;       deleted
    ;       flagged
    ;       from(astring)
    %;      keyword(flag_keyword)
    ;       new
    ;       old
    %;      on(date)
    ;       recent
    ;       seen
    %;      since(date)
    ;       subject(astring)
    ;       text(astring)
    ;       to(astring)
    ;       unanswered
    ;       undeleted
    ;       unflagged
    %;      unkeyword(flag_keyword)
    ;       unseen
    ;       draft
    ;       header(astring, astring)
    %;      larger(number)
    ;       not(search_key)
    ;       or(search_key, search_key)
    %;      sentbefore(date)
    %;      senton(date)
    %;      sentsince(date)
    %;      smaller(number)
    %;      uid(sequence_set)
    %;      undraft(sequence_set)
    ;       and(search_key, list(search_key))
    ;       modseq(/* [entry_name, entry_type_req] */ mod_seq_valzer).

:- type search_return_option
    --->    min
    ;       max
    ;       (all)
    ;       count.

:- type search_return_data(T)
    --->    min(integer)
    ;       max(integer)
    ;       all(sequence_set(T))
    ;       count(integer)
    ;       modseq(mod_seq_value)
    ;       other(tagged_ext_label, tagged_ext_val(T)).

:- type tagged_ext_label
    --->    tagged_ext_label(atom).

:- type tagged_ext_val(T)
    --->    sequence_set(sequence_set(T))
    ;       number(integer)
    ;       complex(tagged_ext_complex).

:- type tagged_ext_complex
    --->    astring(astring)
    ;       list(list(tagged_ext_complex)). % non-empty

:- type fetch_items
    --->    macro(fetch_macro)
    ;       atts(fetch_att, list(fetch_att)).

:- type fetch_macro
    --->    (all)
    ;       fast
    ;       full.

:- type fetch_att
    --->    body
    ;       body(section, maybe(partial))
    ;       body_peek(section, maybe(partial))
    ;       bodystructure
    ;       envelope
    ;       flags
    ;       internaldate
    ;       rfc822
    ;       rfc822_header
    ;       rfc822_size
    ;       rfc822_text
    ;       uid
    ;       modseq.

:- type header_field_name == astring.

:- type partial
    --->    partial(int, int). % number, nz-number

:- type fetch_modifier
    --->    changedsince(mod_seq_value).

:- type msg_atts == list(msg_att).

:- type msg_att
    % Dynamic.
    --->    flags(list(flag_fetch))
    % The rest are static.
    ;       envelope(envelope)
    ;       internaldate(date_time)
    ;       rfc822(nstring)
    ;       rfc822_header(nstring)
    ;       rfc822_text(nstring)
    ;       rfc822_size(integer)
    %;      body(body)
    %;      bodystructure(body)
    ;       body(section, maybe(origin_octet), nstring)
    ;       uid(uid)
    ;       modseq(mod_seq_value).

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

:- type flag_fetch
    --->    flag(flag)
    ;       recent.

:- type envelope
    --->    envelope(
                date        :: nstring,
                subject     :: nstring,
                from        :: list(address),
                sender      :: list(address),
                reply_to    :: list(address),
                to          :: list(address),
                cc          :: list(address),
                bcc         :: list(address),
                in_reply_to :: nstring,
                message_id  :: maybe_message_id
            ).

:- type maybe_message_id
    --->    message_id(string)
    ;       nil.

:- type address
    --->    address(
                name        :: nstring,
                adl         :: nstring,
                mailbox     :: nstring,
                host        :: nstring
            ).

:- type date_time
    --->    date_time(
                day         :: int,
                month       :: month,
                year        :: int,
                time        :: time,
                zone        :: zone
            ).

:- type month
    --->    jan ; feb ; mar ; apr ; may ; jun
    ;       jul ; aug ; sep ; oct ; nov ; dec.

:- type time
    --->    time(int, int, int).

:- type zone
    --->    zone(string).   % +HHMM or -HHMM

:- type origin_octet == integer.

:- type section
    --->    entire_message
    ;       msgtext(section_msgtext).
    %;      part(section_part, section_text).

:- type section_msgtext
    --->    header
    ;       header_fields(header_field_name, list(header_field_name))
    ;       header_fields_not(header_field_name, list(header_field_name))
    ;       text.

:- type appenduid
    --->    appenduid(uidvalidity, uid_set).

:- type uid_set == set(uid_range).

:- type uid_range
    --->    uid_range(uid, uid).    % low, high (inclusive)

:- type store_operation
    --->    replace
    ;       add
    ;       remove.

:- type store_silence
    --->    silent
    ;       not_silent.

%-----------------------------------------------------------------------------%

    % XXX diet uses int internally but UIDs may overflow 32-bit signed ints
:- instance enum(uid).

:- pred mod_seq_value < mod_seq_value.
:- mode in < in is semidet.

:- pred mod_seq_value > mod_seq_value.
:- mode in > in is semidet.

:- func singleton_sequence_set(T) = sequence_set(T)
    <= sequence_set_number(T).

:- pred make_sequence_set(list(T)::in, sequence_set(T)::out) is semidet
    <= sequence_set_number(T).

:- pred sequence_set_only_numbers(sequence_set(T)::in) is semidet
    <= sequence_set_number(T).

:- pred diet_to_sequence_set(diet(T)::in, sequence_set(T)::out) is semidet
    <= (enum(T), sequence_set_number(T)).

:- func make_astring(string) = astring.

:- pred system_flag(atom::in, system_flag::out) is semidet.

:- func flag_to_imap_syntax(flag) = string.

:- pred month(string, month).
:- mode month(in, out) is semidet.
:- mode month(out, in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module integer.
:- import_module string.

:- import_module imap.charclass.

%-----------------------------------------------------------------------------%

    % XXX diet uses int internally but UIDs may overflow 32-bit signed ints
:- instance enum(uid) where [
    from_int(Int) = uid(integer(Int)) :- Int > 0,
    to_int(uid(Integer)) = Int :-
    (
        ( string.to_int(to_string(Integer), IntPrime) ->
            Int = IntPrime
        ;
            sorry($module, $pred,
                "UID to int conversion failed (probably overflow)")
        )
    )
].

%-----------------------------------------------------------------------------%

mod_seq_value(X) < mod_seq_value(Y) :- X < Y.

mod_seq_value(X) > mod_seq_value(Y) :- X > Y.

%-----------------------------------------------------------------------------%

:- instance sequence_set_number(message_seq_nr) where [
    from_nz_number(N) = message_seq_nr(N),
    to_nz_number(message_seq_nr(N)) = N
].

:- instance sequence_set_number(uid) where [
    from_nz_number(N) = uid(N),
    to_nz_number(uid(N)) = N
].

singleton_sequence_set(N) = last(element(number(N))).

make_sequence_set(Ns0, SequenceSet) :-
    list.sort(Ns0, [N | Ns]),
    make_sequence_set_2(N, N, Ns, SequenceSet).

:- pred make_sequence_set_2(T::in, T::in, list(T)::in, sequence_set(T)::out)
    is det <= sequence_set_number(T).

make_sequence_set_2(Lo, Hi, [], SequenceSet) :-
    make_sequence_set_element(Lo, Hi, Elem),
    SequenceSet = last(Elem).
make_sequence_set_2(Lo, Hi, [N | Ns], SequenceSet) :-
    ( to_nz_number(Hi) + one = to_nz_number(N) ->
        make_sequence_set_2(Lo, N, Ns, SequenceSet)
    ;
        make_sequence_set_element(Lo, Hi, Elem),
        make_sequence_set_2(N, N, Ns, SequenceSetTail),
        SequenceSet = cons(Elem, SequenceSetTail)
    ).

:- pred make_sequence_set_element(T::in, T::in, sequence_set_element(T)::out)
    is det <= sequence_set_number(T).

make_sequence_set_element(Lo, Hi, Elem) :-
    ( Lo = Hi ->
        Elem = element(number(Lo))
    ;
        Elem = range(number(Lo), number(Hi))
    ).

sequence_set_only_numbers(Set) :-
    require_complete_switch [Set]
    (
        Set = cons(Elem, Tail),
        sequence_set_element_only_numbers(Elem),
        sequence_set_only_numbers(Tail)
    ;
        Set = last(Elem),
        sequence_set_element_only_numbers(Elem)
    ).

:- pred sequence_set_element_only_numbers(sequence_set_element(T)::in)
    is semidet.

sequence_set_element_only_numbers(Elem) :-
    require_complete_switch [Elem]
    (
        Elem = element(number(_))
    ;
        Elem = range(number(_), number(_))
    ).

diet_to_sequence_set(Diet, SequenceSet) :-
    to_sorted_interval_list(Diet, Intervals),
    intervals_to_sequence_set(Intervals, SequenceSet).

:- pred intervals_to_sequence_set(list({T, T})::in, sequence_set(T)::out)
    is semidet.

intervals_to_sequence_set([H | T], SequenceSet) :-
    (
        T = [],
        interval_to_sequence_set_element(H, Element),
        SequenceSet = last(Element)
    ;
        T = [_ | _],
        intervals_to_sequence_set(T, SequenceSet0),
        interval_to_sequence_set_element(H, Element),
        SequenceSet = cons(Element, SequenceSet0)
    ).

:- pred interval_to_sequence_set_element({T, T}::in,
    sequence_set_element(T)::out) is det.

interval_to_sequence_set_element({Lo, Hi}, Element) :-
    ( Lo = Hi ->
        Element = element(number(Lo))
    ;
        Element = range(number(Lo), number(Hi))
    ).

%-----------------------------------------------------------------------------%

make_astring(S) = AString :-
    ( S = "" ->
        AString = imap_string(quoted(""))
    ; string.all_match('ASTRING-CHAR', S) ->
        AString = astring(S)
    ; string.all_match('QUOTED-CHAR', S) ->
        % Should use literal if S too long.
        AString = imap_string(quoted(S))
    ;
        AString = imap_string(literal(from_string(S)))
    ).

%-----------------------------------------------------------------------------%

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

flag_to_imap_syntax(system(answered)) = "\\Answered".
flag_to_imap_syntax(system(flagged)) = "\\Flagged".
flag_to_imap_syntax(system(deleted)) = "\\Deleted".
flag_to_imap_syntax(system(seen)) = "\\Seen".
flag_to_imap_syntax(system(draft)) = "\\Draft".
flag_to_imap_syntax(system(extension(atom(Atom)))) = "\\" ++ Atom.
flag_to_imap_syntax(keyword(atom(Atom))) = Atom.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
