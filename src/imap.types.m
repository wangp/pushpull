%-----------------------------------------------------------------------------%

:- module imap.types.
:- interface.

:- import_module integer.

:- type atom
    --->    atom(string).           % 1*ATOM-CHAR (keep uppercased)

:- type astring
    --->    astring(string)         % 1*ASTRING-CHAR
    ;       imap_string(imap_string).

:- type imap_string
    --->    quoted(string)          % *QUOTED-CHAR (kept unescaped)
    ;       literal(string).        % *CHAR8

:- type nstring == maybe(imap_string).

:- type tag
    --->    tag(string).            % 1*<any ASTRING-CHAR except "+">

:- type uid
    --->    uid(integer).           % (0 < n < 4,294,967,296)

:- type uidvalidity
    --->    uidvalidity(integer).   % (0 < n < 4,294,967,296)

:- type message_seq_nr
    --->    message_seq_nr(integer). % (0 <= n < 4,294,967,296)

:- type sequence_set(T)
    --->    number(seq_number(T), list(sequence_set(T)))
    ;       range(seq_range(T), list(sequence_set(T))).

:- type seq_number(T)
    --->    number(T)
    ;       star.                   % largest in use

:- type seq_range(T)
    --->    seq_range(seq_number(T), seq_number(T)).

:- type mod_seq_value
    --->    mod_seq_value(integer).  % (0 < n < 18,446,744,073,709,551,615)

:- type mod_seq_valzer
    --->    mod_seq_valzer(integer). % (0 =< n < 18,446,744,073,709,551,615)

:- type search
    --->    search(maybe(charset), search_key).

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
    %;      header(header_field_name, astring)
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

:- type fetch_items
    --->    macro(fetch_macro)
    ;       atts(fetch_att, list(fetch_att)).

:- type fetch_macro
    --->    (all)
    ;       fast
    ;       full.

:- type fetch_att
    --->    body
    ;       body(section_spec, maybe(partial))
    ;       body_peek(section_spec, maybe(partial))
    ;       bodystructure
    ;       envelope
    ;       flags
    ;       internaldate
    ;       rfc822
    ;       rfc822_header
    ;       rfc822_size
    ;       rfc822_text
    ;       uid.

:- type section_spec
    --->    msgtext(section_msgtext).
    %;      part(section_part, section_text).

:- type section_msgtext
    --->    header
    %;      header_fields(header_list)
    %;      header_fields_not(header_list)
    ;       text.

:- type partial
    --->    partial(int, int). % number, nz-number

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
    %;      body(section, optional(number), nstring)
    ;       uid(uid).

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
                message_id  :: nstring
            ).

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

:- func singleton_sequence_set(T) = sequence_set(T).

:- pred make_sequence_set(list(T)::in, sequence_set(T)::out) is semidet.

:- func make_astring(string) = astring.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module imap.charclass.

%-----------------------------------------------------------------------------%

singleton_sequence_set(N) = number(number(N), []).

make_sequence_set([N | Ns], SequenceSet) :-
    % Make it smarter later.
    SequenceSet = number(number(N), list.map(singleton_sequence_set, Ns)).

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
        AString = imap_string(literal(S))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
