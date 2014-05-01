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

:- type tag
    --->    tag(string).            % 1*<any ASTRING-CHAR except "+">

:- type uid
    --->    uid(integer).           % (0 < n < 4,294,967,296)

:- type uidvalidity
    --->    uidvalidity(integer).   % (0 < n < 4,294,967,296)

:- type message_seq_nr
    --->    message_seq_nr(integer). % (0 <= n < 4,294,967,296)

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

:- func make_astring(string) = astring.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module imap.charclass.

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
