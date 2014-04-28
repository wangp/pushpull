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
