% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module imap.charclass.
:- interface.

:- import_module char.

:- pred 'ATOM-CHAR'(char::in) is semidet.

:- pred 'ASTRING-CHAR'(char::in) is semidet.

:- pred 'QUOTED-CHAR'(char::in) is semidet.

:- pred 'TEXT-CHAR'(char::in) is semidet.

:- pred tag_char(char::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%

    % from RFC 2060
:- pred 'CHAR'(char::in) is semidet.

'CHAR'(C) :-
    char.to_int(C, I),
    0x01 =< I, I =< 0x7f.

    % from RFC 2060
:- pred 'CTL'(char::in) is semidet.

'CTL'(C) :-
    char.to_int(C, I),
    ( 0x00 =< I, I =< 0x1f
    ; I = 0x7f
    ).

:- pred 'CR_or_LF'(char::in) is semidet.

'CR_or_LF'(C) :-
    % Keep out of clause head to work around a Mercury compiler bug:
    % the .opt file is generated with doubly-escaped character literals.
    ( C = '\r'
    ; C = '\n'
    ).

:- pred 'SP'(char::in) is semidet.

'SP'(' ').

'ATOM-CHAR'(C) :-
    'CHAR'(C),
    not atom_specials(C).

:- pred atom_specials(char::in) is semidet.

atom_specials('(').
atom_specials(')').
atom_specials('{').
atom_specials(C) :- 'SP'(C).
atom_specials(C) :- 'CTL'(C).
atom_specials(C) :- list_wildcards(C).
atom_specials(C) :- quoted_specials(C).
atom_specials(C) :- resp_specials(C).

'ASTRING-CHAR'(C) :-
    ( 'ATOM-CHAR'(C)
    ; resp_specials(C)
    ).

'QUOTED-CHAR'(C) :-
    'TEXT-CHAR'(C). % including quoted_specials after escaping

:- pred list_wildcards(char::in) is semidet.

list_wildcards('%').
list_wildcards('*').

:- pred resp_specials(char::in) is semidet.

resp_specials(']').

:- pred quoted_specials(char::in) is semidet.

quoted_specials('\\').
quoted_specials('"').

tag_char(C) :-
    'ASTRING-CHAR'(C),
    C \= ('+').

'TEXT-CHAR'(C) :-
    'CHAR'(C),
    not 'CR_or_LF'(C).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
