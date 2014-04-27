%-----------------------------------------------------------------------------%

:- module imap.parsing.
:- interface.

:- import_module char.

:- type src ---> src.

:- type ps == list(int).

:- pred eof(src::in, ps::in) is semidet.

:- pred sp(src::in, ps::in, ps::out) is semidet.

:- pred next_char(src, char, ps, ps).
:- mode next_char(in, in, in, out) is semidet.
:- mode next_char(in, out, in, out) is semidet.

:- pred zero_or_more(pred(src, T, ps, ps), src, list(T), ps, ps).
:- mode zero_or_more(in(pred(in, out, in, out) is semidet), in, out, in, out)
    is det.

:- pred one_or_more(pred(src, T, ps, ps), src, list(T), ps, ps).
:- mode one_or_more(in(pred(in, out, in, out) is semidet), in, out, in, out)
    is semidet.

:- pred zero_or_more_chars(pred(char), src, list(char), ps, ps).
:- mode zero_or_more_chars(in(pred(in) is semidet), in, out, in, out) is det.

:- pred one_or_more_chars(pred(char), src, list(char), ps, ps).
:- mode one_or_more_chars(in(pred(in) is semidet), in, out, in, out)
    is semidet.

    % chars_ci(Expected, Src, !PS)
    % Expected must be written in uppercase.
    %
:- pred chars_ci(string::in, src::in, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

eof(_, []).

sp(_, [0x20 | PS], PS).

% exact(C, _, [I | PS], PS) :-
%     char.to_int(C, I).

next_char(_, C, [I | PS], PS) :-
    char.to_int(C, I).

zero_or_more(P, Src, Xs, !PS) :-
    ( P(Src, X, !PS) ->
        zero_or_more(P, Src, Xs1, !PS),
        Xs = [X | Xs1] % lcmc
    ;
        Xs = []
    ).

one_or_more(P, Src, [X | Xs], !PS) :-
    P(Src, X, !PS),
    zero_or_more(P, Src, Xs, !PS).

zero_or_more_chars(P, Src, Cs, !PS) :-
    (
        next_char(Src, C, !PS),
        P(C)
    ->
        zero_or_more_chars(P, Src, Cs1, !PS),
        Cs = [C | Cs1] % lcmc
    ;
        Cs = []
    ).

one_or_more_chars(P, Src, [C | Cs], !PS) :-
    next_char(Src, C, !PS),
    P(C),
    zero_or_more_chars(P, Src, Cs, !PS).

%-----------------------------------------------------------------------------%

chars_ci(Expected, Src, !PS) :-
    chars_ci_2(Expected, Src, 0, !PS).

:- pred chars_ci_2(string::in, src::in, int::in, ps::in, ps::out) is semidet.

chars_ci_2(Expected, Src, Index0, !PS) :-
    ( string.unsafe_index_next(Expected, Index0, Index1, ExpectC) ->
        next_char(Src, C, !PS),
        char.to_upper(C, ExpectC), % ExpectC uppercase
        chars_ci_2(Expected, Src, Index1, !PS)
    ;
        % Matched entire token.
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
