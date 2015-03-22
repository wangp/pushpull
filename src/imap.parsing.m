%-----------------------------------------------------------------------------%

:- module imap.parsing.
:- interface.

:- import_module char.

:- type src
    --->    src(pipe).

:- type ps == list(int).

:- type fail_exception
    --->    fail_exception.

%-----------------------------------------------------------------------------%

:- pred eof(src::in, ps::in) is semidet.

:- pred sp(src::in, ps::in, ps::out) is semidet.

:- pred det_sp(src::in, ps::in, ps::out) is det.

:- pred sp_then(pred(src, T, ps, ps), src, T, ps, ps).
:- mode sp_then(in(pred(in, out, in, out) is semidet), in, out, in, out)
    is semidet.

:- pred next_char(src, char, ps, ps).
:- mode next_char(in, in, in, out) is semidet.
:- mode next_char(in, out, in, out) is semidet.

:- pred det_next_char(src::in, char::in, ps::in, ps::out) is det.

:- pred detify(pred(src, T, ps, ps), src, T, ps, ps).
:- mode detify(pred(in, out, in, out) is semidet, in, out, in, out) is det.

:- pred digit_char(src::in, char::out, ps::in, ps::out) is semidet.
:- pred digit(src::in, int::out, ps::in, ps::out) is semidet.
:- pred two_digit(src::in, int::out, ps::in, ps::out) is semidet.
:- pred four_digit(src::in, int::out, ps::in, ps::out) is semidet.

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

:- import_module exception.
:- import_module string.

%-----------------------------------------------------------------------------%

eof(_, []).

sp(_, [0x20 | PS], PS).

det_sp(Src, !PS) :-
    ( sp(Src, !PS) ->
        true
    ;
        throw(fail_exception)
    ).

sp_then(P, Src, X, !PS) :-
    sp(Src, !PS),
    P(Src, X, !PS).

next_char(_, C, [I | PS], PS) :-
    char.to_int(C, I).

det_next_char(Src, C, !PS) :-
    ( next_char(Src, C, !PS) ->
        true
    ;
        throw(fail_exception)
    ).

detify(P, Src, X, !PS) :-
    ( P(Src, X0, !PS) ->
        X = X0
    ;
        throw(fail_exception)
    ).

digit_char(Src, C, !PS) :-
    next_char(Src, C, !PS),
    char.is_digit(C).

digit(Src, I, !PS) :-
    digit_char(Src, C, !PS),
    char.decimal_digit_to_int(C, I).

two_digit(Src, I, !PS) :-
    digit(Src, A, !PS),
    digit(Src, B, !PS),
    I = (A * 10) + B.

four_digit(Src, I, !PS) :-
    digit(Src, A, !PS),
    digit(Src, B, !PS),
    digit(Src, C, !PS),
    digit(Src, D, !PS),
    I = (A * 1000) + (B * 100) + (C * 10) + D.

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
