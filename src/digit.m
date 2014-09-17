%-----------------------------------------------------------------------------%

:- module digit.
:- interface.

:- import_module char.

:- pred decimal_digit_to_int(char::in, int::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

decimal_digit_to_int(C, I) :-
    char.digit_to_int(C, I), % deprecated
    I < 10.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
