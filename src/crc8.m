%-----------------------------------------------------------------------------%

:- module crc8.
:- interface.

:- import_module list.

:- type crc == int. % 8-bit

    % x^8 + x^2 + x + 1 polynomial
:- func crc_8(list(int)) = crc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

crc_8(Xs) = list.foldl(update, Xs, 0x00).

:- func update(int, int) = int.

update(X, Crc0) = fold_up(shift, 0, 7, Crc0 `xor` X) /\ 0xff.

:- func shift(int, int) = int.

shift(_, Crc0) = Crc :-
    ( Crc0 /\ 0x80 = 0x80 ->
        Crc = (Crc0 << 1) `xor` p
    ;
        Crc = (Crc0 << 1)
    ).

:- func p = int.

p = 0x07.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
