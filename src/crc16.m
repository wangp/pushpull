% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module crc16.
:- interface.

:- import_module list.

:- type crc == int. % 16-bit

:- func crc_ccitt(list(int)) = crc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

crc_ccitt(Xs) = Crc :-
    Initial = 0xffff,
    Crc = list.foldl(update, Xs, Initial).

:- func update(int, crc) = crc.

update(X, Crc0) = fold_down(update_bit(X), 0, 7, Crc0) /\ 0xffff.

:- func update_bit(int, int, crc) = crc.

update_bit(X, I, Crc0) = Crc1 :-
    XI = unchecked_right_shift(X, I),
    C15 = unchecked_right_shift(Crc0, 15),
    ( xor(XI, C15) /\ 1 = 1 ->
        Crc1 = (Crc0 << 1) `xor` p_ccitt
    ;
        Crc1 = (Crc0 << 1)
    ).

:- func p_ccitt = int.

p_ccitt = 0x1021.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
