%-----------------------------------------------------------------------------%

:- module test_crc16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module crc16.

%-----------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test, cases, !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(String, !IO) :-
    string.to_code_unit_list(String, Octets),
    Crc = crc_ccitt(Octets),
    io.format("0x%04x\n", [i(Crc)], !IO).

:- func cases = list(string).

cases = [
    "",
    "123456789"
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
