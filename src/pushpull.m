% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module pushpull.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module main.

main(!IO) :-
    real_main(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
