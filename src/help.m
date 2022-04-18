% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module help.
:- interface.

:- import_module io.

:- func usage_text(string) = string.

:- pred print_help(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

usage_text(ProgName) =
    "Usage: " ++ ProgName ++ " [OPTIONS] CONFIG-FILE PAIRING".

print_help(ProgName, !IO) :-
    Lines = [
        usage_text(ProgName),
        "",
        "Options:",
        "  -h, --help                  Display usage and options.",
        "  --test-auth-only            Try logging into IMAP server only.",
        "  --allow-mass-delete=NUM     Sync even if more than NUM messages",
        "                              are missing from the Maildir folder."
    ],
    io.output_stream(Stream, !IO),
    list.foldl(write_string_nl(Stream), Lines, !IO).

:- pred write_string_nl(io.text_output_stream::in, string::in, io::di, io::uo)
    is det.

write_string_nl(Stream, S, !IO) :-
    io.write_string(Stream, S, !IO),
    io.nl(Stream, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
