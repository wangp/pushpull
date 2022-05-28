% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module verify_file.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

:- import_module binary_string.

:- pred verify_file(string::in, binary_string::in, maybe_error(bool)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

verify_file(FileName, Expected, Res, !IO) :-
    io.open_binary_input(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        verify(Stream, Expected, 0, length(Expected), Res, !IO),
        io.close_binary_input(Stream, !IO)
    ;
        ResOpen = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred verify(io.binary_input_stream::in, binary_string::in, int::in, int::in,
    maybe_error(bool)::out, io::di, io::uo) is det.

verify(Stream, Expected, Pos0, EndPos, Res, !IO) :-
    io.read_byte(Stream, ReadRes, !IO),
    (
        ReadRes = ok(Byte),
        ( Pos0 < EndPos ->
            ( binary_string.unsafe_byte(Expected, Pos0, Byte) ->
                verify(Stream, Expected, Pos0 + 1, EndPos, Res, !IO)
            ;
                Res = ok(no)
            )
        ;
            Res = ok(no)
        )
    ;
        ReadRes = eof,
        ( Pos0 = EndPos ->
            Res = ok(yes) % matched
        ;
            Res = ok(no)
        )
    ;
        ReadRes = error(Error),
        Res = error(io.error_message(Error))
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
