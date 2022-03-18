% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module imap.read_write.
:- interface.

:- pred read_crlf_line_chop(pipe::in, io.result(list(int))::out,
    io::di, io::uo) is det.

:- pred read_bytes(pipe::in, int::in, io.result(binary_string)::out,
    io::di, io::uo) is det.

:- pred write_command_stream(pipe::in, tag::in, list(chunk)::in,
    maybe_result::out, io::di, io::uo) is det.

:- pred write_command_stream_sensitive(pipe::in, tag::in, list(chunk)::in,
    maybe_result::out, io::di, io::uo) is det.

:- pred write_command_stream_inner(pipe::in, tag::in, list(chunk)::in,
    bool::in, maybe_result::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module require.

:- inst open
    --->    open(ground).

%-----------------------------------------------------------------------------%

read_crlf_line_chop(open(Pipe), Res, !IO) :-
    read_crlf_line(Pipe, Res0, empty, Bytes, !IO),
    (
        Res0 = ok,
        Res = ok(Bytes),
        trace [runtime(env("DEBUG_IMAP")), io(!IO2)] (
            Stream = io.stderr_stream,
            debug_response_bytes(Stream, Bytes, !IO2)
        )
    ;
        Res0 = eof,
        Res = eof
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).
read_crlf_line_chop(closed, Res, !IO) :-
    Res = error(io.make_io_error("session closed")).

:- type prior
    --->    empty
    ;       cr.

:- pred read_crlf_line(bio::in, io.result::out, prior::in, list(int)::out,
    io::di, io::uo) is det.

read_crlf_line(Bio, Res, Prior, Bytes, !IO) :-
    bio_read_byte(Bio, ResByte, !IO),
    (
        ResByte = ok(Byte),
        (
            Prior = empty,
            ( Byte = cr ->
                read_crlf_line(Bio, Res, cr, Bytes, !IO)
            ;
                Bytes = [Byte | BytesTail],
                read_crlf_line(Bio, Res, empty, BytesTail, !IO) % lcmc
            )
        ;
            Prior = cr,
            ( Byte = lf ->
                Res = ok,
                Bytes = [] % drop CR LF
            ; Byte = cr ->
                Bytes = [cr | BytesTail],
                read_crlf_line(Bio, Res, cr, BytesTail, !IO) % lcmc
            ;
                Bytes = [cr, Byte | BytesTail],
                read_crlf_line(Bio, Res, empty, BytesTail, !IO) % lcmc
            )
        )
    ;
        ResByte = eof,
        Res = eof,
        (
            Prior = empty,
            Bytes = []
        ;
            Prior = cr,
            Bytes = [cr]
        )
    ;
        ResByte = error(Error),
        Res = error(Error),
        Bytes = []
    ).

:- func lf = int.
:- func cr = int.

lf = 10.
cr = 13.

%-----------------------------------------------------------------------------%

read_bytes(open(Bio), NumOctets, Res, !IO) :-
    bio_read_bytes(Bio, NumOctets, Res, !IO).
read_bytes(closed, _NumOctets, Res, !IO) :-
    Res = error(io.make_io_error("session closed")).

%-----------------------------------------------------------------------------%

write_command_stream(Pipe, Tag, Xs, Res, !IO) :-
    Sensitive = no,
    write_command_stream_inner(Pipe, Tag, Xs, Sensitive, Res, !IO).

write_command_stream_sensitive(Pipe, Tag, Xs, Res, !IO) :-
    Sensitive = yes,
    write_command_stream_inner(Pipe, Tag, Xs, Sensitive, Res, !IO).

write_command_stream_inner(Pipe, Tag, Xs, Sensitive, Res, !IO) :-
    (
        Pipe = closed,
        Res = error("session closed")
    ;
        Pipe = open(Bio),
        trace [runtime(env("DEBUG_IMAP")), io(!IO2)] (
            Stream = io.stderr_stream,
            (
                Sensitive = no,
                debug_chunks(Stream, Xs, !IO2)
            ;
                Sensitive = yes,
                debug_chunks(Stream, [string("<command omitted>")], !IO2)
            )
        ),
        write_command_stream_chunks(Bio, Tag, Xs, Res, !IO)
    ).

:- pred write_command_stream_chunks(bio::in, tag::in, list(chunk)::in,
    maybe_result::out, io::di, io::uo) is det.

write_command_stream_chunks(Bio, _Tag, [], Res, !IO) :-
    bio_write_string(Bio, crlf, Res0, !IO),
    (
        Res0 = ok,
        bio_flush(Bio, Res1, !IO),
        (
            Res1 = ok,
            Res = ok
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).
write_command_stream_chunks(Bio, Tag, [X | Xs], Res, !IO) :-
    write_chunk(Bio, X, Res0, !IO),
    (
        Res0 = ok,
        (
            ( X = string(_)
            ; X = binary(_)
            ),
            write_command_stream_chunks(Bio, Tag, Xs, Res, !IO)
        ;
            X = crlf_wait,
            bio_flush(Bio, Res1, !IO),
            (
                Res1 = ok,
                wait_for_continuation_request(Bio, Tag, ResContinue, !IO),
                (
                    ResContinue = ok,
                    write_command_stream_chunks(Bio, Tag, Xs, Res, !IO)
                ;
                    ( ResContinue = eof
                    ; ResContinue = error(_)
                    ),
                    Res = ResContinue
                )
            ;
                Res1 = error(Error),
                Res = error(Error)
            )
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred write_chunk(bio::in, chunk::in, maybe_error::out, io::di, io::uo)
    is det.

write_chunk(Bio, Chunk, Res, !IO) :-
    (
        Chunk = string(String),
        bio_write_string(Bio, String, Res, !IO)
    ;
        Chunk = binary(BinaryString),
        bio_write_binary_string(Bio, BinaryString, Res, !IO)
    ;
        Chunk = crlf_wait,
        bio_write_string(Bio, crlf, Res, !IO)
    ).

:- pred wait_for_continuation_request(bio::in, tag::in, maybe_result::out,
    io::di, io::uo) is det.

wait_for_continuation_request(Bio, Tag, Res, !IO) :-
    wait_for_complete_response(open(Bio), Tag, Res0, !IO),
    (
        Res0 = ok(Response),
        % XXX do something with the rest of the response
        Response = complete_response(_, FinalMaybeTag, _),
        (
            FinalMaybeTag = continue,
            Res = ok
        ;
            ( FinalMaybeTag = tagged(_, ok)
            ; FinalMaybeTag = tagged(_, no)
            ; FinalMaybeTag = tagged(_, bad)
            ),
            Res = error("expected continuation request")
        ;
            FinalMaybeTag = bye,
            Res = error("received BYE")
        )
    ;
        ( Res0 = eof
        ; Res0 = error(_)
        ),
        Res = convert(Res0)
    ).

:- func crlf = string.

crlf = "\r\n".

%-----------------------------------------------------------------------------%

:- pred debug_response_bytes(io.output_stream::in, list(int)::in,
    io::di, io::uo) is det.

debug_response_bytes(Stream, Bytes, !IO) :-
    io.write_string(Stream, "\x1B\[34;01m", !IO),
    list.foldl(debug_response_byte(Stream), Bytes, !IO),
    io.write_string(Stream, "\x1B\[0m\n", !IO).

:- pred debug_response_byte(io.output_stream::in, int::in, io::di, io::uo)
    is det.

debug_response_byte(Stream, Byte, !IO) :-
    ( char.to_int(Char, Byte) ->
        io.write_char(Stream, Char, !IO)
    ;
        io.write_char(Stream, '?', !IO)
    ).

:- pred debug_chunks(io.output_stream::in, list(chunk)::in,
    io::di, io::uo) is det.

debug_chunks(Stream, Xs, !IO) :-
    io.write_string(Stream, "\x1B\[32;01m", !IO),
    list.foldl(debug_chunk(Stream), Xs, !IO),
    io.write_string(Stream, "\x1B\[0m\n", !IO).

:- pred debug_chunk(io.output_stream::in, chunk::in, io::di, io::uo) is det.

debug_chunk(Stream, X, !IO) :-
    (
        X = string(String),
        io.write_string(Stream, String, !IO)
    ;
        X = binary(BinaryString),
        io.format(Stream, "<%d bytes>", [i(length(BinaryString))], !IO)
    ;
        X = crlf_wait,
        io.write_string(Stream, "<wait>", !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
