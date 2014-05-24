%-----------------------------------------------------------------------------%

:- module imap.io.
:- interface.

:- pred read_crlf_line_chop(pipe::in, io.result(list(int))::out,
    io::di, io::uo) is det.

:- pred read_bytes(pipe::in, int::in, io.res(list(int))::out,
    io::di, io::uo) is det.

:- pred write_command_stream(pipe::in, tag::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- inst open
    --->    open(ground).

%-----------------------------------------------------------------------------%

read_crlf_line_chop(open(Pipe), Res, !IO) :-
    read_crlf_line(Pipe, Res0, [], RevBytes0, !IO),
    (
        Res0 = ok,
        ( RevBytes0 = [_LF, _CR | RevBytes] ->
            list.reverse(RevBytes, Bytes),
            Res = ok(Bytes),
            trace [runtime(env("DEBUG_IMAP")), io(!IO2)] (
                ( string.from_code_unit_list(Bytes, String) ->
                    Stream = io.stderr_stream,
                    io.write_string(Stream, "\x1B\[34;01m", !IO2),
                    io.write_string(Stream, String, !IO2),
                    io.write_string(Stream, "\x1B\[0m\n", !IO2)
                ;
                    true
                )
            )
        ;
            unexpected($module, $pred, "RevBytes0 too short")
        )
    ;
        Res0 = eof,
        Res = eof
        % RevBytes ignored.
    ;
        Res0 = error(Error),
        Res = error(Error)
        % RevBytes ignored.
    ).

read_crlf_line_chop(closed, Res, !IO) :-
    Res = error(io.make_io_error("session closed")).

:- pred read_crlf_line(subprocess::in, io.result::out,
    list(int)::in, list(int)::out, io::di, io::uo) is det.

read_crlf_line(Pipe, Res, !RevBytes, !IO) :-
    read_byte(Pipe, ResByte, !IO),
    (
        ResByte = ok(Byte),
        (
            Byte = lf,
            head_cr(!.RevBytes)
        ->
            cons(Byte, !RevBytes),
            Res = ok
        ;
            cons(Byte, !RevBytes),
            read_crlf_line(Pipe, Res, !RevBytes, !IO)
        )
    ;
        ResByte = eof,
        Res = eof
    ;
        ResByte = error(Error),
        Res = error(Error)
    ).

:- pred head_cr(list(int)::in) is semidet.

head_cr([cr | _]).

:- func lf = int.
:- func cr = int.

lf = 10.
cr = 13.

%-----------------------------------------------------------------------------%

read_bytes(open(Pipe), NumOctets, Res, !IO) :-
    read_bytes(Pipe, NumOctets, Res, !IO).
read_bytes(closed, _NumOctets, Res, !IO) :-
    Res = error(io.make_io_error("session closed")).

%-----------------------------------------------------------------------------%

write_command_stream(open(Pipe), Tag, Xs, Res, !IO) :-
    trace [runtime(env("DEBUG_IMAP")), io(!IO2)] (
        Stream = io.stderr_stream,
        io.write_string(Stream, "\x1B\[32;01m", !IO2),
        list.foldl(io.write_string(Stream), Xs, !IO2),
        io.write_string(Stream, "\x1B\[0m\n", !IO2)
    ),
    write_command_stream_2(open(Pipe), Tag, Xs, Res, !IO),
    flush_output(Pipe, !IO).

write_command_stream(closed, _, _, Res, !IO) :-
    Res = error("session closed").

:- pred write_command_stream_2(pipe::in(open), tag::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

write_command_stream_2(open(Pipe), _Tag, [], Res, !IO) :-
    write_string(Pipe, crlf, Res, !IO).
write_command_stream_2(OpenPipe, Tag, [X | Xs], Res, !IO) :-
    OpenPipe = open(Pipe),
    write_string(Pipe, X, Res0, !IO),
    (
        Res0 = ok,
        ( X = crlf ->
            flush_output(Pipe, !IO),
            wait_for_continuation_request(OpenPipe, Tag, ResContinue, !IO),
            (
                ResContinue = ok,
                write_command_stream_2(OpenPipe, Tag, Xs, Res, !IO)
            ;
                ResContinue = error(Error),
                Res = error(Error)
            )
        ;
            write_command_stream_2(OpenPipe, Tag, Xs, Res, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred wait_for_continuation_request(pipe::in(open), tag::in,
    maybe_error::out, io::di, io::uo) is det.

wait_for_continuation_request(Pipe, Tag, Res, !IO) :-
    wait_for_complete_response(Pipe, Tag, Res0, !IO),
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
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
