%-----------------------------------------------------------------------------%

:- module imap.io.
:- interface.

:- pred read_crlf_line_chop(pipe::in, io.result(list(int))::out,
    io::di, io::uo) is det.

:- pred write_command_stream(pipe::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

read_crlf_line_chop(open(Pipe), Res, !IO) :-
    read_crlf_line(Pipe, Res0, [], RevBytes0, !IO),
    (
        Res0 = ok,
        ( RevBytes0 = [_LF, _CR | RevBytes] ->
            Res = ok(reverse(RevBytes))
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

write_command_stream(open(Pipe), Xs, Res, !IO) :-
    trace [runtime(env("DEBUG_IMAP")), io(!IO2)] (
        Stream = io.stderr_stream,
        io.write(Stream, Xs, !IO2),
        io.nl(Stream, !IO2)
    ),
    write_command_stream_2(Pipe, Xs, Res, !IO),
    flush_output(Pipe, !IO).

write_command_stream(closed, _, Res, !IO) :-
    Res = error("session closed").

:- pred write_command_stream_2(subprocess::in, list(string)::in,
    maybe_error::out, io::di, io::uo) is det.

write_command_stream_2(Pipe, [], Res, !IO) :-
    write_string(Pipe, crlf, Res, !IO).
write_command_stream_2(Pipe, [X | Xs], Res, !IO) :-
    write_string(Pipe, X, Res0, !IO),
    (
        Res0 = ok,
        ( X = crlf ->
            % XXX Wait for continuation request from server.
            flush_output(Pipe, !IO),
            write_command_stream_2(Pipe, Xs, Res, !IO)
        ;
            write_command_stream_2(Pipe, Xs, Res, !IO)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
