% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module call_command.
:- interface.

:- import_module io.
:- import_module list.

:- import_module process.

:- pred call_command_capture_stdout(string::in, list(string)::in,
    spawn_env::in, io.res(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module string.

call_command_capture_stdout(Command, Args, SpawnEnv, Res, !IO) :-
    posix_spawn_get_stdout(Command, Args, SpawnEnv, SpawnRes, !IO),
    (
        SpawnRes = ok({Pid, PipeRead}),
        drain_pipe(PipeRead, DrainRes, Buffers, !IO),
        close_pipe_read(PipeRead, !IO),
        do_wait(Pid, no, WaitRes, !IO),
        (
            WaitRes = ok,
            (
                DrainRes = ok,
                ( make_utf8_string(no, Buffers, String) ->
                    Res = ok(String)
                ;
                    Res = error(io.make_io_error("not UTF-8 text"))
                )
            ;
                DrainRes = error(Error),
                Res = error(Error)
            )
        ;
            WaitRes = error(Error),
            Res = error(Error)
        )
    ;
        SpawnRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred do_wait(pid::in, maybe(int)::in, io.res::out, io::di, io::uo) is det.

do_wait(Pid, ExpectSignal, Res, !IO) :-
    wait_pid(Pid, blocking, WaitRes, !IO),
    (
        WaitRes = no_hang,
        % Should not occur.
        Res = error(io.make_io_error("process not finished"))
    ;
        WaitRes = child_exit(ExitStatus),
        ( ExitStatus = 0 ->
            Res = ok
        ;
            Msg = "process returned with exit code " ++
                string.from_int(ExitStatus),
            Res = error(io.make_io_error(Msg))
        )
    ;
        WaitRes = child_signalled(Signal),
        ( ExpectSignal = yes(Signal) ->
            Res = ok
        ;
            Msg = "process received signal " ++ string.from_int(Signal),
            Res = error(io.make_io_error(Msg))
        )
    ;
        WaitRes = child_abnormal_exit,
        Msg = "process exited abnormally",
        Res = error(io.make_io_error(Msg))
    ;
        WaitRes = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
