%-----------------------------------------------------------------------------%

:- module subprocess.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type subprocess.

:- pred spawn(string::in, list(string)::in, maybe_error(subprocess)::out,
    io::di, io::uo) is det.

:- type wait_pid_blocking
    --->    blocking
    ;       nonblocking.

:- type wait_pid_result
    --->    no_hang
    ;       child_exit(int)
    ;       child_signalled(int)
    ;       child_abnormal_exit
    ;       error(io.error).

:- pred wait_pid(subprocess::in, wait_pid_blocking::in, wait_pid_result::out,
    io::di, io::uo) is det.

:- pred close_pipes(subprocess::in, io::di, io::uo) is det.

:- pred read_byte(subprocess::in, io.result(int)::out, io::di, io::uo) is det.

:- pred read_bytes(subprocess::in, int::in, io.res(list(int))::out,
    io::di, io::uo) is det.

:- pred write_byte(subprocess::in, int::in, maybe_error::out, io::di, io::uo)
    is det.

:- pred write_string(subprocess::in, string::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred flush_output(subprocess::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.

:- type subprocess
    --->    subprocess(
                pid :: int,
                rd :: int,
                wr :: int
            ).

:- pragma foreign_decl("C", local, "
static int close_eintr(int fd)
{
    int rc;
    do {
        rc = close(fd);
    } while (rc == -1 && errno == EINTR);
    return rc;
}
").

%-----------------------------------------------------------------------------%

spawn(Prog, Args, Res, !IO) :-
    list.length(Args, NumArgs),
    posix_spawn_2(Prog, Args, NumArgs, Pid, Rd, Wr, !IO),
    ( Pid >= 0 ->
        Res = ok(subprocess(Pid, Rd, Wr))
    ;
        Res = error("posix_spawn failed")
    ).

:- pred posix_spawn_2(string::in, list(string)::in, int::in,
    int::out, int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    posix_spawn_2(Prog::in, Args::in, NumArgs::in, Pid::out, Rd::out, Wr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    pid_t pid;
    char *argv[NumArgs + 1];    /* C99 stack allocation */
    int childin[2] = {-1, -1};
    int childout[2] = {-1, -1};
    int i;
    int rc;

    argv[0] = Prog;
    for (i = 1; i <= NumArgs; i++) {
        argv[i] = MR_make_string(MR_ALLOC_ID, ""%s"",
            (MR_ConstString) MR_list_head(Args));
        Args = MR_list_tail(Args);
    }
    argv[i] = NULL;

    rc = pipe(childin);
    if (rc == 0) {
        rc = pipe(childout);
        if (rc == 0) {
            rc = do_posix_spawn(&pid, Prog, argv, childin, childout);
        }
    }

    if (rc == 0) {
        Pid = pid;
        Rd = childout[0];
        Wr = childin[1];
        /* Close the write end of the childout pipe in the parent. */
        if (childout[1] != -1) {
            close_eintr(childout[1]);
        }
        /* Close the read end of the childin pipe in the parent. */
        if (childin[0] != -1) {
            close_eintr(childin[0]);
        }
    } else {
        Pid = -1;
        Rd = -1;
        Wr = -1;
        if (childin[0] != -1) {
            close_eintr(childin[0]);
        }
        if (childin[1] != -1) {
            close_eintr(childin[1]);
        }
        if (childout[0] != -1) {
            close_eintr(childout[0]);
        }
        if (childout[1] != -1) {
            close_eintr(childout[1]);
        }
    }
").

:- pragma foreign_decl("C", local, "
#if defined(__APPLE__) && defined(__MACH__)
    /*
    ** On Darwin, shared libraries and bundles don't have direct access to
    ** environ.
    */
    #include <crt_externs.h>
    #define ENVIRON (*_NSGetEnviron())
#else
    /* POSIX does not require environ to be declared. */
    extern char **environ;
    #define ENVIRON (environ)
#endif

static int
do_posix_spawn(pid_t *pid_ptr, const char *Prog, char *argv[],
    int childin[2], int childout[2])
{
    posix_spawn_file_actions_t file_actions;
    posix_spawnattr_t attr;
    sigset_t sigmask;
    int rc;

    posix_spawn_file_actions_init(&file_actions);

    /*
    ** Close the write end of the childin pipe in the child, then redirect
    ** stdin to the read end of the childin pipe.
    */
    if (childin[1] != -1) {
        posix_spawn_file_actions_addclose(&file_actions, childin[1]);
        posix_spawn_file_actions_adddup2(&file_actions, childin[0],
            STDIN_FILENO);
    }

    /*
    ** Close the read end of the childout pipe in the child, then redirect
    ** stdout to the write end of the childout pipe.
    */
    if (childout[0] != -1) {
        posix_spawn_file_actions_addclose(&file_actions, childout[0]);
        posix_spawn_file_actions_adddup2(&file_actions, childout[1],
            STDOUT_FILENO);
    }

    /*
    ** Block SIGWINCH in the child so that resizing the terminal
    ** window does not kill the child.
    */
    sigemptyset(&sigmask);
    sigaddset(&sigmask, SIGWINCH);
    posix_spawnattr_init(&attr);
    posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSIGMASK);
    posix_spawnattr_setsigmask(&attr, &sigmask);

    rc = posix_spawnp(pid_ptr, Prog, &file_actions, &attr, argv, ENVIRON);

    posix_spawnattr_destroy(&attr);
    posix_spawn_file_actions_destroy(&file_actions);

    return rc;
}
").

%-----------------------------------------------------------------------------%

wait_pid(subprocess(Pid, _Rd, _Wr), Blocking, Res, !IO) :-
    (
        Blocking = blocking,
        BlockingBool = yes
    ;
        Blocking = nonblocking,
        BlockingBool = no
    ),
    wait_pid_2(Pid, BlockingBool, RC, Exited, ExitStatus, Signal, !IO),
    ( RC = -1 ->
        Res = error(io.make_io_error("wait_pid failed"))
    ; RC = 0 ->
        % Child process not done.
        Res = no_hang
    ;
        ( Exited = yes ->
            Res = child_exit(ExitStatus)
        ; Signal > 0 ->
            Res = child_signalled(Signal)
        ;
            Res = child_abnormal_exit
        )
    ).

:- pred wait_pid_2(int::in, bool::in, int::out, bool::out, int::out,
    int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    wait_pid_2(Pid::in, Blocking::in, RC::out, Exited::out,
        ExitStatus::out, Signal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int status;
    int options;

    if (Blocking) {
        options = 0;
    } else {
        options = WNOHANG;
    }
    do {
        RC = waitpid(Pid, &status, options);
    } while (RC == -1 && errno == EINTR);
    if (RC == -1) {
        Exited = MR_NO;
        ExitStatus = -1;
        Signal = -1;
    } else if (WIFEXITED(status)) {
        Exited = MR_YES;
        ExitStatus = WEXITSTATUS(status);
        Signal = -1;
    } else if (WIFSIGNALED(status)) {
        Exited = MR_NO;
        Signal = WTERMSIG(status);
    } else {
        Exited = MR_NO;
        ExitStatus = -1;
        Signal = -1;
    }
").

%-----------------------------------------------------------------------------%

close_pipes(subprocess(_Pid, Rd, Wr), !IO) :-
    close_pipe_2(Rd, !IO),
    close_pipe_2(Wr, !IO).

:- pred close_pipe_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_pipe_2(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    close_eintr(Fd);
").

%-----------------------------------------------------------------------------%

read_byte(subprocess(_Pid, Rd, _Wr), Res, !IO) :-
    read_byte_2(Rd, RC, !IO),
    ( RC = -1 ->
        Res = eof
    ; RC = -2 ->
        Res = error(io.make_io_error("read failed"))
    ;
        Res = ok(RC)
    ).

:- pred read_byte_2(int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_byte_2(Fd::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    unsigned char buf[1];
    ssize_t n;

    n = read(Fd, buf, sizeof(buf));
    if (n == 1) {
        RC = buf[0];
    } else if (n == 0) {
        RC = -1;
    } else {
        RC = -2;
    }
").

%-----------------------------------------------------------------------------%

read_bytes(subprocess(_Pid, Rd, _Wr), NumOctets, Res, !IO) :-
    read_bytes_loop(Rd, NumOctets, Res0, [], RevBytes, !IO),
    (
        Res0 = ok,
        Res = ok(reverse(RevBytes))
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred read_bytes_loop(int::in, int::in, io.res::out,
    list(int)::in, list(int)::out, io::di, io::uo) is det.

read_bytes_loop(Rd, NumOctets, Res, !Acc, !IO) :-
    ( NumOctets =< 0 ->
        Res = ok
    ;
        read_byte_2(Rd, RC, !IO),
        ( RC = -1 ->
            Res = error(io.make_io_error("unexpected eof"))
        ; RC = -2 ->
            Res = error(io.make_io_error("read failed"))
        ;
            cons(RC, !Acc),
            read_bytes_loop(Rd, NumOctets - 1, Res, !Acc, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

write_byte(subprocess(_Pid, _Rd, Wr), Byte, Res, !IO) :-
    write_byte_2(Wr, Byte, RC, !IO),
    ( RC = -1 ->
        Res = error("write failed")
    ;
        Res = ok
    ).

:- pred write_byte_2(int::in, int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_byte_2(Fd::in, Byte::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    unsigned char buf[1] = {Byte};
    ssize_t n;

    n = write(Fd, buf, sizeof(buf));
    if (n == 1) {
        RC = 1;
    } else {
        RC = -1;
    }
").

%-----------------------------------------------------------------------------%

write_string(subprocess(_Pid, _Rd, Wr), String, Res, !IO) :-
    write_string_2(Wr, String, RC, !IO),
    ( RC = -1 ->
        Res = error("write failed")
    ;
        Res = ok
    ).

:- pred write_string_2(int::in, string::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_string_2(Fd::in, String::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ssize_t len = strlen(String);
    ssize_t n;

    n = write(Fd, String, len);
    if (n == len) {
        RC = 1;
    } else {
        RC = -1;
    }
").

%-----------------------------------------------------------------------------%

flush_output(subprocess(_Pid, _Rd, _Wr), !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
