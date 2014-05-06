%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.
:- import_module maybe.

:- pred generate_unique_name(string::in, maybe_error(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module gettimeofday.
:- import_module sys_util.

%-----------------------------------------------------------------------------%

generate_unique_name(DirName, Res, !IO) :-
    % We follow the Dovecot file name generation algorithm
    % (also notmuch insert).
    get_pid(Pid, !IO),
    safe_gethostname(HostName, !IO),
    generate_unique_name_2(DirName, Pid, HostName, Res, !IO).

:- pred generate_unique_name_2(string::in, int::in, string::in,
    maybe_error(string)::out, io::di, io::uo) is det.

generate_unique_name_2(DirName, Pid, HostName, Res, !IO) :-
    gettimeofday(Sec, Usec, !IO),
    string.format("%d.M%dP%d.%s", [i(Sec), i(Usec), i(Pid), s(HostName)],
        UniqueName),
    Path = DirName / UniqueName,
    open_excl(Path, Fd, AlreadyExists, !IO),
    ( Fd >= 0 ->
        close(Fd, !IO),
        Res = ok(UniqueName)
    ; AlreadyExists = yes ->
        generate_unique_name_2(DirName, Pid, HostName, Res, !IO)
    ;
        Res = error("error opening " ++ Path)
    ).

:- pred safe_gethostname(string::out, io::di, io::uo) is det.

safe_gethostname(HostName, !IO) :-
    get_hostname(HostName0, !IO),
    string.replace_all(HostName0, "/", "_", HostName1),
    string.replace_all(HostName1, ":", "_", HostName).

:- pred open_excl(string::in, int::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_excl(Path::in, Fd::out, AlreadyExists::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
	Fd = open(Path, O_WRONLY | O_CREAT | O_TRUNC | O_EXCL, 0600);
    if ((Fd == -1) && (errno == EEXIST)) {
        AlreadyExists = MR_YES;
    } else {
        AlreadyExists = MR_NO;
    }
").

:- pred close(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    close(Fd);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
