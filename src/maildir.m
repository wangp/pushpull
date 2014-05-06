%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type uniquename
    --->    uniquename(string).

:- pred generate_unique_name(string::in, maybe_error(uniquename)::out,
    io::di, io::uo) is det.

:- func info_suffix(list(flag)) = string.

:- func empty_info_suffix = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module string.

:- import_module gettimeofday.
:- import_module sys_util.

:- inst standard_maildir_flag
    --->    'R' ; 'F' ; 'T' ; 'S' ; 'D'.

%-----------------------------------------------------------------------------%

generate_unique_name(DirName, Res, !IO) :-
    % We follow the Dovecot file name generation algorithm
    % (also notmuch insert).
    get_pid(Pid, !IO),
    safe_gethostname(HostName, !IO),
    generate_unique_name_2(DirName, Pid, HostName, Res, !IO).

:- pred generate_unique_name_2(string::in, int::in, string::in,
    maybe_error(uniquename)::out, io::di, io::uo) is det.

generate_unique_name_2(DirName, Pid, HostName, Res, !IO) :-
    gettimeofday(Sec, Usec, !IO),
    string.format("%d.M%dP%d.%s", [i(Sec), i(Usec), i(Pid), s(HostName)],
        UniqueName),
    Path = DirName / UniqueName,
    open_excl(Path, Fd, AlreadyExists, !IO),
    ( Fd >= 0 ->
        close(Fd, !IO),
        Res = ok(uniquename(UniqueName))
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

empty_info_suffix = ":2,".

info_suffix(Flags) = Suffix :-
    list.filter_map(flag_char, Flags, Chars0),
    % Chars must be in ASCII order.
    list.sort(Chars0, Chars),
    Suffix = empty_info_suffix ++ string.from_char_list(Chars).

:- pred flag_char(flag::in, char::out) is semidet.

flag_char(system(Flag), Char) :-
    require_complete_switch [Flag]
    (
        Flag = answered,
        Char = 'R'
    ;
        Flag = flagged,
        Char = 'F'
    ;
        Flag = deleted,
        Char = 'T'
    ;
        Flag = seen,
        Char = 'S'
    ;
        Flag = draft,
        Char = 'D'
    ;
        Flag = extension(_),
        fail
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
