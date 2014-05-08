%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type local_mailbox_path
    --->    local_mailbox_path(string). % XXX canonicalise

:- type uniquename
    --->    uniquename(string).

:- type new_or_cur
    --->    new
    ;       cur.

:- type info_suffix
    --->    info_suffix(string). % everything after ":2,"; may be empty

:- pred generate_unique_name(string::in, maybe_error(uniquename)::out,
    io::di, io::uo) is det.

:- func flag_set_to_info_suffix(set(flag)) = info_suffix.

:- func flags_to_info_suffix(list(flag)) = info_suffix.

:- pred make_tmp_path(string::in, uniquename::in, string::out) is det.

:- pred make_path(string::in, new_or_cur::in, uniquename::in,
    maybe(info_suffix)::in, string::out) is det.

:- type find_file_result
    --->    found(
                dirname_sans_new_cur:: string, % not including new/cur
                path                :: string, % entire path to file
                info_suf            :: maybe(info_suffix)
            )
    ;       found_but_unexpected(string)
    ;       not_found.

:- inst found
    --->    found(ground, ground, ground).

:- pred find_file(local_mailbox_path::in, uniquename::in,
    maybe_error(find_file_result)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module maybe.
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

flag_set_to_info_suffix(Flags) =
    flags_to_info_suffix(set.to_sorted_list(Flags)).

flags_to_info_suffix(Flags) = info_suffix(Suffix) :-
    list.filter_map(flag_char, Flags, Chars0),
    % Chars must be in ASCII order.
    list.sort(Chars0, Chars),
    Suffix = string.from_char_list(Chars).

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

make_tmp_path(DirName, uniquename(Unique), Path) :-
    Path = DirName / "tmp" / Unique.

make_path(DirName, NewOrCur, uniquename(Unique), MaybeInfoSuffix, Path) :-
    (
        MaybeInfoSuffix = yes(info_suffix(Suffix)),
        BaseName = Unique ++ ":2," ++ Suffix
    ;
        MaybeInfoSuffix = no,
        BaseName = Unique
    ),
    Path = DirName / new_or_cur(NewOrCur) / BaseName.

:- func new_or_cur(new_or_cur) = string.

new_or_cur(new) = "new".
new_or_cur(cur) = "cur".

%-----------------------------------------------------------------------------%

find_file(local_mailbox_path(DirName), UniqueName, Res, !IO) :-
    FollowSymLinks = yes,
    % XXX should only find files in "new" and "cur"
    dir.recursive_foldl2(find_file_2(UniqueName), DirName, FollowSymLinks,
        not_found, Res0, !IO),
    (
        Res0 = ok(Found),
        Res = ok(Found)
    ;
        Res0 = error(_, Error),
        Res = error(io.error_message(Error))
    ).

:- pred find_file_2(uniquename::in, string::in, string::in, io.file_type::in,
    bool::out, find_file_result::in, find_file_result::out, io::di, io::uo)
    is det.

find_file_2(UniqueName, DirName, BaseName, FileType, Continue, !Found, !IO) :-
    (
        maybe_regular_file(FileType),
        match_uniquename(UniqueName, BaseName, AfterUnique)
    ->
        Continue = no,
        Path = DirName / BaseName,
        (
            dir.split_name(DirName, DirNameSansNewOrCur, NewOrCur),
            is_new_or_cur(NewOrCur)
        ->
            ( plausible_info_suffix(AfterUnique, MaybeInfoSuffix) ->
                !:Found = found(DirNameSansNewOrCur, Path, MaybeInfoSuffix)
            ;
                !:Found = found_but_unexpected(Path)
            )
        ;
            !:Found = found_but_unexpected(Path)
        )
    ;
        Continue = yes
    ).

:- pred maybe_regular_file(io.file_type::in) is semidet.

maybe_regular_file(regular_file).
maybe_regular_file(unknown).

:- pred match_uniquename(uniquename::in, string::in, string::out) is semidet.

match_uniquename(uniquename(Unique), BaseName, AfterUnique) :-
    string.remove_prefix(Unique, BaseName, AfterUnique).

:- pred plausible_info_suffix(string::in, maybe(info_suffix)::out) is semidet.

plausible_info_suffix(AfterUnique, MaybeInfoSuffix) :-
    ( AfterUnique = "" ->
        MaybeInfoSuffix = no
    ;
        string.remove_prefix(":2,", AfterUnique, Tail),
        MaybeInfoSuffix = yes(info_suffix(Tail))
    ).

:- pred is_new_or_cur(string::in) is semidet.

is_new_or_cur("new").
is_new_or_cur("cur").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
