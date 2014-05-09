%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module char.
:- import_module io.
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
    --->    info_suffix(set(char), string). % after ":2,"; may be empty

:- pred generate_unique_name(string::in, maybe_error(uniquename)::out,
    io::di, io::uo) is det.

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

:- func flags_to_info_suffix(set(flag)) = info_suffix.

:- pred update_standard_flags(set(flag)::in, info_suffix::in, info_suffix::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
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

make_tmp_path(DirName, uniquename(Unique), Path) :-
    Path = DirName / "tmp" / Unique.

make_path(DirName, NewOrCur, uniquename(Unique), MaybeInfoSuffix, Path) :-
    (
        MaybeInfoSuffix = yes(info_suffix(FlagChars, Rest)),
        Flags = string.from_char_list(to_sorted_list(FlagChars)),
        BaseName = string.append_list([Unique, ":2,", Flags, Rest])
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
            ( AfterUnique = "" ->
                !:Found = found(DirNameSansNewOrCur, Path, no)
            ; parse_info_suffix(AfterUnique, InfoSuffix) ->
                !:Found = found(DirNameSansNewOrCur, Path, yes(InfoSuffix))
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

:- pred is_new_or_cur(string::in) is semidet.

is_new_or_cur("new").
is_new_or_cur("cur").

%-----------------------------------------------------------------------------%

flags_to_info_suffix(Flags) = InfoSuffix :-
    list.foldl(update_standard_flag(Flags), standard_flags, init, Chars),
    InfoSuffix = info_suffix(Chars, "").

update_standard_flags(HaveFlags, InfoSuffix0, InfoSuffix) :-
    InfoSuffix0 = info_suffix(Chars0, Rest),
    list.foldl(update_standard_flag(HaveFlags), standard_flags, Chars0, Chars),
    InfoSuffix = info_suffix(Chars, Rest).

:- pred update_standard_flag(set(flag)::in, pair(flag, char)::in,
    set(char)::in, set(char)::out) is det.

update_standard_flag(HaveFlags, Flag - FlagChar, !Set) :-
    ( set.contains(HaveFlags, Flag) ->
        set.insert(FlagChar, !Set)
    ;
        set.delete(FlagChar, !Set)
    ).

:- func standard_flags = list(pair(flag, char)).

standard_flags = [
    system(answered) - 'R',
    system(flagged) - 'F',
    system(deleted) - 'T',
    system(seen) - 'S',
    system(draft) - 'D'
].

:- pred parse_info_suffix(string::in, info_suffix::out) is semidet.

parse_info_suffix(String, InfoSuffix) :-
    string.prefix(String, ":2,"),
    grab_flag_chars(String, 3, StopPos, [], RevFlagChars),
    string.unsafe_between(String, StopPos, length(String), Rest),
    Flags = set.from_list(RevFlagChars),
    InfoSuffix = info_suffix(Flags, Rest).

:- pred grab_flag_chars(string::in, int::in, int::out,
    list(char)::in, list(char)::out) is semidet.

grab_flag_chars(String, !Pos, !RevFlagChars) :-
    (
        string.unsafe_index_next(String, !Pos, Char),
        % Dovecot defines an extension where non-standard fields may follow a
        % comma (but doesn't use it?)
        Char \= (',')
    ->
        % ASCII seems to be implied.
        char.to_int(Char, I),
        0 < I, I =< 0x7f,
        % Flags are supposed to be in ASCII order.
        later(Char, !.RevFlagChars),
        cons(Char, !RevFlagChars),
        grab_flag_chars(String, !Pos, !RevFlagChars)
    ;
        % End of flags.
        true
    ).

:- pred later(char::in, list(char)::in) is semidet.

later(_, []).
later(C, [X | _]) :-
    to_int(C) > to_int(X).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
