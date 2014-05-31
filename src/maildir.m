%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module char.
:- import_module io.
:- import_module maybe.
:- import_module set.

:- import_module dir_cache.
:- import_module imap.
:- import_module imap.types.
:- import_module prog_config.
:- import_module path.

%-----------------------------------------------------------------------------%

:- type uniquename
    --->    uniquename(string).

:- type new_or_cur
    --->    new
    ;       cur.

:- type info_suffix
    --->    info_suffix(set(char), string). % after ":2,"; may be empty

:- pred generate_unique_tmp_path(local_mailbox_path::in,
    maybe_error({uniquename, string})::out, io::di, io::uo) is det.

:- pred make_path(local_mailbox_path::in, new_or_cur::in, uniquename::in,
    maybe(info_suffix)::in, dirname::out, basename::out) is det.

:- type find_file_result
    --->    found(dirname, basename, maybe(info_suffix))
    ;       found_but_unexpected(path)
    ;       not_found.

:- pred find_file(dir_cache::in, uniquename::in, find_file_result::out) is det.

:- pred parse_basename(basename::in, uniquename::out, set(flag)::out)
    is semidet.

:- func flags_to_info_suffix(set(flag)) = info_suffix.

:- pred add_remove_standard_flags(set(flag)::in, set(flag)::in,
    info_suffix::in, info_suffix::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module gettimeofday.
:- import_module sys_util.

%-----------------------------------------------------------------------------%

generate_unique_tmp_path(local_mailbox_path(DirName), Res, !IO) :-
    % We follow the Dovecot file name generation algorithm
    % (also notmuch insert).
    get_pid(Pid, !IO),
    safe_gethostname(HostName, !IO),
    generate_unique_name_2(DirName / "tmp", Pid, HostName, Res, !IO).

:- pred generate_unique_name_2(string::in, int::in, string::in,
    maybe_error({uniquename, string})::out, io::di, io::uo) is det.

generate_unique_name_2(DirName, Pid, HostName, Res, !IO) :-
    gettimeofday(Sec, Usec, !IO),
    string.format("%d.M%dP%d.%s", [i(Sec), i(Usec), i(Pid), s(HostName)],
        UniqueName),
    Path = DirName / UniqueName,
    open_excl(Path, Fd, AlreadyExists, !IO),
    ( Fd >= 0 ->
        close(Fd, !IO),
        Res = ok({uniquename(UniqueName), Path})
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

make_path(local_mailbox_path(DirName0), NewOrCur, uniquename(Unique),
        MaybeInfoSuffix, dirname(DirName), basename(BaseName)) :-
    (
        MaybeInfoSuffix = yes(info_suffix(FlagChars, Rest)),
        Flags = string.from_char_list(to_sorted_list(FlagChars)),
        BaseName = string.append_list([Unique, ":2,", Flags, Rest])
    ;
        MaybeInfoSuffix = no,
        BaseName = Unique
    ),
    DirName = DirName0 / new_or_cur(NewOrCur).

:- func new_or_cur(new_or_cur) = string.

new_or_cur(new) = "new".
new_or_cur(cur) = "cur".

%-----------------------------------------------------------------------------%

find_file(DirCache, UniqueName, Res) :-
    UniqueName = uniquename(Unique),
    search_files_with_prefix(DirCache, Unique, Matching),
    (
        Matching = [],
        Res = not_found
    ;
        Matching = [file(BaseName, DirName) | _],
        BaseName = basename(BaseNameString),
        string.length(Unique, Pos0),
        string.length(BaseNameString, EndPos),
        ( Pos0 = EndPos ->
            Res = found(DirName, BaseName, no)
        ; parse_info_suffix(BaseNameString, Pos0, InfoSuffix) ->
            Res = found(DirName, BaseName, yes(InfoSuffix))
        ;
            Res = found_but_unexpected(DirName / BaseName)
        )
    ).

%-----------------------------------------------------------------------------%

parse_basename(basename(BaseName), uniquename(Unique), Flags) :-
    ( string.sub_string_search(BaseName, ":", Colon) ->
        string.unsafe_between(BaseName, 0, Colon, Unique),
        parse_info_suffix(BaseName, Colon, InfoSuffix),
        InfoSuffix = info_suffix(Chars, _Rest),
        set.filter_map(char_to_standard_flag, Chars, Flags)
    ;
        Unique = BaseName,
        Flags = set.init
    ).

%-----------------------------------------------------------------------------%

flags_to_info_suffix(Flags) = InfoSuffix :-
    set.filter_map(flag_to_standard_char, Flags, Chars),
    InfoSuffix = info_suffix(Chars, "").

add_remove_standard_flags(AddFlags, RemoveFlags, InfoSuffix0, InfoSuffix) :-
    InfoSuffix0 = info_suffix(Chars0, Rest),
    set.filter_map(flag_to_standard_char, RemoveFlags, RemoveChars),
    set.filter_map(flag_to_standard_char, AddFlags, AddChars),
    Chars = difference(Chars0, RemoveChars) `union` AddChars,
    InfoSuffix = info_suffix(Chars, Rest).

:- pred char_to_standard_flag(char::in, flag::out) is semidet.

char_to_standard_flag(Char, Flag) :-
    standard_flag_char(Char, Flag).

:- pred flag_to_standard_char(flag::in, char::out) is semidet.

flag_to_standard_char(Flag, Char) :-
    standard_flag_char(Char, Flag).

:- pred standard_flag_char(char, flag).
:- mode standard_flag_char(in, out) is semidet.
:- mode standard_flag_char(out, in) is semidet.

standard_flag_char('R', system(answered)).
standard_flag_char('F', system(flagged)).
standard_flag_char('T', system(deleted)).
standard_flag_char('S', system(seen)).
standard_flag_char('D', system(draft)).

%-----------------------------------------------------------------------------%

:- pred parse_info_suffix(string::in, int::in, info_suffix::out) is semidet.

parse_info_suffix(String, !.Pos, InfoSuffix) :-
    string.unsafe_index_next(String, !Pos, ':'),
    string.unsafe_index_next(String, !Pos, '2'),
    string.unsafe_index_next(String, !Pos, ','),
    grab_flag_chars(String, !Pos, [], RevFlagChars),
    string.unsafe_between(String, !.Pos, length(String), Rest),
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
