% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module maildir.
:- interface.

:- import_module io.
:- import_module maybe.
:- import_module set.

:- import_module dir_cache.     % XXX cyclic
:- import_module imap.
:- import_module imap.types.
:- import_module lowio.
:- import_module path.

%-----------------------------------------------------------------------------%

:- type uniquename
    --->    uniquename(string).

:- type new_or_cur
    --->    new
    ;       cur.

:- type info_suffix
    --->    info_suffix(fn_flags, string). % after ":2,"; may be empty

:- type fn_flags.

:- pred generate_unique_name(dirname::in,
    maybe_error({uniquename, path, filedes})::out, io::di, io::uo) is det.

:- pred make_message_basename(uniquename::in, maybe(info_suffix)::in,
    basename::out) is det.

:- pred change_to_cur(dirname::in, dirname::out) is semidet.

:- type find_file_result
    --->    found(dirname, basename, maybe(info_suffix))
    ;       found_but_unexpected(path)
    ;       not_found.

:- pred find_file(dir_cache::in, uniquename::in, find_file_result::out) is det.

:- pred parse_basename(basename::in, uniquename::out) is semidet.

:- pred parse_basename(basename::in, uniquename::out, set(flag)::out)
    is semidet.

:- func flags_to_info_suffix(set(flag)) = info_suffix.

:- pred add_remove_standard_flags(set(flag)::in, set(flag)::in,
    info_suffix::in, info_suffix::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module gettimeofday.
:- import_module sys_util.

:- type fn_flags == string. % sorted, no duplicates

:- mutable(last_timeofday, {int, int}, {-1, -1}, ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_unique_name(DirName, Res, !IO) :-
    % We follow the Dovecot file name generation algorithm
    % (also notmuch insert).
    get_pid(Pid, !IO),
    safe_gethostname(HostName, !IO),
    generate_unique_name_2(DirName, Pid, HostName, Res, !IO).

:- pred generate_unique_name_2(dirname::in, int::in, string::in,
    maybe_error({uniquename, path, filedes})::out, io::di, io::uo) is det.

generate_unique_name_2(DirName, Pid, HostName, Res, !IO) :-
    DirName = dirname(DirNameString),
    safer_gettimeofday(Sec, Usec, !IO),
    string.format("%d.M%dP%d.%s", [i(Sec), i(Usec), i(Pid), s(HostName)],
        UniqueName),
    Path = DirNameString / UniqueName,
    open_output_excl(Path, ResOpen, !IO),
    (
        ResOpen = ok(Fd),
        Res = ok({uniquename(UniqueName), path(Path), Fd})
    ;
        ResOpen = already_exists,
        generate_unique_name_2(DirName, Pid, HostName, Res, !IO)
    ;
        ResOpen = error(Error),
        Res = error("error opening " ++ Path ++ ": " ++ Error)
    ).

:- pred safe_gethostname(string::out, io::di, io::uo) is det.

safe_gethostname(HostName, !IO) :-
    get_hostname(HostName0, !IO),
    string.replace_all(HostName0, "/", "_", HostName1),
    string.replace_all(HostName1, ":", "_", HostName).

:- pred safer_gettimeofday(int::out, int::out, io::di, io::uo) is det.

safer_gettimeofday(Sec, Usec, !IO) :-
    % Make sure not to return the same time of day as before. It can happen
    % when the IMAP server is on the same machine and perhaps the granularity
    % of gettimeofday is worse than we wish.
    get_last_timeofday({Sec0, Usec0}, !IO),
    gettimeofday(Sec1, Usec1, !IO),
    (
        Usec0 = Usec1,
        Sec0 = Sec1
    ->
        % Might be worth sleeping for a microsecond or so here.
        safer_gettimeofday(Sec, Usec, !IO)
    ;
        Sec = Sec1,
        Usec = Usec1,
        set_last_timeofday({Sec, Usec}, !IO)
    ).

%-----------------------------------------------------------------------------%

make_message_basename(uniquename(Unique), MaybeInfoSuffix, basename(BaseName))
        :-
    (
        MaybeInfoSuffix = yes(info_suffix(Flags, Rest)),
        BaseName = string.append_list([Unique, ":2,", Flags, Rest])
    ;
        MaybeInfoSuffix = no,
        BaseName = Unique
    ).

change_to_cur(dirname(Dir0), dirname(Dir)) :-
    dir.split_name(Dir0, ButLast, Last),
    ( Last = "cur"
    ; Last = "new"
    ; Last = "tmp"
    ),
    Dir = ButLast / "cur".

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

parse_basename(basename(BaseName), uniquename(Unique)) :-
    ( string.sub_string_search(BaseName, ":", Colon) ->
        string.unsafe_between(BaseName, 0, Colon, Unique),
        parse_info_suffix_inner(BaseName, Colon, _FlagsStart, _FlagsEnd)
    ;
        Unique = BaseName
    ).

parse_basename(basename(BaseName), uniquename(Unique), Flags) :-
    ( string.sub_string_search(BaseName, ":", Colon) ->
        string.unsafe_between(BaseName, 0, Colon, Unique),
        parse_info_suffix(BaseName, Colon, InfoSuffix),
        InfoSuffix = info_suffix(FnFlags, _Rest),
        fn_flags_to_standard_flags(FnFlags, Flags)
    ;
        Unique = BaseName,
        Flags = set.init
    ).

%-----------------------------------------------------------------------------%

flags_to_info_suffix(Flags) = InfoSuffix :-
    standard_flags_to_fn_flags(Flags, FnFlags),
    InfoSuffix = info_suffix(FnFlags, "").

add_remove_standard_flags(AddFlags, RemoveFlags, InfoSuffix0, InfoSuffix) :-
    set.filter_map(flag_to_standard_char, RemoveFlags, RemoveChars),
    set.filter_map(flag_to_standard_char, AddFlags, AddChars),

    InfoSuffix0 = info_suffix(String0, Rest),
    Set0 = set.from_sorted_list(to_char_list(String0)),
    Set = difference(Set0, RemoveChars) `union` AddChars,
    String = from_char_list(to_sorted_list(Set)),
    InfoSuffix = info_suffix(String, Rest).

%-----------------------------------------------------------------------------%

:- pred fn_flags_to_standard_flags(fn_flags::in, set(flag)::out) is det.

fn_flags_to_standard_flags(String, Set) :-
    % Optimise common case.
    ( String = "S" ->
        Set = set.make_singleton_set(system(seen))
    ;
        string.to_char_list(String, Chars),
        list.filter_map(char_to_standard_flag, Chars, Flags),
        set.list_to_set(Flags, Set)
    ).

:- pred standard_flags_to_fn_flags(set(flag)::in, fn_flags::out) is det.

standard_flags_to_fn_flags(Set, String) :-
    set.to_sorted_list(Set, Flags),
    list.filter_map(flag_to_standard_char, Flags, UnsortedChars),
    list.sort(UnsortedChars, Chars),
    string.from_char_list(Chars, String).

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

parse_info_suffix(String, Pos0, InfoSuffix) :-
    parse_info_suffix_inner(String, Pos0, FlagsStart, FlagsEnd),
    string.unsafe_between(String, FlagsStart, FlagsEnd, Flags),
    string.unsafe_between(String, FlagsEnd, length(String), Rest),
    InfoSuffix = info_suffix(Flags, Rest).

:- pred parse_info_suffix_inner(string::in, int::in, int::out, int::out)
    is semidet.

parse_info_suffix_inner(String, !.Pos, FlagsStart, FlagsEnd) :-
    string.unsafe_index_next(String, !Pos, ':'),
    string.unsafe_index_next(String, !Pos, '2'),
    string.unsafe_index_next(String, !Pos, ','),
    FlagsStart = !.Pos,
    FakePrecChar = char.det_from_int(to_int('A') - 1),
    grab_flag_chars(String, FakePrecChar, FlagsStart, FlagsEnd).

:- pred grab_flag_chars(string::in, char::in, int::in, int::out) is semidet.

grab_flag_chars(String, PrecChar, !Pos) :-
    (
        string.unsafe_index_next(String, !Pos, Char),
        % Dovecot defines an extension where non-standard fields may follow a
        % comma (but doesn't use it?)
        Char \= (',')
    ->
        % ASCII seems to be implied.
        char.to_int(Char, I),
        0 < I, I =< 0x7f,
        % Flags must be in ASCII order.
        Char @> PrecChar,
        grab_flag_chars(String, Char, !Pos)
    ;
        % End of flags.
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
