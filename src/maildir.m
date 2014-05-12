%-----------------------------------------------------------------------------%

:- module maildir.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

:- import_module dir_cache.
:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

    % Path to a Maildir directory, also known as a mail folder.
    % It should have tmp/new/cur subdirectories.
    %
:- type local_mailbox_path
    --->    local_mailbox_path(string). % XXX canonicalise

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
    maybe(info_suffix)::in, string::out) is det.

:- pred update_file_list(local_mailbox_path::in, dir_cache::in,
    maybe_error(dir_cache)::out, io::di, io::uo) is det.

    % Temporary interface.
    %
:- pred list_files(dir_cache::in, local_mailbox_path::in,
    list(string)::out, list(string)::out) is det.

:- type find_file_result
    --->    found(
                path         :: string, % entire path to file
                info_suf     :: maybe(info_suffix)
            )
    ;       found_but_unexpected(string)
    ;       not_found.

:- inst found
    --->    found(ground, ground).

    % Search for a file in the given mailbox (including sub-mailboxes) with the
    % given uniquename.
    %
:- pred find_file(dir_cache::in, local_mailbox_path::in, uniquename::in,
    find_file_result::out) is det.

:- pred parse_basename(string::in, uniquename::out, set(flag)::out) is semidet.

:- func flags_to_info_suffix(set(flag)) = info_suffix.

:- pred update_standard_flags(set(flag)::in, info_suffix::in, info_suffix::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module maybe.
:- import_module require.
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

make_path(local_mailbox_path(DirName), NewOrCur, uniquename(Unique),
        MaybeInfoSuffix, Path) :-
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

update_file_list(local_mailbox_path(DirName), DirCache0, Res, !IO) :-
    update_file_list(DirName / "new", DirCache0, Res1, !IO),
    (
        Res1 = ok(DirCache1),
        update_file_list(DirName / "cur", DirCache1, Res, !IO)
    ;
        Res1 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

list_files(DirCache, local_mailbox_path(DirName), NewFiles, CurFiles) :-
    get_file_list(DirCache, DirName / "new", NewFiles),
    get_file_list(DirCache, DirName / "cur", CurFiles).

%-----------------------------------------------------------------------------%

find_file(DirCache, local_mailbox_path(DirName), UniqueName, Res) :-
    find_file_2(DirCache, DirName, "new", UniqueName, Res0),
    (
        Res0 = found(_, _),
        Res = Res0
    ;
        Res0 = found_but_unexpected(_),
        Res = Res0
    ;
        Res0 = not_found,
        find_file_2(DirCache, DirName, "cur", UniqueName, Res)
    ).

:- pred find_file_2(dir_cache::in, string::in, string::in, uniquename::in,
    find_file_result::out) is det.

find_file_2(DirCache, DirName, SubDir, UniqueName, Res) :-
    UniqueName = uniquename(Unique),
    search_files_with_prefix(DirCache, DirName / SubDir, Unique, Matching),
    (
        Matching = [],
        Res = not_found
    ;
        Matching = [BaseName | _],
        Path = DirName / SubDir / BaseName,
        string.length(Unique, Pos0),
        string.length(BaseName, EndPos),
        ( Pos0 = EndPos ->
            Res = found(Path, no)
        ; parse_info_suffix(BaseName, Pos0, InfoSuffix) ->
            Res = found(Path, yes(InfoSuffix))
        ;
            Res = found_but_unexpected(Path)
        )
    ).

%-----------------------------------------------------------------------------%

parse_basename(BaseName, uniquename(Unique), Flags) :-
    ( string.sub_string_search(BaseName, ":", Colon) ->
        string.unsafe_between(BaseName, 0, Colon, Unique),
        parse_info_suffix(BaseName, Colon, InfoSuffix),
        InfoSuffix = info_suffix(Chars, _Rest),
        set.filter_map(standard_flag_char, Chars, Flags)
    ;
        Unique = BaseName,
        Flags = set.init
    ).

%-----------------------------------------------------------------------------%

flags_to_info_suffix(Flags) = InfoSuffix :-
    list.foldl(update_standard_flag(Flags), standard_flag_chars, init, Chars),
    InfoSuffix = info_suffix(Chars, "").

update_standard_flags(HaveFlags, InfoSuffix0, InfoSuffix) :-
    InfoSuffix0 = info_suffix(Chars0, Rest),
    list.foldl(update_standard_flag(HaveFlags), standard_flag_chars, Chars0, Chars),
    InfoSuffix = info_suffix(Chars, Rest).

:- pred update_standard_flag(set(flag)::in, char::in,
    set(char)::in, set(char)::out) is det.

update_standard_flag(HaveFlags, FlagChar, !Set) :-
    det_standard_flag_char(FlagChar, Flag),
    ( set.contains(HaveFlags, Flag) ->
        set.insert(FlagChar, !Set)
    ;
        set.delete(FlagChar, !Set)
    ).

:- func standard_flag_chars = list(char).

standard_flag_chars = ['R', 'F', 'T', 'S', 'D'].

:- pred standard_flag_char(char::in, flag::out) is semidet.

standard_flag_char('R', system(answered)).
standard_flag_char('F', system(flagged)).
standard_flag_char('T', system(deleted)).
standard_flag_char('S', system(seen)).
standard_flag_char('D', system(draft)).

:- pred det_standard_flag_char(char::in, flag::out) is det.

det_standard_flag_char(Char, Flag) :-
    ( standard_flag_char(Char, FlagPrime) ->
        Flag = FlagPrime
    ;
        unexpected($module, $pred, "standard_flag_char failed")
    ).

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
