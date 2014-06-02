%-----------------------------------------------------------------------------%

:- module dir_cache.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module inotify.
:- import_module path.

%-----------------------------------------------------------------------------%

    % Cache the file names in a single Maildir mailbox.
    %
:- type dir_cache.

:- type index.

:- type file
    --->    file(basename, dirname).

:- func init(dirname) = dir_cache.

:- pred update_mailbox_file_list(maybe_error::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

:- pred update_for_rename(dirname::in, basename::in, dirname::in, basename::in,
    dir_cache::in, dir_cache::out) is det.

:- pred search_files_with_prefix(dir_cache::in, string::in, list(file)::out)
    is det.

:- pred begin(dir_cache::in, index::out) is det.

:- pred get(dir_cache::in, index::in, index::out, file::out) is semidet.

:- pred update_watches(inotify(S)::in, dir_cache::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module time.
:- import_module version_array.

:- type dir_cache
    --->    dir_cache(
                toplevel    :: dirname,
                mtimes      :: mtimes,
                files       :: version_array(file) % sorted
            ).

:- type mtimes == map(dirname, time_t).

:- type index == int.

%-----------------------------------------------------------------------------%

init(Top) = dir_cache(Top, map.init, version_array.empty).

update_mailbox_file_list(Res, DirCache0, DirCache, !IO) :-
    DirCache0 = dir_cache(TopDirName, Mtimes0, Array0),

    Queue0 = set.from_sorted_list(map.sorted_keys(Mtimes0)),
    set.insert(TopDirName, Queue0, Queue1),

    % Might want to defer conversion to list.
    OldFiles0 = version_array.to_list(Array0),
    update_queue(Res, Queue1, _Queue, Mtimes0, Mtimes,
        OldFiles0, OldFiles, [], NewFiless, !IO),
    (
        Res = ok,
        % NewFiless must be non-empty if OldFiles0 != OldFiles,
        % even if the number of new files would be zero.
        NewFiless = [_ | _]
    ->
        list.condense([OldFiles | NewFiless], FilesList),
        list.sort(FilesList, SortedList),
        Array = version_array.from_list(SortedList),
        DirCache = dir_cache(TopDirName, Mtimes, Array)
    ;
        DirCache = dir_cache(TopDirName, Mtimes0, Array0)
    ).

:- pred update_queue(maybe_error::out, set(dirname)::in, set(dirname)::out,
    mtimes::in, mtimes::out, list(file)::in, list(file)::out,
    list(list(file))::in, list(list(file))::out, io::di, io::uo) is det.

update_queue(Res, !Queue, !Mtimes, !OldFiles, !NewFiless, !IO) :-
    ( set.remove_least(DirName, !Queue) ->
        update_dir(DirName, Res0, !Queue, !Mtimes, !OldFiles, !NewFiless, !IO),
        (
            Res0 = ok,
            update_queue(Res, !Queue, !Mtimes, !OldFiles, !NewFiless, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = ok
    ).

:- pred update_dir(dirname::in, maybe_error::out,
    set(dirname)::in, set(dirname)::out,
    mtimes::in, mtimes::out, list(file)::in, list(file)::out,
    list(list(file))::in, list(list(file))::out, io::di, io::uo) is det.

update_dir(DirName, Res, !Queue, !Mtimes, !OldFiles, !NewFiless, !IO) :-
    DirName = dirname(DirNameString),
    io.file_modification_time(DirNameString, ResModTime, !IO),
    (
        ResModTime = ok(ModTime),
        (
            map.search(!.Mtimes, DirName, ModTime0),
            ModTime0 = ModTime
        ->
            % Entry already up-to-date (not including subdirectories).
            Res = ok
        ;
            (
                dir.split_name(DirNameString, _, Tail),
                new_or_cur(Tail)
            ->
                IsNewOrCur = yes
            ;
                IsNewOrCur = no
            ),
            dir.foldl2(enumerate_files(IsNewOrCur), DirNameString,
                [] - !.Queue, Res1, !IO),
            (
                Res1 = ok(DirFiles - !:Queue),
                Res = ok,
                map.set(DirName, ModTime, !Mtimes),
                forget_dir_files(DirName, !OldFiles),
                cons(DirFiles, !NewFiless),

                list.length(DirFiles, NumFiles),
                ( NumFiles > 0 ->
                    io.format("%s contains %d files\n",
                        [s(DirNameString), i(NumFiles)], !IO)
                ;
                    true
                )
            ;
                Res1 = error(_, Error),
                Res = error(io.error_message(Error))
            )
        )
    ;
        ResModTime = error(Error),
        ErrorMsg = io.error_message(Error),
        % XXX check with stat
        ( string.sub_string_search(ErrorMsg, "No such file or directory", _) ->
            map.delete(DirName, !Mtimes),
            Res = ok
        ;
            Res = error(ErrorMsg)
        )
    ).

:- pred enumerate_files(bool::in,
    string::in, string::in, io.file_type::in, bool::out,
    pair(list(file), set(dirname))::in, pair(list(file), set(dirname))::out,
    io::di, io::uo) is det.

enumerate_files(IsNewOrCur, DirName, BaseName, FileType, Continue,
        !.Files - !.Queue, !:Files - !:Queue, !IO) :-
    (
        FileType = regular_file,
        (
            IsNewOrCur = yes,
            not dot_file(BaseName)
        ->
            File = file(basename(BaseName), dirname(DirName)),
            cons(File, !Files)
        ;
            true
        )
    ;
        FileType = directory,
        (
            IsNewOrCur = no,
            % XXX could still scan an already scanned directory?
            set.insert(dirname(DirName / BaseName), !Queue)
        ;
            IsNewOrCur = yes
        )
    ;
        FileType = symbolic_link,
        sorry($module, $pred, "symbolic_link file type")
    ;
        FileType = unknown,
        sorry($module, $pred, "unknown file type")
    ;
        ( FileType = named_pipe
        ; FileType = socket
        ; FileType = character_device
        ; FileType = block_device
        ; FileType = message_queue
        ; FileType = semaphore
        ; FileType = shared_memory
        )
    ),
    Continue = yes.

:- pred dot_file(string::in) is semidet.

dot_file(S) :-
    string.prefix(S, ".").

:- pred new_or_cur(string::in) is semidet.

new_or_cur("new").
new_or_cur("cur").

:- pred forget_dir_files(dirname::in, list(file)::in, list(file)::out) is det.

forget_dir_files(DirName, !Files) :-
    list.negated_filter(file_has_dirname(DirName), !Files).

:- pred file_has_dirname(dirname::in, file::in) is semidet.

file_has_dirname(DirName, file(_, DirName)).

%-----------------------------------------------------------------------------%

update_for_rename(OldDirName, OldBaseName, NewDirName, NewBaseName,
        DirCache0, DirCache) :-
    DirCache0 = dir_cache(TopDirName, Mtimes, Files0),
    update_for_rename_2(OldDirName, OldBaseName, NewDirName, NewBaseName,
        Files0, Files),
    DirCache = dir_cache(TopDirName, Mtimes, Files).

:- pred update_for_rename_2(dirname::in, basename::in,
    dirname::in, basename::in,
    version_array(file)::in, version_array(file)::out) is det.

update_for_rename_2(OldDirName, OldBaseName, NewDirName, NewBaseName,
        Files0, Files) :-
    OldBaseName = basename(OldBaseNameString),
    ( bsearch_prefix(Files0, OldBaseNameString, Index) ->
        File0 = version_array.lookup(Files0, Index),
        ( File0 = file(OldBaseName, OldDirName) ->
            Files = Files0 ^ elem(Index) := file(NewBaseName, NewDirName)
        ;
            unexpected($module, $pred, "unexpected old file")
        )
    ;
        unexpected($module, $pred, "old file not found in cache")
    ).

%-----------------------------------------------------------------------------%

search_files_with_prefix(DirCache, Prefix, Matching) :-
    DirCache = dir_cache(_TopDir, _Mtimes, Array),
    ( bsearch_prefix(Array, Prefix, Index) ->
        take_match_prefix(Array, Prefix, Index, +1, [], MatchingR),
        take_match_prefix(Array, Prefix, Index-1, -1, [], MatchingL),
        Matching = MatchingL ++ reverse(MatchingR)
    ;
        Matching = []
    ).

:- pred bsearch_prefix(version_array(file)::in, string::in, int::out)
    is semidet.

bsearch_prefix(Array, Prefix, Index) :-
    bsearch_prefix_2(Array, Prefix, 0, max(Array), Index).

:- pred bsearch_prefix_2(version_array(file)::in, string::in, int::in, int::in,
    int::out) is semidet.

bsearch_prefix_2(Array, Prefix, Lo, Hi, Index) :-
    ( Hi >= Lo ->
        Mid = (Lo + Hi) // 2,
        version_array.lookup(Array, Mid) = file(basename(BaseName), _),
        ( string.prefix(BaseName, Prefix) ->
            Index = Mid
        ; Prefix @< BaseName ->
            bsearch_prefix_2(Array, Prefix, Lo, Mid - 1, Index)
        ;
            bsearch_prefix_2(Array, Prefix, Mid + 1, Hi, Index)
        )
    ;
        fail
    ).

:- pred take_match_prefix(version_array(file)::in, string::in,
    int::in, int::in, list(file)::in, list(file)::out) is det.

take_match_prefix(Array, Prefix, Index, Delta, !Matching) :-
    ( 0 =< Index, Index =< max(Array) ->
        File = version_array.lookup(Array, Index),
        File = file(basename(BaseName), _DirName),
        ( string.prefix(BaseName, Prefix) ->
            cons(File, !Matching),
            take_match_prefix(Array, Prefix, Index + Delta, Delta, !Matching)
        ;
            true
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

begin(_, 0).

get(dir_cache(_, _, Array), I, I + 1, File) :-
    Max = max(Array),
    ( I =< Max ->
        File = lookup(Array, I)
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

update_watches(Inotify, DirCache, Res, !IO) :-
    DirCache = dir_cache(_TopDirName, Mtimes, _Files),
    map.foldl2(add_watch(Inotify), Mtimes, ok, Res, !IO).

:- pred add_watch(inotify(S)::in, dirname::in, time_t::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

add_watch(Inotify, dirname(DirName), _Time, Res0, Res, !IO) :-
    (
        Res0 = ok,
        dir.split_name(DirName, _, Tail),
        new_or_cur(Tail)
    ->
        is_watched(Inotify, DirName, IsWatched, !IO),
        (
            IsWatched = yes,
            Res = ok
        ;
            IsWatched = no,
            io.format("Adding watch %s\n", [s(DirName)], !IO),
            add_watch(Inotify, DirName, watch_events, Res1, !IO),
            (
                Res1 = ok(_),
                Res = ok
            ;
                Res1 = error(Error),
                Res = error(Error)
            )
        )
    ;
        Res = Res0
    ).

:- func watch_events = list(inotify_event).

watch_events = [modify, close_write, moved_from, delete].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
