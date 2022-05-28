% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module dir_cache.
:- interface.

:- import_module array.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module inotify.
:- import_module log.
:- import_module maildir.   % XXX cyclic
:- import_module path.

%-----------------------------------------------------------------------------%

    % Cache the file names in a single Maildir mailbox.
    %
:- type dir_cache.

:- type file
    --->    file(basename, dirname).

:- type update_method
    --->    scan_all
    ;       scan_from_inotify_events(bool).

:- func init(dirname) = dir_cache.

:- pred update_dir_cache(log::in, inotify(S)::in, update_method::in,
    bool::in, maybe_error(bool)::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

:- pred update_for_new_file(dirname::in, basename::in,
    dir_cache::in, dir_cache::out) is semidet.

:- pred update_for_rename(dirname::in, basename::in, dirname::in, basename::in,
    dir_cache::in, dir_cache::out) is semidet.

:- pred search_files_with_prefix(dir_cache::in, string::in, list(file)::out)
    is det.

%-----------------------------------------------------------------------------%

:- type files.

:- pred all_files(dir_cache::in, files::array_uo) is semidet.

:- pred remove_uniquename(uniquename::in, basename::out,
    files::array_di, files::array_uo) is semidet.

:- pred foldl2(pred(basename, dirname, T, T, U, U), files, T, T, U, U).
:- mode foldl2(pred(in, in, in, out, di, uo) is det, array_ui, in, out, di, uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module float.
:- import_module map.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module time.

:- import_module gettimeofday.
:- import_module my_rbtree.
:- import_module signal.

:- type dir_cache
    --->    dir_cache(
                toplevel    :: dirname,
                dirinfos    :: dirinfos
            ).

:- type dirinfos == map(dirname, dirinfo).

:- type dirinfo
    --->    dirinfo(
                context     :: context,
                mtime       :: maybe(time_t),
                basenames   :: basename_tree
                % Only new_or_cur should have non-empty basename_tree.
            ).

:- type context
    --->    top
    ;       bucket
    ;       new_or_cur.

:- type basename_tree == rbtree(basename, dummy).

:- type dummy
    --->    dummy(int).

:- type enum_info
    --->    enum_info(
                enum_basenames  :: basename_tree,
                enum_count      :: int,
                enum_queue      :: queue(scan_dir),
                enum_progress   :: maybe(float)
            ).

:- type scan_dir == pair(dirname, context).

:- type files == array(file_or_tombstone).

    % The basename and dirname values are shared with dir_cache
    % so that the extra memory is all in the nodes.
    %
:- type file_or_tombstone
    --->    file(basename, dirname)
    ;       tombstone(basename).

%-----------------------------------------------------------------------------%

init(Top) = dir_cache(Top, map.init).

%-----------------------------------------------------------------------------%

update_dir_cache(Log, Inotify, Method, AddNewWatches, Res, !DirCache, !IO) :-
    % In theory we could maintain our dir cache on the basis of inotify
    % events only (after the initial scan) but it seems too hairy.
    inotify.read_events(Inotify, ResEvents, !IO),
    (
        Method = scan_all,
        !.DirCache = dir_cache(TopDirName, Dirs0),
        List0 = list.map(scan_element, map.to_assoc_list(Dirs0)),
        List1 = [TopDirName - top | List0],
        Queue = queue.from_list(set.to_sorted_list(set.from_list(List1))),
        update_dir_cache_2(Log, Inotify, Queue, AddNewWatches, Res,
            !DirCache, !IO)
    ;
        Method = scan_from_inotify_events(Force),
        (
            ResEvents = ok(Events),
            list.foldl(evaluate_event(!.DirCache), Events, set.init, Set),
            ( set.is_empty(Set) ->
                Res = ok(Force)
            ;
                Queue = queue.from_list(to_sorted_list(Set)),
                update_dir_cache_2(Log, Inotify, Queue, AddNewWatches, Res,
                    !DirCache, !IO)
            )
        ;
            ResEvents = error(Error1),
            Res = error(Error1)
        )
    ).

:- func scan_element(pair(dirname, dirinfo)) = pair(dirname, context).

scan_element(DirName - DirInfo) = DirName - DirInfo ^ context.

:- pred update_dir_cache_2(log::in, inotify(S)::in, queue(scan_dir)::in,
    bool::in, maybe_error(bool)::out, dir_cache::in, dir_cache::out,
    io::di, io::uo) is det.

update_dir_cache_2(Log, Inotify, Queue, AddNewWatches, Res, !DirCache, !IO) :-
    update_by_scan(Log, Queue, Res0, !DirCache, !IO),
    (
        Res0 = ok,
        (
            AddNewWatches = yes,
            add_watches(Log, Inotify, !.DirCache, Res1, !IO)
        ;
            AddNewWatches = no,
            Res1 = ok
        ),
        (
            Res1 = ok,
            Res = ok(yes)
        ;
            Res1 = error(Error),
            Res = error(Error)
        )
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

:- pred update_by_scan(log::in, queue(scan_dir)::in, maybe_error::out,
    dir_cache::in, dir_cache::out, io::di, io::uo) is det.

update_by_scan(Log, Queue0, Res, DirCache0, DirCache, !IO) :-
    DirCache0 = dir_cache(TopDirName, Dirs0),
    update_queue(Log, Res, Queue0, _Queue, Dirs0, Dirs, !IO),
    ( Res = ok ->
        DirCache = dir_cache(TopDirName, Dirs)
    ;
        DirCache = DirCache0
    ).

:- pred update_queue(log::in, maybe_error::out, queue(scan_dir)::in,
    queue(scan_dir)::out, dirinfos::in, dirinfos::out, io::di, io::uo) is det.

update_queue(Log, Res, !Queue, !Dirs, !IO) :-
    signal.get_sigint_or_sigterm_count(InterruptCount, !IO),
    ( InterruptCount > 0 ->
        Res = error("interrupted")
    ; queue.get(DirNameContext, !Queue) ->
        update_dir(Log, DirNameContext, Res0, !Queue, !Dirs, !IO),
        (
            Res0 = ok,
            update_queue(Log, Res, !Queue, !Dirs, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        Res = ok
    ).

:- pred update_dir(log::in, pair(dirname, context)::in, maybe_error::out,
    queue(scan_dir)::in, queue(scan_dir)::out, dirinfos::in, dirinfos::out,
    io::di, io::uo) is det.

update_dir(Log, DirName - Context, Res, !Queue, !Dirs, !IO) :-
    DirName = dirname(DirNameString),
    io.file_modification_time(DirNameString, ResModTime, !IO),
    (
        ResModTime = ok(ModTime),
        (
            Context = new_or_cur,
            map.search(!.Dirs, DirName, dirinfo(Context, yes(ModTime0), _)),
            ModTime0 = ModTime
        ->
            % Entry already up-to-date (not including subdirectories).
            log_debug(Log, "Directory unchanged " ++ DirNameString, !IO),
            Res = ok
        ;
            log_debug(Log, "Scanning " ++ DirNameString, !IO),
            gettimeofday_float(Time0, !IO),
            ReportProgress = yes(Time0 + 2.0),
            Info0 = enum_info(init, 0, !.Queue, ReportProgress),
            dir.foldl2(enumerate_files(Log, Context), DirNameString,
                Info0, Res1, !IO),
            (
                Res1 = ok(Info),
                Info = enum_info(DirFiles, Count, !:Queue, _ReportProgress),
                map.set(DirName, dirinfo(Context, yes(ModTime), DirFiles),
                    !Dirs),
                ( Count > 0 ->
                    log_debug(Log,
                        format("%s contains %d files\n",
                            [s(DirNameString), i(Count)]), !IO)
                ;
                    true
                ),
                Res = ok
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
            delete(DirName, !Dirs),
            Res = ok
        ;
            Res = error(ErrorMsg)
        )
    ).

:- pred enumerate_files(log::in, context::in,
    string::in, string::in, io.file_type::in, bool::out,
    enum_info::in, enum_info::out, io::di, io::uo) is det.

enumerate_files(Log, Context, DirName, BaseName, FileType, Continue,
        !Info, !IO) :-
    (
        FileType = regular_file,
        (
            Context = new_or_cur,
            not dot_file(BaseName)
        ->
            !.Info = enum_info(BaseNames0, Count0, Queue, ReportProgress0),
            det_insert(basename(BaseName), dummy, BaseNames0, BaseNames),
            Count = Count0 + 1,
            (
                ReportProgress0 = no,
                ReportProgress = no
            ;
                ReportProgress0 = yes(Time0),
                gettimeofday_float(Time, !IO),
                ( Time >= Time0 ->
                    log_info(Log, "Scanning " ++ DirName ++ " ...", !IO),
                    ReportProgress = no
                ;
                    ReportProgress = ReportProgress0
                )
            ),
            !:Info = enum_info(BaseNames, Count, Queue, ReportProgress)
        ;
            true
        )
    ;
        FileType = directory,
        SubDirName = dirname(DirName / BaseName),
        (
            Context = top,
            ( new_or_cur(BaseName) ->
                queue_scan(SubDirName - new_or_cur, !Info)
            ; bucketlike(BaseName) ->
                queue_scan(SubDirName - bucket, !Info)
            ;
                true
            )
        ;
            Context = bucket,
            ( new_or_cur(BaseName) ->
                queue_scan(SubDirName - new_or_cur, !Info)
            ;
                true
            )
        ;
            Context = new_or_cur
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

:- pred queue_scan(scan_dir::in, enum_info::in, enum_info::out) is det.

queue_scan(ScanDir, !Info) :-
    Queue0 = !.Info ^ enum_queue,
    queue.put(ScanDir, Queue0, Queue),
    !Info ^ enum_queue := Queue.

%-----------------------------------------------------------------------------%

:- pred evaluate_event(dir_cache::in, inotify_event(S)::in,
    set(pair(dirname, context))::in, set(pair(dirname, context))::out) is det.

evaluate_event(DirCache, Event, !Set) :-
    Event = inotify_event(_Watch, DirNameString, Mask, _Cookie, MaybeName),
    DirName = dirname(DirNameString),
    lookup(DirCache, DirName, DirInfo),
    Context = DirInfo ^ context,
    % If we already knew about the moved files (because we did it)
    % then that is no reason to rescan the directory.
    ( contains(Mask, moved_from) ->
        (
            MaybeName = yes(BaseName),
            not contains_file(DirInfo, basename(BaseName))
        ->
            true
        ;
            set.insert(DirName - Context, !Set)
        )
    ; contains(Mask, moved_to) ->
        (
            MaybeName = yes(BaseName),
            contains_file(DirInfo, basename(BaseName))
        ->
            true
        ;
            set.insert(DirName - Context, !Set)
        )
    ;
        set.insert(DirName - Context, !Set)
    ).

:- pred lookup(dir_cache::in, dirname::in, dirinfo::out) is det.

lookup(dir_cache(_TopDir, Dirs), DirName, DirInfo) :-
    map.lookup(Dirs, DirName, DirInfo).

:- pred contains_file(dirinfo::in, basename::in) is semidet.

contains_file(dirinfo(_Context, _ModTime, BaseNames), BaseName) :-
    search(BaseNames, BaseName, _).

%-----------------------------------------------------------------------------%

update_for_new_file(DirName, NewBaseName, DirCache0, DirCache) :-
    DirCache0 = dir_cache(TopDirName, Dirs0),
    insert_cached(DirName, NewBaseName, Dirs0, Dirs),
    DirCache = dir_cache(TopDirName, Dirs).

update_for_rename(OldDirName, OldBaseName, NewDirName, NewBaseName,
        DirCache0, DirCache) :-
    DirCache0 = dir_cache(TopDirName, Dirs0),
    ( OldDirName = NewDirName ->
        rename_cached(OldDirName, OldBaseName, NewBaseName, Dirs0, Dirs)
    ;
        remove_cached(OldDirName, OldBaseName, Dirs0, Dirs1),
        insert_cached(NewDirName, NewBaseName, Dirs1, Dirs)
    ),
    DirCache = dir_cache(TopDirName, Dirs).

:- pred insert_cached(dirname::in, basename::in, dirinfos::in, dirinfos::out)
    is semidet.

insert_cached(DirName, BaseName, !Dirs) :-
    ( map.search(!.Dirs, DirName, dirinfo(Context, ModTime, BaseNames0)) ->
        insert(BaseName, dummy, BaseNames0, BaseNames),
        map.det_update(DirName, dirinfo(Context, ModTime, BaseNames), !Dirs)
    ;
        DirName = dirname(DirNameString),
        new_or_cur_suffix(DirNameString)
    ->
        % An previously unseen directory.
        BaseNames = singleton(BaseName, dummy),
        map.det_insert(DirName, dirinfo(new_or_cur, no, BaseNames), !Dirs)
    ;
        unexpected($module, $pred, "not new or cur")
    ).

:- pred remove_cached(dirname::in, basename::in, dirinfos::in, dirinfos::out)
    is semidet.

remove_cached(DirName, BaseName, Dirs0, Dirs) :-
    map.search(Dirs0, DirName, dirinfo(Context, ModTime, BaseNames0)),
    remove(BaseName, _, BaseNames0, BaseNames),
    map.det_update(DirName, dirinfo(Context, ModTime, BaseNames), Dirs0, Dirs).

:- pred rename_cached(dirname::in, basename::in, basename::in,
    dirinfos::in, dirinfos::out) is semidet.

rename_cached(DirName, OldBaseName, NewBaseName, Dirs0, Dirs) :-
    map.search(Dirs0, DirName, dirinfo(Context, ModTime, BaseNames0)),
    remove(OldBaseName, _, BaseNames0, BaseNames1),
    insert(NewBaseName, dummy, BaseNames1, BaseNames),
    map.det_update(DirName, dirinfo(Context, ModTime, BaseNames), Dirs0, Dirs).

:- pred det_insert(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out) is det.

det_insert(K, V, !Tree) :-
    ( insert(K, V, !Tree) ->
        true
    ;
        unexpected($module, $pred, "duplicate key")
    ).

    % Use an initialised dummy value to avoid retaining garbage.
:- func dummy = dummy.

dummy = dummy(0).

%-----------------------------------------------------------------------------%

search_files_with_prefix(DirCache, Prefix, Matching) :-
    DirCache = dir_cache(_TopDir, Dirs0),
    map.foldl(search_prefix(Prefix), Dirs0, [], Matching).

:- pred search_prefix(string::in, dirname::in, dirinfo::in,
    list(file)::in, list(file)::out) is det.

search_prefix(Prefix, DirName, dirinfo(Context, _ModTime, BaseNames),
        !Matching) :-
    (
        Context = new_or_cur,
        search_prefix_2(Prefix, DirName, BaseNames, !Matching)
    ;
        Context = top
    ;
        Context = bucket
    ).

:- pred search_prefix_2(string::in, dirname::in, basename_tree::in,
    list(file)::in, list(file)::out) is det.

search_prefix_2(Prefix, DirName, Tree, !Matching) :-
    (
        Tree = empty
    ;
        (
            Tree = red(K, _, L, R)
        ;
            Tree = black(K, _, L, R)
        ),
        K = basename(KString),
        ( string.prefix(KString, Prefix) ->
            search_prefix_2(Prefix, DirName, R, !Matching),
            cons(file(K, DirName), !Matching),
            search_prefix_2(Prefix, DirName, L, !Matching)
        ; Prefix @< KString ->
            search_prefix_2(Prefix, DirName, L, !Matching)
        ;
            search_prefix_2(Prefix, DirName, R, !Matching)
        )
    ).

%-----------------------------------------------------------------------------%

all_files(dir_cache(_, Dirs), Array) :-
    map.foldr2(gather_new_or_cur_dirs, Dirs, [], NewOrCurDirs,
        0, TotalBaseNames),
    Array0 = array.init(TotalBaseNames, tombstone(basename(""))),
    fill_array(NewOrCurDirs, 0, Index, Array0, Array1),
    expect(unify(TotalBaseNames, Index),
        $module, $pred, "TotalBaseNames != Index"),
    % The trees are already sorted, so it seems like we could do better here.
    % I tried populating the array by multi-level merging of lazy-streamed
    % trees; that used less memory but was a bit slower.  I tried merge sorting
    % the subarrays, which are populated in sorted order; that turned out to
    % perform basically the same as samsort, even though samsort has to
    % identify the subarrays.
    Array = sort(Array1),
    ( size(Array) > 0 ->
        ( lookup(Array, 0) = file(BaseName0, _) ->
            sanity_check_array(Array, BaseName0, 1, size(Array))
        ;
            unexpected($module, $pred, "missing file entry")
        )
    ;
        true
    ).

:- pred gather_new_or_cur_dirs(dirname::in, dirinfo::in,
    list(pair(dirname, basename_tree))::in,
    list(pair(dirname, basename_tree))::out, int::in, int::out) is det.

gather_new_or_cur_dirs(DirName, DirInfo, !NewOrCurDirs, !CountBaseNames) :-
    DirInfo = dirinfo(Context, _ModTime, BaseNames),
    (
        Context = top
    ;
        Context = bucket
    ;
        Context = new_or_cur,
        cons(DirName - BaseNames, !NewOrCurDirs),
        !:CountBaseNames = !.CountBaseNames + my_rbtree.count(BaseNames)
    ).

:- pred fill_array(list(pair(dirname, basename_tree))::in, int::in, int::out,
    files::array_di, files::array_uo) is det.

fill_array([], !Index, !Array).
fill_array([H | T], !Index, !Array) :-
    H = DirName - BaseNames,
    my_rbtree.foldl2(fill_array_2(DirName), BaseNames, !Index, !Array),
    fill_array(T, !Index, !Array).

:- pred fill_array_2(dirname::in, basename::in, dummy::in, int::in, int::out,
    array(file_or_tombstone)::array_di, array(file_or_tombstone)::array_uo)
    is det.

fill_array_2(DirName, BaseName, _unit, Index, Index + 1, !Array) :-
    array.set(Index, file(BaseName, DirName), !Array).

:- pred sanity_check_array(files::in, basename::in, int::in, int::in)
    is semidet.

sanity_check_array(Array, PrevBaseName, I, Size) :-
    ( I < Size ->
        ( lookup(Array, I) = file(BaseName, _) ->
            PrevBaseName @< BaseName,
            sanity_check_array(Array, BaseName, I + 1, Size)
        ;
            unexpected($module, $pred, "not file entry")
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

remove_uniquename(Unique, BaseName, !Files) :-
    Lo = 0,
    Hi = size(!.Files) - 1,
    binsearch(!.Files, Unique, Lo, Hi, Index),
    lookup(!.Files, Index) = file(BaseName, _DirName),
    array.set(Index, tombstone(BaseName), !Files).

:- pred binsearch(files::in, uniquename::in, int::in, int::in, int::out)
    is semidet.

binsearch(Array, UniqueName, Lo, Hi, Index) :-
    Lo =< Hi,
    Mid = (Lo + Hi) / 2,
    MidX = lookup(Array, Mid),
    (
        MidX = file(MidBaseName, _)
    ;
        MidX = tombstone(MidBaseName)
    ),
    compare_basename_uniquename(Rel, MidBaseName, UniqueName),
    (
        Rel = (=),
        Index = Mid
    ;
        Rel = (<),
        binsearch(Array, UniqueName, Mid + 1, Hi, Index)
    ;
        Rel = (>),
        binsearch(Array, UniqueName, Lo, Mid - 1, Index)
    ).

:- pred compare_basename_uniquename(comparison_result::out,
    basename::in, uniquename::in) is det.

compare_basename_uniquename(Rel, BaseName, UniqueName) :-
    BaseName = basename(BN),
    UniqueName = uniquename(Unique),
    (
        string.prefix(BN, Unique),
        parse_basename(BaseName, UniqueName)
    ->
        Rel = (=)
    ;
        compare(Rel, BN, Unique)
    ).

%-----------------------------------------------------------------------------%

foldl2(Pred, Array, !A, !B) :-
    foldl2(Pred, Array, 0, size(Array), !A, !B).

:- pred foldl2(pred(basename, dirname, T, T, U, U), files, int, int,
    T, T, U, U).
:- mode foldl2(pred(in, in, in, out, di, uo) is det, in, in, in,
    in, out, di, uo) is det.

foldl2(P, Array, I, Size, !A, !B) :-
    ( I < Size ->
        X = lookup(Array, I),
        (
            X = file(BaseName, DirName),
            P(BaseName, DirName, !A, !B)
        ;
            X = tombstone(_)
        ),
        foldl2(P, Array, I + 1, Size, !A, !B)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred add_watches(log::in, inotify(S)::in, dir_cache::in, maybe_error::out,
    io::di, io::uo) is det.

add_watches(Log, Inotify, DirCache, Res, !IO) :-
    DirCache = dir_cache(_TopDirName, Dirs),
    map.foldl2(add_watch(Log, Inotify), Dirs, ok, Res, !IO).

:- pred add_watch(log::in, inotify(S)::in, dirname::in, T::in,
    maybe_error::in, maybe_error::out, io::di, io::uo) is det.

add_watch(Log, Inotify, dirname(DirName), _, Res0, Res, !IO) :-
    (
        Res0 = ok,
        new_or_cur_suffix(DirName)
    ->
        is_watched(Inotify, DirName, IsWatched, !IO),
        (
            IsWatched = yes,
            Res = ok
        ;
            IsWatched = no,
            log_debug(Log, format("Adding watch %s\n", [s(DirName)]), !IO),
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

:- func watch_events = list(event_type).

watch_events = [close_write, moved_from, moved_to, delete].

%-----------------------------------------------------------------------------%

:- pred dot_file(string::in) is semidet.

dot_file(S) :-
    string.prefix(S, ".").

:- pred new_or_cur(string::in) is semidet.

new_or_cur("new").
new_or_cur("cur").

:- pred new_or_cur_suffix(string::in) is semidet.

new_or_cur_suffix(S) :-
    % dir.basename and dir.split_name are a bit slow.
    ( string.suffix(S, "/new")
    ; string.suffix(S, "/cur")
    ).

:- pred bucketlike(string::in) is semidet.

bucketlike(S) :-
    string.to_char_list(S, [X, Y]),
    lower_hex_digit(X),
    lower_hex_digit(Y).

:- pred lower_hex_digit(char::in) is semidet.

lower_hex_digit(C) :- char.is_digit(C).
lower_hex_digit('a').
lower_hex_digit('b').
lower_hex_digit('c').
lower_hex_digit('d').
lower_hex_digit('e').
lower_hex_digit('f').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
