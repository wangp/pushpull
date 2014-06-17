%-----------------------------------------------------------------------------%

:- module dir_cache.
:- interface.

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

:- pred all_files(dir_cache::in, files::out) is semidet.

:- pred remove_uniquename(uniquename::in, basename::out, files::in, files::out)
    is semidet.

:- pred foldl2(pred(basename, dirname, T, T, U, U), files, T, T, U, U) is det.
:- mode foldl2(pred(in, in, in, out, di, uo) is det, in, in, out, di, uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module time.
:- import_module unit.

:- import_module my_rbtree.

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

:- type basename_tree == rbtree(basename, unit).

:- type enum_info
    --->    enum_info(
                enum_basenames  :: basename_tree,
                enum_count      :: int,
                enum_queue      :: queue(scan_dir),
                enum_progress   :: maybe(int)
            ).

:- type scan_dir == pair(dirname, context).

    % The basename and dirname values are shared with dir_cache
    % so that the extra memory is all in the nodes.
    %
:- type files == rbtree(basename, dirname).

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
    ( queue.get(DirNameContext, !Queue) ->
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
            get_time(Time0, !IO),
            ReportProgress = yes(Time0 + 2),
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
            det_insert(basename(BaseName), unit, BaseNames0, BaseNames),
            Count = Count0 + 1,
            (
                ReportProgress0 = no,
                ReportProgress = no
            ;
                ReportProgress0 = yes(Time0),
                get_time(Time, !IO),
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
        insert(BaseName, unit, BaseNames0, BaseNames),
        map.det_update(DirName, dirinfo(Context, ModTime, BaseNames), !Dirs)
    ;
        DirName = dirname(DirNameString),
        new_or_cur_suffix(DirNameString)
    ->
        % An previously unseen directory.
        BaseNames = singleton(BaseName, unit),
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
    insert(NewBaseName, unit, BaseNames1, BaseNames),
    map.det_update(DirName, dirinfo(Context, ModTime, BaseNames), Dirs0, Dirs).

:- pred det_insert(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out) is det.

det_insert(K, V, !Tree) :-
    ( insert(K, V, !Tree) ->
        true
    ;
        unexpected($module, $pred, "duplicate key")
    ).

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

all_files(dir_cache(_, Dirs), Map) :-
    map.foldl(make_combined_map, Dirs, init, Map).

:- pred make_combined_map(dirname::in, dirinfo::in,
    rbtree(basename, dirname)::in, rbtree(basename, dirname)::out) is semidet.

make_combined_map(DirName, dirinfo(Context, _ModTime, BaseNames), !Map) :-
    (
        Context = top
    ;
        Context = bucket
    ;
        Context = new_or_cur,
        my_rbtree.foldl(make_combined_map_2(DirName), BaseNames, !Map)
    ).

:- pred make_combined_map_2(dirname::in, basename::in, unit::in,
    rbtree(basename, dirname)::in, rbtree(basename, dirname)::out) is semidet.

make_combined_map_2(DirName, BaseName, _unit, !Map) :-
    insert(BaseName, DirName, !Map).

%-----------------------------------------------------------------------------%

remove_uniquename(Unique, BaseName, !Files) :-
    search_uniquename_2(Unique, !.Files, BaseName),
    remove(BaseName, _DirName, !Files).

:- pred search_uniquename_2(uniquename::in, files::in, basename::out)
    is semidet.

search_uniquename_2(UniqueName, Tree, BaseName) :-
    UniqueName = uniquename(Unique),
    (
        Tree = empty,
        fail
    ;
        (
            Tree = red(K, _, L, R)
        ;
            Tree = black(K, _, L, R)
        ),
        K = basename(KString),
        ( string.prefix(KString, Unique) ->
            ( parse_basename(K, UniqueName, _Flags) ->
                BaseName = K
            ;
                fail
            )
        ; Unique @< KString ->
            search_uniquename_2(UniqueName, L, BaseName)
        ;
            search_uniquename_2(UniqueName, R, BaseName)
        )
    ).

%-----------------------------------------------------------------------------%

foldl2(Pred, Tree, !A, !B) :-
    my_rbtree.foldl2(Pred, Tree, !A, !B).

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

:- pred get_time(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_time(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = (MR_Integer) time(0);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
