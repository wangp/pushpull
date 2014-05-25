%-----------------------------------------------------------------------------%

:- module dir_cache.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- type dir_cache.

:- type dirname == string.

:- type filename == string.

:- func init = dir_cache.

    % Update list of files in the given directory which are
    % regular files (or unknown). Dot-files are ignored.
    %
:- pred update_file_list(dirname::in, dir_cache::in,
    maybe_error(dir_cache)::out, io::di, io::uo) is det.

:- pred update_for_rename(dirname::in, filename::in, dirname::in, filename::in,
    dir_cache::in, dir_cache::out) is det.

    % Mostly for debugging.
    %
:- pred get_file_list(dir_cache::in, dirname::in, list(filename)::out)
    is det.

:- pred search_files_with_prefix(dir_cache::in, dirname::in, string::in,
    list(filename)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module time.

:- type dir_cache == map(dirname, dir).

:- type dir
    --->    dir(
                mtime   :: time_t,
                files   :: bst(string)
            ).

:- type bst(T)
    --->    nil
    ;       node(bst(T), T, bst(T)).

%-----------------------------------------------------------------------------%

init = map.init.

update_file_list(DirName, DirCache0, Res, !IO) :-
    io.file_modification_time(DirName, ResModTime, !IO),
    (
        ResModTime = ok(ModTime),
        (
            map.search(DirCache0, DirName, Dir0),
            Dir0 = dir(ModTime0, _Files0),
            ModTime0 = ModTime
        ->
            % Entry already up-to-date.
            Res = ok(DirCache0)
        ;
            dir.foldl2(enumerate_files, DirName, [], ResFiles, !IO),
            (
                ResFiles = ok(FilesList),
                make_bst(FilesList, FilesTree),
                Dir = dir(ModTime, FilesTree),
                map.set(DirName, Dir, DirCache0, DirCache),
                Res = ok(DirCache)
            ;
                ResFiles = error(_, Error),
                Res = error(io.error_message(Error))
            )
        )
    ;
        ResModTime = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred enumerate_files(string::in, string::in, io.file_type::in, bool::out,
    list(filename)::in, list(filename)::out, io::di, io::uo) is det.

enumerate_files(_DirName, BaseName, FileType, Continue, !Files, !IO) :-
    (
        maybe_regular_file(FileType),
        not dot_file(BaseName)
    ->
        cons(BaseName, !Files)
    ;
        true
    ),
    Continue = yes.

:- pred maybe_regular_file(io.file_type::in) is semidet.

maybe_regular_file(regular_file).
maybe_regular_file(unknown).

:- pred dot_file(string::in) is semidet.

dot_file(S) :-
    string.prefix(S, ".").

%-----------------------------------------------------------------------------%

update_for_rename(OldDirName, OldFileName, NewDirName, NewFileName,
        !DirCache) :-
    (
        OldDirName = NewDirName,
        OldFileName = NewFileName
    ->
        true
    ;
        OldDirName = NewDirName
    ->
        map.lookup(!.DirCache, OldDirName, Dir0),
        Dir0 = dir(ModTime, Files0),
        (
            remove(OldFileName, Files0, Files1),
            insert_new(NewFileName, Files1, Files)
        ->
            Dir = dir(ModTime, Files),
            map.det_update(NewDirName, Dir, !DirCache)
        ;
            unexpected($module, $pred, "remove or insert_new failed")
        )
    ;
        map.lookup(!.DirCache, OldDirName, OldDir0),
        OldDir0 = dir(OldModTime, OldFiles0),
        ( remove(OldFileName, OldFiles0, OldFiles) ->
            OldDir = dir(OldModTime, OldFiles),
            map.det_update(OldDirName, OldDir, !DirCache)
        ;
            unexpected($module, $pred, "remove failed")
        ),

        map.lookup(!.DirCache, NewDirName, NewDir0),
        NewDir0 = dir(NewModTime, NewFiles0),
        ( insert_new(NewFileName, NewFiles0, NewFiles) ->
            NewDir = dir(NewModTime, NewFiles),
            map.det_update(NewDirName, NewDir, !DirCache)
        ;
            unexpected($module, $pred, "remove failed")
        )
    ).

%-----------------------------------------------------------------------------%

get_file_list(DirCache, DirName, FileList) :-
    ( map.search(DirCache, DirName, Dir) ->
        Dir = dir(_ModTime, BST),
        bst_to_list(BST, FileList)
    ;
        FileList = []
    ).

%-----------------------------------------------------------------------------%

search_files_with_prefix(DirCache, DirName, Prefix, Matching) :-
    ( map.search(DirCache, DirName, Dir) ->
        Dir = dir(_ModTime, BST),
        search_bst_prefix(BST, Prefix, [], Matching)
    ;
        Matching = []
    ).

:- pred search_bst_prefix(bst(string)::in, string::in,
    list(string)::in, list(string)::out) is det.

search_bst_prefix(nil, _Prefix, !Matching).
search_bst_prefix(node(L, X, R), Prefix, !Matching) :-
    ( string.prefix(X, Prefix) ->
        IsPrefixOfX = yes
    ;
        IsPrefixOfX = no
    ),
    (
        R = node(_, _, _),
        ( IsPrefixOfX = yes
        ; IsPrefixOfX = no, Prefix @> X
        )
    ->
        search_bst_prefix(R, Prefix, !Matching)
    ;
        true
    ),
    (
        IsPrefixOfX = yes,
        cons(X, !Matching)
    ;
        IsPrefixOfX = no
    ),
    (
        L = node(_, _, _),
        ( IsPrefixOfX = yes
        ; IsPrefixOfX = no, Prefix @< X
        )
    ->
        search_bst_prefix(L, Prefix, !Matching)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred make_bst(list(T)::in, bst(T)::out) is det.

make_bst(UnsortedList, Tree) :-
    list.sort(UnsortedList, SortedList),
    make_bst(SortedList, length(SortedList), Tree).

:- pred make_bst(list(T)::in, int::in, bst(T)::out) is det.

make_bst(Xs, Length, Node) :-
    (
        Xs = [],
        Node = nil
    ;
        Xs = [X],
        Node = node(nil, X, nil)
    ;
        Xs = [_, _ | _],
        LeftLength = Length // 2,
        RightLength = Length - LeftLength,
        ( list.split_list(LeftLength, Xs, LeftList, [Mid | RightList]) ->
            make_bst(RightList, RightLength - 1, RightNode),
            make_bst(LeftList, LeftLength, LeftNode),
            Node = node(LeftNode, Mid, RightNode)
        ;
            unexpected($module, $pred)
        )
    ).

:- pred bst_to_list(bst(T)::in, list(T)::out) is det.

bst_to_list(BST, List) :-
    bst_to_list(BST, [], List).

:- pred bst_to_list(bst(T)::in, list(T)::in, list(T)::out) is det.

bst_to_list(nil, !List).
bst_to_list(node(L, X, R), !List) :-
    bst_to_list(R, !List),
    cons(X, !List),
    bst_to_list(L, !List).

:- pred insert_new(T::in, bst(T)::in, bst(T)::out) is semidet.

insert_new(X, T0, T) :-
    (
        T0 = nil,
        T = node(nil, X, nil)
    ;
        T0 = node(L0, X0, R0),
        compare(Rel, X, X0),
        (
            Rel = (=),
            fail
        ;
            Rel = (<),
            insert_new(X, L0, L),
            T = node(L, X0, R0)
        ;
            Rel = (>),
            insert_new(X, R0, R),
            T = node(L0, X0, R)
        )
    ).

:- pred remove(T::in, bst(T)::in, bst(T)::out) is semidet.

remove(X, T0, T) :-
    (
        T0 = nil,
        fail
    ;
        T0 = node(L0, X0, R0),
        compare(Rel, X, X0),
        (
            Rel = (=),
            T = join(L0, R0)
        ;
            Rel = (<),
            remove(X, L0, L),
            T = node(L, X0, R0)
        ;
            Rel = (>),
            remove(X, R0, R),
            T = node(L0, X0, R)
        )
    ).

:- func join(bst(T), bst(T)) = bst(T).

join(L, R) = T :-
    (
        L = nil,
        R = nil,
        T = nil
    ;
        L = nil,
        R = node(_, _, _) @ T
    ;
        L = node(_, _, _) @ T,
        R = nil
    ;
        L = node(LL, LX, LR),
        R = node(RL, RX, RR),
        expect(LX @< RX, $module, $pred, "LX >= RX"),
        ( height(L) =< height(R) ->
            T = node(LL, LX, join(LR, R))
        ;
            T = node(join(L, RL), RX, RR)
        )
    ).

:- func height(bst(T)) = int.

height(nil) = 0.
height(node(L, _, R)) = max(height(L), height(R)).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
