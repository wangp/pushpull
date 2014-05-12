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

:- type dir_cache == map(dirname, bst(string)).

:- type bst(T)
    --->    nil
    ;       node(bst(T), T, bst(T)).

%-----------------------------------------------------------------------------%

init = map.init.

update_file_list(DirName, DirCache0, Res, !IO) :-
    dir.foldl2(enumerate_files, DirName, [], Res0, !IO),
    (
        Res0 = ok(FilesList),
        make_bst(FilesList, FilesTree),
        map.set(DirName, FilesTree, DirCache0, DirCache),
        Res = ok(DirCache)
    ;
        Res0 = error(_, Error),
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

get_file_list(DirCache, DirName, FileList) :-
    ( map.search(DirCache, DirName, BST) ->
        bst_to_list(BST, FileList)
    ;
        FileList = []
    ).

%-----------------------------------------------------------------------------%

search_files_with_prefix(DirCache, DirName, Prefix, Matching) :-
    ( map.search(DirCache, DirName, BST) ->
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
