%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2000, 2003-2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: m.
% Main author: petdr.
% Stability: medium.
%
% Contains an implementation of red black trees.
%
% *** Exit conditions of main predicates ***
% insert:
%   fails if key already in tree.
% update:
%   changes value of key already in tree.  fails if key doesn't exist.
% transform_value:
%   looks up an existing value in the tree, applies a transformation to the
%   value and then updates the value.  fails if the key doesn't exist.
% set:
%   inserts or updates. Never fails.
%
% insert_duplicate:
%   inserts duplicate keys into the tree, never fails.  Search doesn't
%   yet support looking for duplicates.
%
% delete:
%   deletes a node from the tree if it exists.
% remove:
%   fails if node to remove doesn't exist in the tree.
%
% lookup:
%   Aborts program if key looked up doesn't exist.
% search:
%   Fails if key looked up doesn't exist.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module my_rbtree.
:- interface.

:- import_module array.
:- import_module assoc_list.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type rbtree(Key, Value).

    % Initialise the data structure.
    %
:- func init = rbtree(K, V).
:- pred init(rbtree(K, V)::uo) is det.

    % Initialise an rbtree containing the given key-value pair.
    %
:- func singleton(K, V) = rbtree(K, V).

    % Check whether a tree is empty.
    %
:- pred is_empty(rbtree(K, V)::in) is semidet.

    % Inserts a new key-value pair into the tree.
    % Fails if key already in the tree.
    %
:- pred insert(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out)
    is semidet.

    % Updates the value associated with a key.
    % Fails if the key does not exist.
    %
:- pred update(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out)
    is semidet.

    % Update the value at the given key by applying the supplied
    % transformation to it.  Fails if the key is not found.  This is faster
    % than first searching for the value and then updating it.
    %
:- pred transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Sets a value regardless of whether key exists or not.
    %
:- func set(rbtree(K, V), K, V) = rbtree(K, V).
:- pred set(K::in, V::in, rbtree(K, V)::in, rbtree(K, V)::out) is det.

    % Insert a duplicate key into the tree.
    %
:- func insert_duplicate(rbtree(K, V), K, V) = rbtree(K, V).
:- pred insert_duplicate(K::in, V::in,
    rbtree(K, V)::in, rbtree(K, V)::out) is det.

:- pred member(rbtree(K, V)::in, K::out, V::out) is nondet.

    % Search for a key-value pair using the key.
    % Fails if the key does not exist.
    %
:- pred search(rbtree(K, V)::in, K::in, V::out) is semidet.

    % Lookup the value associated with a key.
    % Throws an exception if the key does not exist.
    %
:- func lookup(rbtree(K, V), K) = V.
:- pred lookup(rbtree(K, V)::in, K::in, V::out) is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred lower_bound_search(rbtree(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Throws an exception if there is no key with the given or lower value.
    %
:- pred lower_bound_lookup(rbtree(K, V)::in, K::in, K::out, V::out)
    is det.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred upper_bound_search(rbtree(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key.  If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Throws an exception if there is no key with the given or higher value.
    %
:- pred upper_bound_lookup(rbtree(K, V)::in, K::in, K::out, V::out)
    is det.

    % Delete the key-value pair associated with a key.
    % Does nothing if the key does not exist.
    %
:- func delete(rbtree(K, V), K) = rbtree(K, V).
:- pred delete(K::in, rbtree(K, V)::in, rbtree(K, V)::out) is det.

    % Remove the key-value pair associated with a key.
    % Fails if the key does not exist.
    %
:- pred remove(K::in, V::out, rbtree(K, V)::in, rbtree(K, V)::out)
    is semidet.

    % Deletes the node with the minimum key from the tree,
    % and returns the key and value fields.
    %
:- pred remove_smallest(K::out, V::out,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Deletes the node with the maximum key from the tree,
    % and returns the key and value fields.
    %
:- pred remove_largest(K::out, V::out,
    rbtree(K, V)::in, rbtree(K, V)::out) is semidet.

    % Returns an in-order list of all the keys in the 
    %
:- func keys(rbtree(K, V)) = list(K).
:- pred keys(rbtree(K, V)::in, list(K)::out) is det.

    % Returns a list of values such that the keys associated with the
    % values are in-order.
    %
:- func values(rbtree(K, V)) = list(V).
:- pred values(rbtree(K, V)::in, list(V)::out) is det.

    % Count the number of elements in the tree.
    %
:- func count(rbtree(K, V)) = int.
:- pred count(rbtree(K, V)::in, int::out) is det.

:- func assoc_list_to_rbtree(assoc_list(K, V)) = rbtree(K, V).
:- pred assoc_list_to_rbtree(assoc_list(K, V)::in, rbtree(K, V)::out)
    is det.

:- func from_assoc_list(assoc_list(K, V)) = rbtree(K, V).

:- func rbtree_to_assoc_list(rbtree(K, V)) = assoc_list(K, V).
:- pred rbtree_to_assoc_list(rbtree(K, V)::in, assoc_list(K, V)::out)
    is det.

:- func to_assoc_list(rbtree(K, V)) = assoc_list(K, V).

:- func foldl(func(K, V, T) = T, rbtree(K, V), T) = T.
:- pred foldl(pred(K, V, T, T), rbtree(K, V), T, T).
:- mode foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, in, out) is semidet, in, in, out)
    is semidet.
:- mode foldl(pred(in, in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode foldl(pred(in, in, di, uo) is semidet, in, di, uo)
    is semidet.

:- pred foldl2(pred(K, V, T, T, U, U), rbtree(K, V), T, T, U, U).
:- mode foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldl2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, in, out, array_di, array_uo) is det,
    in, in, out, array_di, array_uo) is det.
:- mode foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

:- pred foldl3(pred(K, V, T, T, U, U, W, W), rbtree(K, V),
    T, T, U, U, W, W).
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.

:- func map_values(func(K, V) = W, rbtree(K, V)) = rbtree(K, W).
:- pred map_values(pred(K, V, W), rbtree(K, V), rbtree(K, W)).
:- mode map_values(pred(in, in, out) is det, in, out) is det.
:- mode map_values(pred(in, in, out) is semidet, in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.

:- interface.

    % Exported for dir_cache.
:- type rbtree(K,V)
    --->    empty
    ;       red(K, V, rbtree(K,V), rbtree(K,V))
    ;       black(K, V, rbtree(K, V), rbtree(K, V)).

:- implementation.

%-----------------------------------------------------------------------------%

init = RBT :-
    init(RBT).

init(empty).

is_empty(Tree) :-
    Tree = empty.

singleton(K, V) = black(K, V, empty, empty).

%-----------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees.
%   * The root node must be black.
%   * There can never be 2 red nodes in a row.

insert(K, V, empty, black(K, V, empty, empty)).
insert(_K, _V, red(_, _, _, _), _Tree) :-
    error("insert: root node cannot be red!").
insert(K, V, black(K0, V0, L0, R0), Tree) :-
    insert_2(black(K0, V0, L0, R0), K, V, Tree0),
    % Ensure that the root of the tree is black.
    (
        Tree0 = black(_, _, _, _),
        Tree = Tree0
    ;
        Tree0 = red(K1, V1, L1, R1),
        Tree = black(K1, V1, L1, R1)
    ;
        Tree0 = empty,
        error("insert: new tree is empty")
    ).

    % insert_2:
    %
    % We traverse down the tree until we find the correct spot to insert.
    % Then as we fall back out of the recursions we look for possible
    % rotation cases.

:- pred insert_2(rbtree(K, V)::in, K::in, V::in, rbtree(K, V)::out)
    is semidet.

% Red node always inserted at the bottom as it will be rotated into the
% correct place as we move back up the tree.
insert_2(empty, K, V, red(K, V, empty, empty)).
insert_2(red(K0, V0, L0, R0), K, V, Tree) :-
    % Work out which side of the rbtree to insert.
    compare(Result, K, K0),
    (
        Result = (<),
        insert_2(L0, K, V, NewL),
        Tree = red(K0, V0, NewL, R0)
    ;
        Result = (>),
        insert_2(R0, K, V, NewR),
        Tree = red(K0, V0, L0, NewR)
    ;
        Result = (=),
        fail
    ).
% Only ever need to look for a possible rotation if we are in a black node.
% The rotation criteria is when there is 2 red nodes in a row.
insert_2(black(K0, V0, L0, R0), K, V, Tree) :-
    (
        L0 = red(LK, LV, LL, LR),
        R0 = red(RK, RV, RL, RR)
    ->
        % On the way down the rbtree we split any 4-nodes we find.
        % This converts the current node to a red node, so we call
        % the red node version of insert_2/4.
        L = black(LK, LV, LL, LR),
        R = black(RK, RV, RL, RR),
        Tree0 = red(K0, V0, L, R),
        insert_2(Tree0, K, V, Tree)
    ;
        % Work out which side of the rbtree to insert.
        compare(Result, K, K0),
        (
            Result = (<),
            insert_2(L0, K, V, NewL),
            (
                % Only need to start looking for a rotation case
                % if the current node is black(known), and its
                % new child red.
                NewL = red(LK, LV, LL, LR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( LL = red(_LLK, _LLV, _LLL, _LLR) ->
                    TreeR = red(K0, V0, LR, R0),
                    Tree = black(LK, LV, LL, TreeR)
                ; LR = red(LRK, LRV, LRL, LRR) ->
                    TreeL = red(LK, LV, LL, LRL),
                    TreeR = red(K0, V0, LRR, R0),
                    Tree = black(LRK, LRV, TreeL, TreeR)
                ;
                    Tree = black(K0, V0, NewL, R0)
                )
            ;
                Tree = black(K0, V0, NewL, R0)
            )
        ;
            Result = (>),
            insert_2(R0, K, V, NewR),
            (
                % Only need to start looking for a rotation case
                % if the current node is black(known), and its
                % new child red.
                NewR = red(RK, RV, RL, RR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( RL = red(RLK, RLV, RLL, RLR) ->
                    TreeL = red(K0, V0, L0, RLL),
                    TreeR = red(RK, RV, RLR, RR),
                    Tree = black(RLK, RLV, TreeL, TreeR)
                ; RR = red(_RRK, _RRV, _RRL, _RRR) ->
                    TreeL = red(K0, V0, L0, RL),
                    Tree = black(RK, RV, TreeL, RR)
                ;
                    Tree = black(K0, V0, L0, NewR)
                )
            ;
                Tree = black(K0, V0, L0, NewR)
            )
        ;
            Result = (=),
            fail
        )
    ).

%-----------------------------------------------------------------------------%

update(_K, _V, empty, _T) :-
    fail.
update(K, V, red(K0, V0, L, R), Tree) :-
    compare(Result, K, K0),
    (
        Result = (=),
        Tree = red(K, V, L, R)
    ;
        Result = (<),
        update(K, V, L, NewL),
        Tree = red(K0, V0, NewL, R)
    ;
        Result = (>),
        update(K, V, R, NewR),
        Tree = red(K0, V0, L, NewR)
    ).
update(K, V, black(K0, V0, L, R), Tree) :-
    compare(Result, K, K0),
    (
        Result = (=),
        Tree = black(K, V, L, R)
    ;
        Result = (<),
        update(K, V, L, NewL),
        Tree = black(K0, V0, NewL, R)
    ;
        Result = (>),
        update(K, V, R, NewR),
        Tree = black(K0, V0, L, NewR)
    ).

%-----------------------------------------------------------------------------%

transform_value(_P, _K, empty, _T) :-
    fail.
transform_value(P, K, red(K0, V0, L, R), Tree) :-
    compare(Result, K, K0),
    (
        Result = (=),
        P(V0, NewV),
        Tree = red(K, NewV, L, R)
    ;
        Result = (<),
        transform_value(P, K, L, NewL),
        Tree = red(K0, V0, NewL, R)
    ;
        Result = (>),
        transform_value(P, K, R, NewR),
        Tree = red(K0, V0, L, NewR)
    ).
transform_value(P, K, black(K0, V0, L, R), Tree) :-
    compare(Result, K, K0),
    (
        Result = (=),
        P(V0, NewV),
        Tree = black(K, NewV, L, R)
    ;
        Result = (<),
        transform_value(P, K, L, NewL),
        Tree = black(K0, V0, NewL, R)
    ;
        Result = (>),
        transform_value(P, K, R, NewR),
        Tree = black(K0, V0, L, NewR)
    ).

%-----------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees.
%   * The root node must be black.
%   * There can never be 2 red nodes in a row.

set(!.RBT, K, V) = !:RBT :-
    set(K, V, !RBT).

set(K, V, empty, black(K, V, empty, empty)).
set(_K, _V, red(_, _, _, _), _Tree) :-
    error("set: root node cannot be red!").
set(K, V, black(K0, V0, L0, R0), Tree) :-
    set_2(black(K0, V0, L0, R0), K, V, Tree0),
    % Ensure that the root of the tree is black.
    (
        Tree0 = black(_, _, _, _),
        Tree = Tree0
    ;
        Tree0 = red(K1, V1, L1, R1),
        Tree = black(K1, V1, L1, R1)
    ;
        Tree0 = empty,
        error("set: new tree is empty")
    ).

    % set_2:
    %
    % We traverse down the tree until we find the correct spot to insert, or
    % update. Then as we fall back out of the recursions we look for possible
    % rotation cases.

:- pred set_2(rbtree(K, V), K, V, rbtree(K, V)).
% :- mode set_2(di, di, di, uo) is det.
:- mode set_2(in, in, in, out) is det.

% Red node always inserted at the bottom as it will be rotated into the
% correct place as we move back up the tree.
set_2(empty, K, V, red(K, V, empty, empty)).
set_2(red(K0, V0, L0, R0), K, V, Tree) :-
    % Work out which side of the rbtree to insert.
    compare(Result, K, K0),
    (
        Result = (=),
        Tree = red(K, V, L0, R0)
    ;
        Result = (<),
        set_2(L0, K, V, NewL),
        Tree = red(K0, V0, NewL, R0)
    ;
        Result = (>),
        set_2(R0, K, V, NewR),
        Tree = red(K0, V0, L0, NewR)
    ).
set_2(black(K0, V0, L0, R0), K, V, Tree) :-
    (
        L0 = red(LK, LV, LL, LR),
        R0 = red(RK, RV, RL, RR)
    ->
        % On the way down the rbtree we split any 4-nodes we find.
        L = black(LK, LV, LL, LR),
        R = black(RK, RV, RL, RR),
        Tree0 = red(K0, V0, L, R),
        set_2(Tree0, K, V, Tree)
    ;
        % Work out which side of the rbtree to insert.
        compare(Result, K, K0),
        (
            Result = (=),
            Tree = black(K, V, L0, R0)
        ;
            Result = (<),
            set_2(L0, K, V, NewL),
            (
                % Only need to start looking for a rotation case
                % if the current node is black(known), and its
                % new child red.
                NewL = red(LK, LV, LL, LR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( LL = red(_LLK, _LLV, _LLL, _LLR) ->
                    TreeR = red(K0, V0, LR, R0),
                    Tree = black(LK, LV, LL, TreeR)
                ; LR = red(LRK, LRV, LRL, LRR) ->
                    TreeL = red(LK, LV, LL, LRL),
                    TreeR = red(K0, V0, LRR, R0),
                    Tree = black(LRK, LRV, TreeL, TreeR)
                ;
                    % NewL2 == NewL, but this hack
                    % needed for unique modes to work.
                    NewL2 = red(LK, LV, LL, LR),
                    Tree = black(K0, V0, NewL2, R0)
                )
            ;
                Tree = black(K0, V0, NewL, R0)
            )
        ;
            Result = (>),
            set_2(R0, K, V, NewR),
            (
                % Only need to start looking for a rotation case
                % if the current node is black(known), and its
                % new child red.
                NewR = red(RK, RV, RL, RR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( RL = red(RLK, RLV, RLL, RLR) ->
                    TreeL = red(K0, V0, L0, RLL),
                    TreeR = red(RK, RV, RLR, RR),
                    Tree = black(RLK, RLV, TreeL, TreeR)
                ; RR = red(_RRK, _RRV, _RRL, _RRR) ->
                    TreeL = red(K0, V0, L0, RL),
                    Tree = black(RK, RV, TreeL, RR)
                ;
                    % NewR2 == NewR, but this hack
                    % needed for unique modes to work.
                    NewR2 = red(RK, RV, RL, RR),
                    Tree = black(K0, V0, L0, NewR2)
                )
            ;
                Tree = black(K0, V0, L0, NewR)
            )
        )
    ).

%-----------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees.
%   * The root node must be black.
%   * There can never be 2 red nodes in a row.

insert_duplicate(!.RBT, K, V) = !:RBT :-
    insert_duplicate(K, V, !RBT).

insert_duplicate(K, V, empty, black(K, V, empty, empty)).
insert_duplicate(_K, _V, red(_, _, _, _), _Tree) :-
    error("insert_duplicate: root node cannot be red!").
insert_duplicate(K, V, black(K0, V0, L0, R0), Tree) :-
    insert_duplicate_2(black(K0, V0, L0, R0), K, V, Tree0),
    % Ensure that the root of the tree is black.
    ( Tree0 = red(K1, V1, L1, R1) ->
        Tree = black(K1, V1, L1, R1)
    ;
        Tree = Tree0
    ).

    % insert_duplicate_2:
    %
    % We traverse down the tree until we find the correct spot to insert.
    % Then as we fall back out of the recursions we look for possible
    % rotation cases.

:- pred insert_duplicate_2(rbtree(K, V), K, V, rbtree(K, V)).
:- mode insert_duplicate_2(in, in, in, out) is det.

% Red node always inserted at the bottom as it will be rotated into the
% correct place as we move back up the tree.
insert_duplicate_2(empty, K, V, red(K, V, empty, empty)).
insert_duplicate_2(red(K0, V0, L0, R0), K, V, Tree) :-
    % Work out which side of the rbtree to insert.
    compare(Result, K, K0),
    (
        Result = (<),
        insert_duplicate_2(L0, K, V, NewL),
        Tree = red(K0, V0, NewL, R0)
    ;
        Result = (>),
        insert_duplicate_2(R0, K, V, NewR),
        Tree = red(K0, V0, L0, NewR)
    ;
        Result = (=),
        insert_duplicate_2(L0, K, V, NewL),
        Tree = red(K0, V0, NewL, R0)
    ).
% Only ever need to look for a possible rotation if we are in a black node.
% The rotation criteria is when there is 2 red nodes in a row.
insert_duplicate_2(black(K0, V0, L0, R0), K, V, Tree) :-
    (
        L0 = red(LK, LV, LL, LR),
        R0 = red(RK, RV, RL, RR)
    ->
        % On the way down the rbtree we split any 4-nodes we find.
        % This converts the current node to a red node, so we call
        % the red node version of insert_duplicate_2/4.
        L = black(LK, LV, LL, LR),
        R = black(RK, RV, RL, RR),
        Tree0 = red(K0, V0, L, R),
        insert_duplicate_2(Tree0, K, V, Tree)
    ;
        % Work out which side of the rbtree to insert.
        compare(Result, K, K0),
        (
            Result = (<),
            insert_duplicate_2(L0, K, V, NewL),
            (
                % Only need to start looking for a rotation case
                % if the current node is black(known), and its
                % new child red.
                NewL = red(LK, LV, LL, LR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( LL = red(_LLK1, _LLV1, _LLL1, _LLR1) ->
                    TreeR = red(K0, V0, LR, R0),
                    Tree = black(LK, LV, LL, TreeR)
                ; LR = red(LRK, LRV, LRL, LRR) ->
                    TreeL = red(LK, LV, LL, LRL),
                    TreeR = red(K0, V0, LRR, R0),
                    Tree = black(LRK, LRV, TreeL, TreeR)
                ;
                    Tree = black(K0, V0, NewL, R0)
                )
            ;
                Tree = black(K0, V0, NewL, R0)
            )
        ;
            Result = (>),
            insert_duplicate_2(R0, K, V, NewR),
            (
                % Only need to start looking for a rotation case
                % if the current node is black(known), and its
                % new child red.
                NewR = red(RK, RV, RL, RR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( RL = red(RLK, RLV, RLL, RLR) ->
                    TreeL = red(K0, V0, L0, RLL),
                    TreeR = red(RK, RV, RLR, RR),
                    Tree = black(RLK, RLV, TreeL, TreeR)
                ; RR = red(_RRK, _RRV, _RRL, _RRR) ->
                    TreeL = red(K0, V0, L0, RL),
                    Tree = black(RK, RV, TreeL, RR)
                ;
                    Tree = black(K0, V0, L0, NewR)
                )
            ;
                Tree = black(K0, V0, L0, NewR)
            )
        ;
            Result = (=),
            insert_duplicate_2(L0, K, V, NewL),
            (
                % Only need to start looking for a rotation case if the
                % current node is black(known), and its new child red.
                NewL = red(LK, LV, LL, LR)
            ->
                % Check to see if a grandchild is red and if so rotate.
                ( LL = red(_LLK2, _LLV2, _LLL2, _LLR2) ->
                    TreeR = red(K0, V0, LR, R0),
                    Tree = black(LK, LV, LL, TreeR)
                ; LR = red(LRK, LRV, LRL, LRR) ->
                    TreeL = red(LK, LV, LL, LRL),
                    TreeR = red(K0, V0, LRR, R0),
                    Tree = black(LRK, LRV, TreeL, TreeR)
                ;
                    Tree = black(K0, V0, NewL, R0)
                )
            ;
                Tree = black(K0, V0, NewL, R0)
            )
        )
    ).

%-----------------------------------------------------------------------------%

member(empty, _K, _V) :- fail.
member(red(K0, V0, Left, Right), K, V) :-
    (
        K = K0,
        V = V0
    ;
        member(Left, K, V)
    ;
        member(Right, K, V)
    ).
member(black(K0, V0, Left, Right), K, V) :-
    (
        K = K0,
        V = V0
    ;
        member(Left, K, V)
    ;
        member(Right, K, V)
    ).

%-----------------------------------------------------------------------------%

search(Tree, K, V) :-
    ( Tree = red(K0, V0, Left, Right)
    ; Tree = black(K0, V0, Left, Right)
    ),
    compare(Result, K, K0),
    (
        Result = (=),
        V = V0
    ;
        Result = (<),
        search(Left, K, V)
    ;
        Result = (>),
        search(Right, K, V)
    ).

lookup(RBT, K) = V :-
    lookup(RBT, K, V).

lookup(T, K, V) :-
    ( search(T, K, V0) ->
        V = V0
    ;
        report_lookup_error("lookup: key not found", K, V)
    ).

%-----------------------------------------------------------------------------%

lower_bound_search(Tree, SearchK, K, V) :-
    ( Tree = red(K0, V0, Left, Right)
    ; Tree = black(K0, V0, Left, Right)
    ),
    compare(Result, SearchK, K0),
    (
        Result = (=),
        K = K0,
        V = V0
    ;
        Result = (<),
        lower_bound_search(Left, SearchK, K, V)
    ;
        Result = (>),
        ( lower_bound_search(Right, SearchK, Kp, Vp) ->
            K = Kp,
            V = Vp
        ;
            K = K0,
            V = V0
        )
    ).

lower_bound_lookup(T, SearchK, K, V) :-
    ( lower_bound_search(T, SearchK, K0, V0) ->
        K = K0,
        V = V0
    ;
        report_lookup_error("lower_bound_lookup: key not found",
            SearchK, V)
    ).

%-----------------------------------------------------------------------------%

upper_bound_search(Tree, SearchK, K, V) :-
    ( Tree = red(K0, V0, Left, Right)
    ; Tree = black(K0, V0, Left, Right)
    ),
    compare(Result, SearchK, K0),
    (
        Result = (=),
        K = K0,
        V = V0
    ;
        Result = (<),
        ( upper_bound_search(Left, SearchK, Kp, Vp) ->
            K = Kp,
            V = Vp
        ;
            K = K0,
            V = V0
        )
    ;
        Result = (>),
        upper_bound_search(Right, SearchK, K, V)
    ).

upper_bound_lookup(T, SearchK, K, V) :-
    ( upper_bound_search(T, SearchK, K0, V0) ->
        K = K0,
        V = V0
    ;
        report_lookup_error("upper_bound_lookup: key not found",
            SearchK, V)
    ).

%-----------------------------------------------------------------------------%

delete(!.RBT, K) = !:RBT :-
    delete(K, !RBT).

delete(K, !Tree) :-
    delete_2(!.Tree, K, no, _, !:Tree).

    % delete_2(Tree0, Key, MustRemove, MaybeValue, Tree):
    %
    % Search the tree Tree0, looking for a node with key Key to delete.
    % If MustRemove is `yes' and we don't find the key, fail.
    % If we find the key, return it in MaybeValue and delete the node.
    % Tree is the resulting tree, whether a node was removed or not.
    %
    % Deletion algorithm:
    %
    % Search down the tree, looking for the node to delete.  O(log N)
    % When we find it, there are 4 possible conditions ->
    %   * Leaf node
    %       Remove node  O(1)
    %   * Left subtree of node to be deleted exists
    %       Move maximum key of Left subtree to current node. O(log N)
    %   * Right subtree of node to be deleted exists
    %       Move minimum key of Right subtree to current node. O(log N)
    %   * Both left and right subtrees of node to be deleted exist
    %       Move maximum key of Left subtree to current node. O(log N)
    %
    % Algorithm O(log N).

:- pred delete_2(rbtree(K, V), K, bool, maybe(V), rbtree(K, V)).
:- mode delete_2(in, in, in, out, out) is semidet.
:- mode delete_2(in, in, in(bound(no)), out, out) is det.

delete_2(empty, _K, no, no, empty).
delete_2(red(K0, V0, L, R), K, MustRemove, MaybeV, Tree) :-
    compare(Result, K, K0),
    (
        Result = (=),
        ( remove_largest(NewK, NewV, L, NewL) ->
            Tree = red(NewK, NewV, NewL, R)
        ;
            % L must be empty.
            ( remove_smallest(NewK, NewV, R, NewR) ->
                Tree = red(NewK, NewV, empty, NewR)
            ;
                % R must be empty
                Tree = empty
            )
        ),
        MaybeV = yes(V0)
    ;
        Result = (<),
        delete_2(L, K, MustRemove, MaybeV, NewL),
        Tree = red(K0, V0, NewL, R)
    ;
        Result = (>),
        delete_2(R, K, MustRemove, MaybeV, NewR),
        Tree = red(K0, V0, L, NewR)
    ).
delete_2(black(K0, V0, L, R), K, MustRemove, MaybeV, Tree) :-
    compare(Result, K, K0),
    (
        Result = (=),
        ( remove_largest(NewK, NewV, L, NewL) ->
            Tree = black(NewK, NewV, NewL, R)
        ;
            % L must be empty
            ( remove_smallest(NewK, NewV, R, NewR) ->
                Tree = black(NewK, NewV, empty, NewR)
            ;
                % R must be empty
                Tree = empty
            )
        ),
        MaybeV = yes(V0)
    ;
        Result = (<),
        delete_2(L, K, MustRemove, MaybeV, NewL),
        Tree = black(K0, V0, NewL, R)
    ;
        Result = (>),
        delete_2(R, K, MustRemove, MaybeV, NewR),
        Tree = black(K0, V0, L, NewR)
    ).

%-----------------------------------------------------------------------------%

remove(K, V, !Tree) :-
    delete_2(!.Tree, K, yes, yes(V), !:Tree).

remove_largest(_K, _V, empty, _Tree) :-
    fail.
remove_largest(NewK, NewV, red(K0, V0, L, R), Tree) :-
    (
        R = empty,
        NewK = K0,
        NewV = V0,
        Tree = L
    ;
        ( R = red(_, _, _, _)
        ; R = black(_, _, _, _)
        ),
        remove_largest(NewK, NewV, R, NewR),
        Tree = red(K0, V0, L, NewR)
    ).
remove_largest(NewK, NewV, black(K0, V0, L, R), Tree) :-
    (
        R = empty,
        NewK = K0,
        NewV = V0,
        Tree = L
    ;
        ( R = red(_, _, _, _)
        ; R = black(_, _, _, _)
        ),
        remove_largest(NewK, NewV, R, NewR),
        Tree = black(K0, V0, L, NewR)
    ).

% remove_smallest:
%   Deletes the node with the minimum K from the tree, and returns the
%   key and value fields.

remove_smallest(_K, _V, empty, _Tree) :-
    fail.
remove_smallest(NewK, NewV, red(K0, V0, L, R), Tree) :-
    (
        L = empty,
        NewK = K0,
        NewV = V0,
        Tree = R
    ;
        ( L = red(_, _, _, _)
        ; L = black(_, _, _, _)
        ),
        remove_smallest(NewK, NewV, L, NewL),
        Tree = red(K0, V0, NewL, R)
    ).
remove_smallest(NewK, NewV, black(K0, V0, L, R), Tree) :-
    (
        L = empty,
        NewK = K0,
        NewV = V0,
        Tree = R
    ;
        ( L = red(_, _, _, _)
        ; L = black(_, _, _, _)
        ),
        remove_smallest(NewK, NewV, L, NewL),
        Tree = black(K0, V0, NewL, R)
    ).

%-----------------------------------------------------------------------------%

keys(RBT) = Ks :-
    keys(RBT, Ks).

keys(empty, []).
keys(red(K0, _V0, L, R), List) :-
    keys(L, List0),
    keys(R, List1),
    list.append(List0, [K0 | List1], List).
keys(black(K0, _V0, L, R), List) :-
    keys(L, List0),
    keys(R, List1),
    list.append(List0, [K0 | List1], List).

%-----------------------------------------------------------------------------%

values(RBT) = Vs :-
    values(RBT, Vs).

values(empty, []).
values(red(_K0, V0, L, R), List) :-
    values(L, List0),
    values(R, List1),
    list.append(List0, [V0 | List1], List).
values(black(_K0, V0, L, R), List) :-
    values(L, List0),
    values(R, List1),
    list.append(List0, [V0 | List1], List).

%-----------------------------------------------------------------------------%

count(RBT) = N :-
    count(RBT, N).

count(empty, 0).
count(red(_K, _V, L, R), N) :-
    count(L, NO),
    count(R, N1),
    N = 1 + NO + N1.
count(black(_K, _V, L, R), N) :-
    count(L, NO),
    count(R, N1),
    N = 1 + NO + N1.

%-----------------------------------------------------------------------------%

from_assoc_list(AList) = assoc_list_to_rbtree(AList).

assoc_list_to_rbtree(AL) = RBT :-
    assoc_list_to_rbtree(AL, RBT).

assoc_list_to_rbtree([], empty).
assoc_list_to_rbtree([K - V | T], Tree) :-
    assoc_list_to_rbtree(T, Tree0),
    set(K, V, Tree0, Tree).

%-----------------------------------------------------------------------------%

to_assoc_list(T) = rbtree_to_assoc_list(T).

rbtree_to_assoc_list(RBT) = AL :-
    rbtree_to_assoc_list(RBT, AL).

rbtree_to_assoc_list(empty, []).
rbtree_to_assoc_list(red(K0, V0, Left, Right), L) :-
    rbtree_to_assoc_list(Left, L0),
    rbtree_to_assoc_list(Right, L1),
    list.append(L0, [K0 - V0|L1], L).
rbtree_to_assoc_list(black(K0, V0, Left, Right), L) :-
    rbtree_to_assoc_list(Left, L0),
    rbtree_to_assoc_list(Right, L1),
    list.append(L0, [K0 - V0|L1], L).

%-----------------------------------------------------------------------------%

foldl(F, T, A) = B :-
    P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    foldl(P, T, A, B).

foldl(_Pred, empty, !Acc).
foldl(Pred, red(K, V, Left, Right), !Acc) :-
    foldl(Pred, Left, !Acc),
    Pred(K, V, !Acc),
    foldl(Pred, Right, !Acc).
foldl(Pred, black(K, V, Left, Right), !Acc) :-
    foldl(Pred, Left, !Acc),
    Pred(K, V, !Acc),
    foldl(Pred, Right, !Acc).

%-----------------------------------------------------------------------------%

foldl2(_, empty, !Acc1, !Acc2).
foldl2(Pred, red(K, V, Left, Right), !Acc1, !Acc2) :-
    foldl2(Pred, Left, !Acc1, !Acc2),
    Pred(K, V, !Acc1, !Acc2),
    foldl2(Pred, Right, !Acc1, !Acc2).
foldl2(Pred, black(K, V, Left, Right), !Acc1, !Acc2) :-
    foldl2(Pred, Left, !Acc1, !Acc2),
    Pred(K, V, !Acc1, !Acc2),
    foldl2(Pred, Right, !Acc1, !Acc2).

%-----------------------------------------------------------------------------%

foldl3(_, empty, !Acc1, !Acc2, !Acc3).
foldl3(Pred, red(K, V, Left, Right), !Acc1, !Acc2, !Acc3) :-
    foldl3(Pred, Left, !Acc1, !Acc2, !Acc3),
    Pred(K, V, !Acc1, !Acc2, !Acc3),
    foldl3(Pred, Right, !Acc1, !Acc2, !Acc3).
foldl3(Pred, black(K, V, Left, Right), !Acc1, !Acc2, !Acc3) :-
    foldl3(Pred, Left, !Acc1, !Acc2, !Acc3),
    Pred(K, V, !Acc1, !Acc2, !Acc3),
    foldl3(Pred, Right, !Acc1, !Acc2, !Acc3).

%-----------------------------------------------------------------------------%

map_values(F, T1) = T2 :-
    P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    map_values(P, T1, T2).

map_values(_Pred, empty, empty).
map_values(Pred, Tree0, Tree) :-
    Tree0 = red(K0, V0, Left0, Right0),
    Pred(K0, V0, W0),
    map_values(Pred, Left0, Left),
    map_values(Pred, Right0, Right),
    Tree = red(K0, W0, Left, Right).
map_values(Pred, Tree0, Tree) :-
    Tree0 = black(K0, V0, Left0, Right0),
    Pred(K0, V0, W0),
    map_values(Pred, Left0, Left),
    map_values(Pred, Right0, Right),
    Tree = black(K0, W0, Left, Right).

%-----------------------------------------------------------------------------%
:- end_module my_rbtree.
%-----------------------------------------------------------------------------%
