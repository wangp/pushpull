%-----------------------------------------------------------------------------%

:- module flag_delta.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module set.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type flag_deltas
    --->    sets(
                % Set of current flags.
                cur_set :: set(flag),

                % Subset of cur_set, which have yet to be added to the other
                % side.
                plus_set :: set(flag),

                % Removed flags which have yet to be removed on the other side.
                % The intersection of minus_set and cur_set is the empty set.
                minus_set :: set(flag)
            ).

:- func init_flag_deltas(list(flag)) = flag_deltas.

:- func update_flag_deltas(flag_deltas, list(flag)) = flag_deltas.

:- func require_attn(flag_deltas) = bool.

:- func to_string(flag_deltas) = string.

:- pred from_string(string::in, flag_deltas::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------------%

init_flag_deltas(Flags) = sets(Cur, Plus, Minus) :-
    Cur = set.from_list(Flags),
    Plus = set.init,
    Minus = set.init.

update_flag_deltas(Sets0, Flags) = Sets :-
    Sets0 = sets(Cur0, Plus0, Minus0),
    Cur = set.from_list(Flags),
    Plus = difference(Cur, Cur0) `union` intersect(Plus0, Cur),
    Minus = difference(Cur0, Cur) `union` difference(Minus0, Cur),
    Sets = sets(Cur, Plus, Minus).

require_attn(Sets) = Attn :-
    Sets = sets(_Cur, Plus, Minus),
    (
        set.is_empty(Plus),
        set.is_empty(Minus)
    ->
        Attn = no
    ;
        Attn = yes
    ).

% For now.

to_string(Sets) = string(Sets).

from_string(Input0, Sets) :-
    Input = Input0 ++ ".",
    io.read_from_string("", Input, length(Input), Result,
        posn(1, 0, 0), _Posn),
    Result = ok(Sets).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
