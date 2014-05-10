%-----------------------------------------------------------------------------%

:- module flag_delta.
:- interface.

:- import_module bool.
:- import_module set.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type flag_deltas(S)
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

:- func init_flags(set(flag)) = flag_deltas(S).

    % update_flags(Flags, !FlagDeltas, IsChanged)
    % Assuming Flags contains the entire set of flags update !FlagDeltas.
    %
:- pred update_flags(set(flag)::in,
    flag_deltas(S)::in, flag_deltas(S)::out, bool::out) is det.

    % update_maildir_standard_flags(Flags, !FlagDeltas, IsChanged)
    % Assuming Flags contains the entire set of maildir-standard flags
    % update !FlagDeltas.
    %
:- pred update_maildir_standard_flags(set(flag)::in,
    flag_deltas(S)::in, flag_deltas(S)::out, bool::out) is det.

    % apply_flag_deltas(!L, !R)
    % Apply nonconflicting deltas from R to L.
    %
:- pred apply_flag_deltas(flag_deltas(L)::in, flag_deltas(L)::out,
    flag_deltas(R)::in, flag_deltas(R)::out) is det.

:- func require_attn(flag_deltas(S)) = bool.

:- func to_string(flag_deltas(S)) = string.

:- pred from_string(string::in, flag_deltas(S)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module io.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

init_flags(Flags) = sets(Flags, Plus, Minus) :-
    Plus = set.init,
    Minus = set.init.

update_flags(Flags, Sets0, Sets, IsChanged) :-
    Sets0 = sets(Cur0, Plus0, Minus0),
    ( Cur0 = Flags ->
        Sets = Sets0,
        IsChanged = no
    ;
        Cur = Flags,
        Plus = difference(Cur, Cur0) `union` intersect(Plus0, Cur),
        Minus = difference(Cur0, Cur) `union` difference(Minus0, Cur),
        Sets = sets(Cur, Plus, Minus),
        IsChanged = yes
    ).

update_maildir_standard_flags(Flags, Sets0, Sets, IsChanged) :-
    Sets0 = sets(Cur0, _, _),
    Cur = difference(Cur0, maildir_standard_flags) `union` Flags,
    update_flags(Cur, Sets0, Sets, IsChanged).

:- func maildir_standard_flags = set(flag).

maildir_standard_flags = set.from_list([
    system(answered),
    system(flagged),
    system(deleted),
    system(seen),
    system(draft)
]).

    %   For L{F +G -H}
    %
    %   R{+F}   =>  L{F +G -H}      +F nonconflict, (add F), (remove +F)
    %   R{+G}   =>  L{F  G -H}      +G nonconflict, (add G), remove +G
    %   R{+H}   =>  L{F +G -H}      +H conflicts
    %   R{+I}   =>  L{F +G -H I}    +I nonconflict, add I, (remove +I)
    %
    %   R{-F}   =>  L{  +G -H}      -F nonconflict, remove F, (remove -F)
    %   R{-G}   =>  L{F +G -H}      -G conflicts
    %   R{-H}   =>  L{F +G   }      -H nonconflict, (remove H), remove -H
    %   R{-I}   =>  L{F +G -H}      -I nonconflict, (remove I), (remove -I)
    %
apply_flag_deltas(L0, L, R0, R) :-
    R0 = sets(Rcur, Rplus, Rminus),
    L0 = sets(Lcur0, Lplus0, Lminus0),

    Rplus_conflict = Rplus `intersect` Lminus0,
    Rminus_conflict = Rminus `intersect` Lplus0,

    Rplus_nonconflict = Rplus `difference` Rplus_conflict,
    Rminus_nonconflict = Rminus `difference` Rminus_conflict,

    Lcur = (Lcur0 `union` Rplus_nonconflict) `difference` Rminus_nonconflict,

    Lplus = Lplus0 `difference` Rplus_nonconflict,
    Lminus = Lminus0 `difference` Rminus_nonconflict,

    L = sets(Lcur, Lplus, Lminus),
    R = sets(Rcur, Rplus_conflict, Rminus_conflict).

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
