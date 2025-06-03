% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module flag_delta.
:- interface.

:- import_module bool.
:- import_module set.

:- import_module imap.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

:- type local_mailbox ---> local_mailbox.
:- type remote_mailbox ---> remote_mailbox.

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

:- type expunged(S)
    --->    exists
    ;       expunged.

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

    % Add Deleted flag if not already present.
    %
:- pred add_deleted_flag(flag_deltas(S)::in, flag_deltas(S)::out) is det.

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

:- import_module char.
:- import_module list.
:- import_module parsing_utils.
:- import_module string.
:- import_module unit.

:- import_module imap.charclass.

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
:- pragma memo(maildir_standard_flags/0).

maildir_standard_flags = set.from_list([
    system(answered),
    system(flagged),
    system(deleted),
    system(seen),
    system(draft)
]).

add_deleted_flag(Sets0, Sets) :-
    Sets0 = sets(Cur0, Plus0, Minus0),
    DeletedFlag = system(deleted),
    ( set.contains(Cur0, DeletedFlag) ->
        Sets = Sets0
    ;
        set.insert(DeletedFlag, Cur0, Cur),
        set.insert(DeletedFlag, Plus0, Plus),
        set.delete(DeletedFlag, Minus0, Minus),
        Sets = sets(Cur, Plus, Minus)
    ).

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

%-----------------------------------------------------------------------------%

% Flags are represented by single char like maildir where possible,
% otherwise surrounded by parentheses. Atoms are as for IMAP.
%
%   R       \Answered
%   F       \Flagged
%   T       \Deleted
%   S       \Seen
%   D       \Draft
%   (\EXT)  \EXT
%   (ATOM)  ATOM
%
%   + signifies plus set if present
%   - signifies minus set if present

to_string(Sets) = String :-
    Sets = sets(Cur, Plus, Minus),
    CurNotPlus = Cur `difference` Plus,
    some [!Acc] (
        !:Acc = [],
        ( set.is_empty(Minus) ->
            true
        ;
            list.foldr(cons, set.to_sorted_list(Minus), !Acc),
            cons("-", !Acc)
        ),
        ( set.is_empty(Plus) ->
            true
        ;
            list.foldr(cons, set.to_sorted_list(Plus), !Acc),
            cons("+", !Acc)
        ),
        list.foldr(cons, set.to_sorted_list(CurNotPlus), !Acc),
        string.append_list(!.Acc, String)
    ).

:- pred cons(flag::in, list(string)::in, list(string)::out) is det.

cons(Flag, !Acc) :-
    (
        Flag = system(SystemFlag),
        cons_system_flag(SystemFlag, !Acc)
    ;
        Flag = keyword(atom(Atom)),
        cons("(" ++ Atom ++ ")", !Acc)
    ).

:- pred cons_system_flag(system_flag::in, list(string)::in, list(string)::out)
    is det.

cons_system_flag(Flag, !Acc) :-
    (
        Flag = answered,
        cons("R", !Acc)
    ;
        Flag = flagged,
        cons("F", !Acc)
    ;
        Flag = deleted,
        cons("T", !Acc)
    ;
        Flag = seen,
        cons("S", !Acc)
    ;
        Flag = draft,
        cons("D", !Acc)
    ;
        Flag = extension(atom(Atom)),
        cons("(\\" ++ Atom ++ ")", !Acc)
    ).

%-----------------------------------------------------------------------------%

    % Most flag delta strings are the same.
:- pragma memo(from_string/2).

from_string(Input, Sets) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, no_skip_whitespace, parse_sets, ParseResult)
    ),
    ParseResult = ok(Sets).

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, PS, PS) :-
    semidet_true.

:- pred parse_sets(src::in, flag_deltas(S)::out, ps::in, ps::out) is semidet.

parse_sets(Src, Sets, !PS) :-
    zero_or_more(parse_flag, Src, Cur0, !PS),
    ( next_char(Src, '+', !PS) ->
        zero_or_more(parse_flag, Src, PlusList, !PS),
        Plus = set.from_list(PlusList),
        Cur = set.from_list(PlusList ++ Cur0)
    ;
        Plus = set.init,
        Cur = set.from_list(Cur0)
    ),
    ( next_char(Src, '-', !PS) ->
        zero_or_more(parse_flag, Src, MinusList, !PS),
        Minus = set.from_list(MinusList)
    ;
        Minus = set.init
    ),
    eof(Src, _, !PS),
    Sets = sets(Cur, Plus, Minus).

:- pred parse_flag(src::in, flag::out, ps::in, ps::out) is semidet.

parse_flag(Src, Flag, !PS) :-
    next_char(Src, Char, !PS),
    (
        Char = 'R',
        Flag = system(answered)
    ;
        Char = 'F',
        Flag = system(flagged)
    ;
        Char = 'T',
        Flag = system(deleted)
    ;
        Char = 'S',
        Flag = system(seen)
    ;
        Char = 'D',
        Flag = system(draft)
    ;
        Char = ('('),
        ( next_char(Src, ('\\'), !PS) ->
            atom(Src, Atom, !PS),
            system_flag(Atom, SystemFlag),
            Flag = system(SystemFlag)
        ;
            atom(Src, Atom, !PS),
            Flag = keyword(Atom)
        ),
        next_char(Src, ')', !PS)
    ).

:- pred atom(src::in, atom::out, ps::in, ps::out) is semidet.

atom(Src, atom(Atom), !PS) :-
    one_or_more(atom_char, Src, Chars, !PS),
    string.from_char_list(Chars, Atom0),
    string.to_upper(Atom0, Atom).

:- pred atom_char(src::in, char::out, ps::in, ps::out) is semidet.

atom_char(Src, Char, !PS) :-
    next_char(Src, Char, !PS),
    'ATOM-CHAR'(Char).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
