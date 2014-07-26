%-----------------------------------------------------------------------------%

% This is just array.sort from the Mercury standard library
% adapted to version arrays.

:- module va_sort.
:- interface.

:- import_module version_array.

    % sort(Array) returns a version of Array sorted into ascending
    % order.
    %
    % This sort is not stable. That is, elements that compare/3 decides are
    % equal will appear together in the sorted array, but not necessarily
    % in the same order in which they occurred in the input version_array. This is
    % primarily only an issue with types with user-defined equivalence for
    % which `equivalent' objects are otherwise distinguishable.
    %
:- func sort(version_array(T)) = version_array(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

sort(A) = samsort_subarray(A, 0, max(A)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % SAMsort (smooth applicative merge) invented by R.A. O'Keefe.
    %
    % SAMsort is a mergesort variant that works by identifying contiguous
    % monotonic sequences and merging them, thereby taking advantage of
    % any existing order in the input sequence.
    %
:- func samsort_subarray(version_array(T)::in, int::in, int::in) =
    (version_array(T)::out) is det.

samsort_subarray(A0, Lo, Hi) = A :-
    samsort_up(0, A0, _, version_array.copy(A0), A, Lo, Hi, Lo).

:- pred samsort_up(int::in, version_array(T)::in, version_array(T)::out,
    version_array(T)::in, version_array(T)::out, int::in, int::in, int::in)
    is det.

    % Precondition:
    %   We are N levels from the bottom (leaf nodes) of the tree.
    %   A0 is sorted from Lo .. I - 1.
    %   A0 and B0 are identical from I .. Hi.
    % Postcondition:
    %   B is sorted from Lo .. Hi.
    %
samsort_up(N, A0, A, B0, B, Lo, Hi, I) :-
    ( I > Hi ->
        A = A0,
        B = B0
    ; N > 0 ->
        samsort_down(N - 1, B0, B1, A0, A1, I, Hi, J),
        % A1 is sorted from I .. J - 1.
        % A1 and B1 are identical from J .. Hi.
        merge_subarrays(A1, Lo, I - 1, I, J - 1, Lo, B1, B2),
        A2 = A1,
        % B2 is sorted from Lo .. J - 1.
        samsort_up(N + 1, B2, B, A2, A, Lo, Hi, J)
    ;
        % N = 0, I = Lo
        copy_run_ascending(A0, B0, B1, Lo, Hi, J),
        % B1 is sorted from Lo .. J - 1.
        samsort_up(N + 1, B1, B, A0, A, Lo, Hi, J)
    ).

:- pred samsort_down(int::in, version_array(T)::in, version_array(T)::out,
    version_array(T)::in, version_array(T)::out, int::in, int::in, int::out)
    is det.

    % Precondition:
    %   We are N levels from the bottom (leaf nodes) of the tree.
    %   A0 and B0 are identical from Lo .. Hi.
    % Postcondition:
    %   B is sorted from Lo .. I - 1.
    %   A and B are identical from I .. Hi.
    %
samsort_down(N, A0, A, B0, B, Lo, Hi, I) :-
    ( Lo > Hi ->
        A = A0,
        B = B0,
        I = Lo
    ; N > 0 ->
        samsort_down(N - 1, B0, B1, A0, A1, Lo, Hi, J),
        samsort_down(N - 1, B1, B2, A1, A2, J,  Hi, I),
        % A2 is sorted from Lo .. J - 1.
        % A2 is sorted from J  .. I - 1.
        A = A2,
        merge_subarrays(A2, Lo, J - 1, J, I - 1, Lo, B2, B)
        % B is sorted from Lo .. I - 1.
    ;
        A = A0,
        copy_run_ascending(A0, B0, B, Lo, Hi, I)
        % B is sorted from Lo .. I - 1.
    ).

%------------------------------------------------------------------------------%

:- pred copy_run_ascending(version_array(T)::in,
    version_array(T)::in, version_array(T)::out, int::in, int::in, int::out) is det.

copy_run_ascending(A, !B, Lo, Hi, I) :-
    (
        Lo < Hi,
        compare((>), A ^ elem(Lo), A ^ elem(Lo + 1))
    ->
        I = search_until((<), A, Lo, Hi),
        copy_subarray_reverse(A, Lo, I - 1, I - 1, !B)
    ;
        I = search_until((>), A, Lo, Hi),
        copy_subarray(A, Lo, I - 1, Lo, !B)
    ).

%------------------------------------------------------------------------------%

:- func search_until(comparison_result::in, version_array(T)::in,
    int::in, int::in) = (int::out) is det.

search_until(R, A, Lo, Hi) =
    (
        Lo < Hi,
        not compare(R, A ^ elem(Lo), A ^ elem(Lo + 1))
    ->
        search_until(R, A, Lo + 1, Hi)
    ;
        Lo + 1
    ).

%------------------------------------------------------------------------------%

    % Assigns the subarray A[Lo..Hi] to B[InitI..Final], where InitI
    % is the initial value of I, and FinalI = InitI + (Ho - Lo + 1).
    % In this version, I is ascending, so B[InitI] gets A[Lo]
    %
:- pred copy_subarray(version_array(T)::in, int::in, int::in, int::in,
    version_array(T)::in, version_array(T)::out) is det.

copy_subarray(A, Lo, Hi, I, !B) :-
    ( Lo =< Hi ->
        !B ^ elem(I) := A ^ elem(Lo),
        copy_subarray(A, Lo + 1, Hi, I + 1, !B)
    ;
        true
    ).

    % Assigns the subarray A[Lo..Hi] to B[InitI..Final], where InitI
    % is the initial value of I, and FinalI = InitI - (Ho - Lo + 1).
    % In this version, I is descending, so B[InitI] gets A[Hi].
    %
:- pred copy_subarray_reverse(version_array(T)::in, int::in, int::in, int::in,
    version_array(T)::in, version_array(T)::out) is det.

copy_subarray_reverse(A, Lo, Hi, I, !B) :-
    ( Lo =< Hi ->
        !B ^ elem(I) := A ^ elem(Lo),
        copy_subarray_reverse(A, Lo + 1, Hi, I - 1, !B)
    ;
        true
    ).

%------------------------------------------------------------------------------%

    % merges the two sorted consecutive subarrays Lo1 .. Hi1 and
    % Lo2 .. Hi2 from A into the subarray starting at I in B.
    %
:- pred merge_subarrays(version_array(T)::in,
    int::in, int::in, int::in, int::in, int::in,
    version_array(T)::in, version_array(T)::out) is det.

merge_subarrays(A, Lo1, Hi1, Lo2, Hi2, I, !B) :-
    ( Lo1 > Hi1 ->
        copy_subarray(A, Lo2, Hi2, I, !B)
    ; Lo2 > Hi2 ->
        copy_subarray(A, Lo1, Hi1, I, !B)
    ;
        X1 = A ^ elem(Lo1),
        X2 = A ^ elem(Lo2),
        compare(R, X1, X2),
        (
            R = (<),
            version_array.set(I, X1, !B),
            merge_subarrays(A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, !B)
        ;
            R = (=),
            version_array.set(I, X1, !B),
            merge_subarrays(A, Lo1 + 1, Hi1, Lo2, Hi2, I + 1, !B)
        ;
            R = (>),
            version_array.set(I, X2, !B),
            merge_subarrays(A, Lo1, Hi1, Lo2 + 1, Hi2, I + 1, !B)
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
