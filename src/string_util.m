% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module string_util.
:- interface.

:- import_module char.

:- pred strcase_equal(string::in, string::in) is semidet.

:- pred advance_while(pred(char)::in(pred(in) is semidet), string::in,
    int::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    strcase_equal(SA::in, SB::in),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (strcasecmp(SA, SB) == 0);
").

%-----------------------------------------------------------------------------%

advance_while(Pred, String, I0, I) :-
    (
        string.unsafe_index_next(String, I0, I1, Char),
        Pred(Char)
    ->
        advance_while(Pred, String, I1, I)
    ;
        I = I0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
