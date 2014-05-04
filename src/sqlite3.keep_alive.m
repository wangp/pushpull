%-----------------------------------------------------------------------------%

:- module sqlite3.keep_alive.
:- interface.

    % This is to prevent the theoretical(?) possibility that the GC collects
    % strings which have been bound to a statement but are otherwise
    % unreachable from being collected.  Then we can bind strings with
    % SQLITE_STATIC so SQLite does not need its own copy of the string.
    %
:- pred keep_alive(assoc_list(bind_index, bind_value)::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma no_inline(keep_alive/3).

:- pragma foreign_proc("C",
    keep_alive(KeepAlive::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    (void) KeepAlive;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
