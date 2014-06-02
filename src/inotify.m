%-----------------------------------------------------------------------------%

:- module inotify.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type inotify(S).

:- type watch(S).

:- type inotify_event
    --->    access
    ;       modify
    ;       attrib
    ;       close_write
    ;       close_nowrite
    ;       open
    ;       moved_from
    ;       moved_to
    ;       create
    ;       delete
    ;       delete_self
    ;       move_self.

:- some [S] pred init(maybe_error(inotify(S))::out, io::di, io::uo) is det.

:- pred close(inotify(S)::in, io::di, io::uo) is det.

:- pred add_watch(inotify(S)::in, string::in, list(inotify_event)::in,
    maybe_error(watch(S))::out, io::di, io::uo) is det.

:- pred remove_watch(inotify(S)::in, watch(S)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred is_watched(inotify(S)::in, string::in, bool::out, io::di, io::uo)
    is det.

:- pred get_filedes(inotify(S)::in, int::out) is det.

:- pred read_all(inotify(S)::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module int.
:- import_module store.

:- type inotify(S)
    --->    inotify(
                events_fd :: int,
                watch :: io_mutvar(bimap(watch(S), string))
            ).

:- type watch(S)
    --->    watch(int). % watch descriptor

:- type dummy
    --->    dummy.

:- pragma foreign_decl("C", "
    #include <sys/inotify.h>
").

:- pragma foreign_decl("C", local, "
    #include <sys/ioctl.h>
").

:- pragma foreign_enum("C", inotify_event/0,
    [
        access          - "IN_ACCESS",
        modify          - "IN_MODIFY",
        attrib          - "IN_ATTRIB",
        close_write     - "IN_CLOSE_WRITE",
        close_nowrite   - "IN_CLOSE_NOWRITE",
        open            - "IN_OPEN",
        moved_from      - "IN_MOVED_FROM",
        moved_to        - "IN_MOVED_TO",
        create          - "IN_CREATE",
        delete          - "IN_DELETE",
        delete_self     - "IN_DELETE_SELF",
        move_self       - "IN_MOVE_SELF"
    ]).

%-----------------------------------------------------------------------------%

init(Res, !IO) :-
    inotify_init(Fd, Error, !IO),
    ( Fd = -1 ->
        Res = error(Error)
    ;
        new_mutvar(bimap.init, WatchesVar, !IO),
        Res = ok(inotify(Fd, WatchesVar) : inotify(dummy))
    ).

:- pred inotify_init(int::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    inotify_init(Fd::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Fd = inotify_init();
    if (Fd != -1) {
        Error = MR_make_string_const("""");
    } else {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    }
").

%-----------------------------------------------------------------------------%

close(inotify(Fd, WatchesVar), !IO) :-
    close_2(Fd, !IO),
    set_mutvar(WatchesVar, bimap.init, !IO).

:- pred close_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_2(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    close(Fd);
").

%-----------------------------------------------------------------------------%

add_watch(inotify(Fd, WatchesVar), PathName, EventList, Res, !IO) :-
    foldl(event_mask, EventList, 0) = Mask,
    inotify_add_watch(Fd, PathName, Mask, WatchDescr, Error, !IO),
    ( WatchDescr >= 0 ->
        Watch = watch(WatchDescr),
        get_mutvar(WatchesVar, Watches0, !IO),
        bimap.set(Watch, PathName, Watches0, Watches),
        set_mutvar(WatchesVar, Watches, !IO),
        Res = ok(Watch)
    ;
        Res = error(Error)
    ).

:- func event_mask(inotify_event, int) = int.

event_mask(Event, Mask) = Mask \/ bit(Event).

:- func bit(inotify_event) = int.

:- pragma foreign_proc("C",
    bit(X0::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = X0;
").

:- pred inotify_add_watch(int::in, string::in, int::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    inotify_add_watch(Fd::in, PathName::in, Mask::in, WatchDescr::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    WatchDescr = inotify_add_watch(Fd, PathName, Mask);
    if (WatchDescr >= 0) {
        Error = MR_make_string_const("""");
    } else {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    }
").

%-----------------------------------------------------------------------------%

remove_watch(inotify(Fd, WatchesVar), Watch, Res, !IO) :-
    Watch = watch(WatchDescr),
    inotify_rm_watch(Fd, WatchDescr, RC, Error, !IO),
    ( RC = 0 ->
        remove_watch_from_map(WatchesVar, Watch, !IO),
        Res = ok
    ;
        Res = error(Error)
    ).

:- pred inotify_rm_watch(int::in, int::in, int::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    inotify_rm_watch(Fd::in, WatchDescr::in, RC::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    RC = inotify_rm_watch(Fd, WatchDescr);
    if (RC == 0) {
        Error = MR_make_string_const("""");
    } else {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    }
").

:- pred remove_watch_from_map(io_mutvar(bimap(watch(S), string))::in,
    watch(S)::in, io::di, io::uo) is det.

remove_watch_from_map(WatchesVar, Watch, !IO) :-
    get_mutvar(WatchesVar, Watches0, !IO),
    bimap.delete_key(Watch, Watches0, Watches),
    set_mutvar(WatchesVar, Watches, !IO).

%-----------------------------------------------------------------------------%

is_watched(inotify(_Fd, WatchesVar), PathName, IsWatched, !IO) :-
    get_mutvar(WatchesVar, Watches, !IO),
    ( bimap.reverse_search(Watches, _, PathName) ->
        IsWatched = yes
    ;
        IsWatched = no
    ).

%-----------------------------------------------------------------------------%

get_filedes(inotify(Fd, _), Fd).

%-----------------------------------------------------------------------------%

read_all(Inotify, Res, !IO) :-
    Inotify = inotify(Fd, WatchesVar),
    read_next(Fd, RC, Error, RemovedWatchDescr, !IO),
    ( RC = 0 ->
        Res = ok
    ; RC = 1 ->
        read_all(Inotify, Res, !IO)
    ; RC = 2 ->
        remove_watch_from_map(WatchesVar, watch(RemovedWatchDescr), !IO),
        read_all(Inotify, Res, !IO)
    ;
        Res = error(Error)
    ).

:- pred read_next(int::in, int::out, string::out, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    read_next(Fd::in, RC::out, Error::out, RemovedWatchDescr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int queue_length;

    Error = MR_make_string_const("""");
    RemovedWatchDescr = -1;

    RC = ioctl(Fd, FIONREAD, &queue_length);
    if (RC == -1) {
        RC = -1;
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
    }
    else if (queue_length == 0) {
        RC = 0;
    }
    else {
        char buf[sizeof(struct inotify_event) + NAME_MAX + 1];
        ssize_t sz;

        sz = read(Fd, buf, sizeof(buf));
        if (sz <= 0) {
            RC = -1;
            Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
        } else {
            const struct inotify_event *ev = (const struct inotify_event *)buf;
            if (ev->mask & IN_IGNORED) {
                RC = 2;
                RemovedWatchDescr = ev->wd;
            } else {
                RC = 1;
            }
        }
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
