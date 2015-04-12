% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module inotify.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type inotify(S).

:- type watch(S).

:- type event_type
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

:- type inotify_event(S)
    --->    inotify_event(
                watch :: watch(S),
                path :: string,
                mask :: mask,
                cookie :: cookie,
                maybe_name :: maybe(string)
            ).

:- type mask.

:- type cookie.

:- type emptiness
    --->    empty
    ;       nonempty.

%-----------------------------------------------------------------------------%

:- some [S] pred init(maybe_error(inotify(S))::out, io::di, io::uo) is det.

:- pred close(inotify(S)::in, io::di, io::uo) is det.

:- pred add_watch(inotify(S)::in, string::in, list(event_type)::in,
    maybe_error(watch(S))::out, io::di, io::uo) is det.

:- pred remove_watch(inotify(S)::in, watch(S)::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred is_watched(inotify(S)::in, string::in, bool::out, io::di, io::uo)
    is det.

:- pred get_filedes(inotify(S)::in, int::out) is det.

:- pred check_queue(inotify(S)::in, maybe_error(emptiness)::out,
    io::di, io::uo) is det.

:- pred read_events(inotify(S)::in, maybe_error(list(inotify_event(S)))::out,
    io::di, io::uo) is det.

:- pred buffer_events(inotify(S)::in, maybe_error::out, io::di, io::uo) is det.

:- pred contains(mask::in, event_type::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module int.
:- import_module queue.
:- import_module store.

:- type inotify(S)
    --->    inotify(
                events_fd :: int,
                watches :: io_mutvar(watches(S)),
                buffer :: io_mutvar(queue(inotify_event(S)))
            ).

:- type watch(S)
    --->    watch(int). % watch descriptor

:- type watches(S) == bimap(watch(S), string).

:- type mask == int.

:- type cookie == int.

:- type dummy
    --->    dummy.

:- pragma foreign_decl("C", "
    #include <sys/inotify.h>
").

:- pragma foreign_decl("C", local, "
    #include <sys/ioctl.h>
").

:- pragma foreign_enum("C", event_type/0,
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
        new_mutvar(queue.init, BufferVar, !IO),
        Res = ok(inotify(Fd, WatchesVar, BufferVar) : inotify(dummy))
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

close(inotify(Fd, WatchesVar, BufferVar), !IO) :-
    close_2(Fd, !IO),
    set_mutvar(WatchesVar, bimap.init, !IO),
    set_mutvar(BufferVar, queue.init, !IO).

:- pred close_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_2(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    close(Fd);
").

%-----------------------------------------------------------------------------%

add_watch(Inotify, PathName, EventList, Res, !IO) :-
    Inotify = inotify(Fd, WatchesVar, _BufferVar),
    Mask = list.foldl(make_mask, EventList, 0),
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

:- func make_mask(event_type, mask) = mask.

make_mask(Event, Mask) = Mask \/ bit(Event).

:- pred inotify_add_watch(int::in, string::in, mask::in, int::out, string::out,
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

remove_watch(inotify(Fd, WatchesVar, _BufferVar), Watch, Res, !IO) :-
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

is_watched(inotify(_Fd, WatchesVar, _BufferVar), PathName, IsWatched, !IO) :-
    get_mutvar(WatchesVar, Watches, !IO),
    ( bimap.reverse_search(Watches, _, PathName) ->
        IsWatched = yes
    ;
        IsWatched = no
    ).

%-----------------------------------------------------------------------------%

get_filedes(inotify(Fd, _, _), Fd).

%-----------------------------------------------------------------------------%

check_queue(inotify(Fd, _, BufferVar), Res, !IO) :-
    get_mutvar(BufferVar, Queue, !IO),
    ( queue.is_full(Queue) ->
        Res = ok(nonempty)
    ;
        get_queue_length(Fd, RC, Error, Length, !IO),
        ( RC = 0 ->
            ( Length = 0 ->
                Res = ok(empty)
            ;
                Res = ok(nonempty)
            )
        ;
            Res = error(Error)
        )
    ).

:- pred get_queue_length(int::in, int::out, string::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_queue_length(Fd::in, RC::out, Error::out, Length::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int queue_length;
    RC = ioctl(Fd, FIONREAD, &queue_length);
    if (RC == -1) {
        Error = MR_make_string(MR_ALLOC_ID, ""%s"", strerror(errno));
        Length = -1;
    } else {
        RC = 0;
        Length = queue_length;
    }
").

%-----------------------------------------------------------------------------%

read_events(Inotify, Res, !IO) :-
    Inotify = inotify(_Fd, _WatchesVar, BufferVar),
    buffer_events(Inotify, Res0, !IO),
    (
        Res0 = ok,
        get_mutvar(BufferVar, Queue, !IO),
        set_mutvar(BufferVar, queue.init, !IO),
        Res = ok(to_list(Queue))
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

buffer_events(inotify(Fd, WatchesVar, BufferVar), Res, !IO) :-
    get_mutvar(WatchesVar, Watches0, !IO),
    get_mutvar(BufferVar, Queue0, !IO),

    read_all(Fd, Watches0, Res, [], RevEvents1, !IO),
    list.foldl(remove_ignored, RevEvents1, Watches0, Watches),
    queue.put_list(reverse(RevEvents1), Queue0, Queue),

    set_mutvar(WatchesVar, Watches, !IO),
    set_mutvar(BufferVar, Queue, !IO).

:- pred read_all(int::in, watches(S)::in, maybe_error::out,
    list(inotify_event(S))::in, list(inotify_event(S))::out, io::di, io::uo)
    is det.

read_all(Fd, Watches, Res, !RevEvents, !IO) :-
    read_some(Fd, Watches, RC, Error, !RevEvents, !IO),
    ( RC = 0 ->
        Res = ok
    ; RC = 1 ->
        read_all(Fd, Watches, Res, !RevEvents, !IO)
    ;
        Res = error(Error)
    ).

:- pred read_some(int::in, watches(S)::in, int::out, string::out,
    list(inotify_event(S))::in, list(inotify_event(S))::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    read_some(Fd::in, Watches::in, RC::out, Error::out,
        RevEvents0::in, RevEvents::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, not_thread_safe, tabled_for_io,
        may_not_duplicate],
"
    int queue_length;

    Error = MR_make_string_const("""");
    RevEvents = RevEvents0;

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
            const char *ev_ptr = buf;

            while (ev_ptr < buf + sz) {
                const struct inotify_event *ev = (const struct inotify_event *)ev_ptr;
                MR_String name;

                if (ev->len > 0) {
                    MR_make_aligned_string_copy_msg(name, ev->name, MR_ALLOC_ID);
                } else {
                    name = MR_make_string_const("""");
                }

                cons_event(TypeInfo_for_S, Watches, ev->wd, ev->mask,
                    ev->cookie, name, RevEvents, &RevEvents);

                ev_ptr += sizeof(struct inotify_event) + ev->len;
            }

            RC = 1;
        }
    }
").

:- pred cons_event(watches(S)::in, int::in, mask::in, cookie::in,
    string::in, list(inotify_event(S))::in, list(inotify_event(S))::out) is det.

:- pragma foreign_export("C", cons_event(in, in, in, in, in, in, out),
    "cons_event").

cons_event(Watches, WatchDescr, Mask, Cookie, Name, RevEvents0, RevEvents) :-
    Watch = watch(WatchDescr),
    % Get the path up front, as the watch may be removed by the time the user
    % reads the event (especially for IN_IGNORED events).
    ( bimap.forward_search(Watches, Watch, PathName) ->
        ( Name = "" ->
            MaybeName = no
        ;
            MaybeName = yes(Name)
        ),
        Event = inotify_event(Watch, PathName, Mask, Cookie, MaybeName),
        RevEvents = [Event | RevEvents0]
    ;
        % Should not happen?
        RevEvents = RevEvents0
    ).

:- pred remove_ignored(inotify_event(S)::in, watches(S)::in, watches(S)::out)
    is det.

remove_ignored(Event, !Watches) :-
    ( Event ^ mask /\ ignored \= 0 ->
        bimap.delete_key(Event ^ watch, !Watches)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

contains(Mask, Event) :-
    Mask /\ bit(Event) \= 0.

:- func bit(event_type) = int.

:- pragma foreign_proc("C",
    bit(X0::in) = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = X0;
").

:- func ignored = int.

:- pragma foreign_proc("C",
    ignored = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = IN_IGNORED;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
