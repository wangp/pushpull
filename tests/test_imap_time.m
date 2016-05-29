%-----------------------------------------------------------------------------%

:- module test_imap_time.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module time.

:- import_module imap.
:- import_module imap.time.
:- import_module imap.types.

%-----------------------------------------------------------------------------%

main(!IO) :-
    test(date_time(29, may, 2016, time(13, 51, 46), zone("+0930")), !IO),
    test(date_time(29, may, 2016, time(13, 51, 46), zone("-0700")), !IO).

:- pred test(date_time::in, io::di, io::uo) is det.

test(DateTime, !IO) :-
    io.write(DateTime, !IO),
    io.nl(!IO),

    Time = from_date_time(DateTime),
    io.write(time_to_float(Time), !IO),
    io.nl(!IO),

    local_date_time(Time, LocalDateTime, !IO),
    io.write(LocalDateTime, !IO),
    io.nl(!IO),

    write_string("--------\n", !IO).

:- func time_to_float(time_t) = float.

:- pragma foreign_proc("C",
    time_to_float(Time::in) = (Float::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /*
    ** XXX depends on internal representation of Mercury `time_t' but so do
    ** other parts of the program.  That's another reason we need this test.
    */
    Float = Time;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
