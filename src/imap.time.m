% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module imap.time.
:- interface.

:- import_module time.

:- func from_date_time(date_time) = time_t.

:- func make_date_time(time_t) = date_time.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module require.

:- pragma foreign_decl("C", local, "
    #include <time.h>
").

%-----------------------------------------------------------------------------%

from_date_time(DateTime) = TimeT :-
    DateTime = date_time(Day, Month, Year, time(Hours, Minutes, Seconds), Zone),

    YearSince1900 = Year - 1900,
    month_from_zero(Month, MonthFromZero),
    MonthDayFromOne = Day,
    ( parse_zone(Zone, GMTOff0) ->
        GMTOff = GMTOff0
    ;
        unexpected($module, $pred, "parse_zone failed")
    ),

    TimeT = tm2time(YearSince1900, MonthFromZero, MonthDayFromOne,
        Hours, Minutes, Seconds, GMTOff).

:- pred parse_zone(zone::in, int::out) is semidet.

parse_zone(zone(Zone), GMTOff) :-
    string.to_char_list(Zone, [PlusMinus, D1, D2, D3, D4]),
    char.decimal_digit_to_int(D1, H1),
    char.decimal_digit_to_int(D2, H2),
    char.decimal_digit_to_int(D3, M1),
    char.decimal_digit_to_int(D4, M2),
    HH = (H1 * 10) + H2,
    MM = (M1 * 10) + M2,
    Secs = (HH * 60 * 60) + (MM * 60),
    (
        PlusMinus = ('+'),
        GMTOff = Secs
    ;
        PlusMinus = ('-'),
        GMTOff = -Secs
    ).

:- func tm2time(int, int, int, int, int, int, int) = time_t.

:- pragma foreign_proc("C",
    tm2time(YearSince1900::in, MonthFromZero::in, MonthDayFromOne::in,
        Hours::in, Minutes::in, Seconds::in, GMTOff::in) = (TimeT::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    struct tm tm;

    tm.tm_sec = Seconds;
    tm.tm_min = Minutes;
    tm.tm_hour = Hours;
    tm.tm_mon = MonthFromZero;
    tm.tm_year = YearSince1900;
    tm.tm_wday = -1;
    tm.tm_mday = MonthDayFromOne;
    tm.tm_yday = -1;
    tm.tm_isdst = -1;

    /*
    ** mktime() does not take tm.tm_gmtoff into account even when available,
    ** i.e. it always assumes its input is in the local time zone.  Therefore
    ** we use the timegm() function then adjust for the time zone manually.
    */

    TimeT = timegm(&tm) - GMTOff;
").

%-----------------------------------------------------------------------------%

make_date_time(Time) = DateTime :-
    TM = time.localtime(Time),
    TM = tm(YearSince1900, MonthFromZero, MonthDayFromOne,
            Hours, Minutes, Seconds, _YearDay, _WeekDay, _IsDst),

    Year = 1900 + YearSince1900,
    ( month_from_zero(MonthPrime, MonthFromZero) ->
        Month = MonthPrime
    ;
        unexpected($module, $pred, "month_from_zero failed")
    ),
    MonthDayFromOne = Day,
    Zone = make_zone(get_gmtoff(Time)),

    DateTime = date_time(Day, Month, Year, time(Hours, Minutes, Seconds), Zone).

:- func get_gmtoff(time_t) = int.

:- pragma foreign_proc("C",
    get_gmtoff(Time::in) = (GMTOff::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    time_t t;
    struct tm tm;

    t = Time;
    localtime_r(&t, &tm);
    GMTOff = tm.tm_gmtoff; /* GNU extension */
").

:- func make_zone(int) = zone.

make_zone(GMTOff) = zone(Zone) :-
    ( GMTOff >= 0 ->
        EastWest = ('+'),
        Seconds = GMTOff
    ;
        EastWest = ('-'),
        Seconds = -GMTOff
    ),
    HH = Seconds / 3600,
    MM = mod(Seconds, 3600) / 60,
    Zone = string.format("%c%02d%02d", [c(EastWest), i(HH), i(MM)]).

%-----------------------------------------------------------------------------%

:- pred month_from_zero(month, int).
:- mode month_from_zero(in, out) is det.
:- mode month_from_zero(out, in) is semidet.

month_from_zero(jan, 0).
month_from_zero(feb, 1).
month_from_zero(mar, 2).
month_from_zero(apr, 3).
month_from_zero(may, 4).
month_from_zero(jun, 5).
month_from_zero(jul, 6).
month_from_zero(aug, 7).
month_from_zero(sep, 8).
month_from_zero(oct, 9).
month_from_zero(nov, 10).
month_from_zero(dec, 11).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
