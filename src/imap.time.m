%-----------------------------------------------------------------------------%

:- module imap.time.
:- interface.

:- import_module time.

:- func mktime(date_time) = time_t.

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

mktime(DateTime) = TimeT :-
    DateTime = date_time(Day, Month, Year, time(Hours, Minutes, Seconds), Zone),

    YearSince1900 = Year - 1900,
    month_from_zero(Month, MonthFromZero),
    MonthDayFromOne = Day,
    ( parse_zone(Zone, GMTOff0) ->
        GMTOff = GMTOff0
    ;
        unexpected($module, $pred, "parse_zone failed")
    ),

    % We cannot use the Mercury time module as it does not expose the tm_gmtoff
    % extension.
    TimeT = mktime(YearSince1900, MonthFromZero, MonthDayFromOne,
        Hours, Minutes, Seconds, GMTOff).

:- pred month_from_zero(month::in, int::out) is det.

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

:- pred parse_zone(zone::in, int::out) is semidet.

parse_zone(zone(Zone), GMTOff) :-
    string.to_char_list(Zone, [PlusMinus, D1, D2, D3, D4]),
    char.digit_to_int(D1, H1),
    char.digit_to_int(D2, H2),
    char.digit_to_int(D3, M1),
    char.digit_to_int(D4, M2),
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

:- func mktime(int, int, int, int, int, int, int) = time_t.

:- pragma foreign_proc("C",
    mktime(YearSince1900::in, MonthFromZero::in, MonthDayFromOne::in,
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
    tm.tm_gmtoff = GMTOff; /* GNU/BSD extension */

    TimeT = mktime(&tm);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
