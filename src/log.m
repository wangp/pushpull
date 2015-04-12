% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module log.
:- interface.

:- import_module io.
:- import_module maybe.

:- type log.

:- type level
    --->    error
    ;       warning
    ;       notice
    ;       info
    ;       debug.

:- pred open_log(maybe(string)::in, level::in, maybe_error(log)::out,
    io::di, io::uo) is det.

:- pred close_log(log::in, io::di, io::uo) is det.

:- pred log(log::in, level::in, string::in, io::di, io::uo) is det.

:- pred log_error(log::in, string::in, io::di, io::uo) is det.
:- pred log_warning(log::in, string::in, io::di, io::uo) is det.
:- pred log_notice(log::in, string::in, io::di, io::uo) is det.
:- pred log_info(log::in, string::in, io::di, io::uo) is det.
:- pred log_debug(log::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module time.

:- type log
    --->    log(
                stderr  :: io.output_stream,
                file    :: maybe(io.output_stream),
                maxlvl  :: level
            ).

open_log(MaybeFileName, MaxLevel, Res, !IO) :-
    io.stderr_stream(Stderr, !IO),
    (
        MaybeFileName = yes(FileName),
        io.open_append(FileName, ResOpen, !IO),
        (
            ResOpen = ok(FileStream),
            Res = ok(log(Stderr, yes(FileStream), MaxLevel))
        ;
            ResOpen = error(Error),
            Res = error(io.error_message(Error))
        )
    ;
        MaybeFileName = no,
        Res = ok(log(Stderr, no, MaxLevel))
    ).

close_log(log(_Stderr, MaybeFileStream, _MinLevel), !IO) :-
    (
        MaybeFileStream = yes(FileStream),
        io.close_output(FileStream, !IO)
    ;
        MaybeFileStream = no
    ).

log(log(Stderr, MaybeFileStream, MaxLevel), Level, Message, !IO) :-
    ( to_int(Level) =< to_int(MaxLevel) ->
        time(Time, !IO),
        Timestamp = timestamp(localtime(Time)),
        do_log(Stderr, Level, Timestamp, Message, !IO),
        (
            MaybeFileStream = yes(FileStream),
            do_log(FileStream, Level, Timestamp, Message, !IO),
            io.flush_output(FileStream, !IO)
        ;
            MaybeFileStream = no
        )
    ;
        true
    ).

:- pred do_log(io.output_stream::in, level::in, string::in, string::in,
    io::di, io::uo) is det.

do_log(Stream, Level, Timestamp, Message, !IO) :-
    % XXX may want to avoid throwing exceptions
    io.write_string(Stream, Timestamp, !IO),
    io.write_string(Stream, to_string(Level), !IO),
    io.write_string(Stream, Message, !IO),
    ( string.suffix(Message, "\n") ->
        true
    ;
        io.nl(Stream, !IO)
    ).

log_error(Log, Message, !IO) :-
    log(Log, error, Message, !IO).

log_warning(Log, Message, !IO) :-
    log(Log, warning, Message, !IO).

log_notice(Log, Message, !IO) :-
    log(Log, notice, Message, !IO).

log_info(Log, Message, !IO) :-
    log(Log, info, Message, !IO).

log_debug(Log, Message, !IO) :-
    log(Log, debug, Message, !IO).

:- func to_int(level) = int.

to_int(error) = 0.
to_int(warning) = 1.
to_int(notice) = 2.
to_int(info) = 3.
to_int(debug) = 4.

:- func to_string(level) = string.

to_string(error) = "Error: ".
to_string(warning) = "Warning: ".
to_string(notice) = "".
to_string(info) = "".
to_string(debug) = "".

:- func timestamp(tm) = string.

timestamp(TM) = Timestamp :-
    Year = TM ^ tm_year + 1900,
    Month = TM ^ tm_mon + 1,
    MonthDay = TM ^ tm_mday,
    Hour = TM ^ tm_hour,
    Min = TM ^ tm_min,
    Sec = TM ^ tm_sec,
    Timestamp = string.format("%04d-%02d-%02d %02d:%02d:%02d - ",
        [i(Year), i(Month), i(MonthDay), i(Hour), i(Min), i(Sec)]).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
