%-----------------------------------------------------------------------------%

:- module message_file.
:- interface.

:- import_module io.

:- type read_message_id_result
    --->    yes(string)
    ;       no
    ;       format_error(string)
    ;       error(io.error).

:- pred read_message_id(string::in, read_message_id_result::out,
    io::di, io::uo) is det.

    % The input string is assumed to use LF endings.
    %
:- pred read_message_id_from_string(string::in, read_message_id_result::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module string_util.

:- type message_id == string.

%-----------------------------------------------------------------------------%

read_message_id(FileName, Res, !IO) :-
    io.open_input(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        read_header_lines(Stream, [], ResRead, !IO),
        io.close_input(Stream, !IO),
        (
            ResRead = ok(Lines),
            extract_message_id(Lines, no, Res)
        ;
            ResRead = eof,
            Res = format_error("missing header and body separator line")
        ;
            ResRead = error(Error),
            Res = error(Error)
        )
    ;
        ResOpen = error(Error),
        Res = error(Error)
    ).

:- pred read_header_lines(io.input_stream::in, list(string)::in,
    io.result(list(string))::out, io::di, io::uo) is det.

read_header_lines(Stream, RevLines0, Res, !IO) :-
    io.read_line_as_string(Stream, RLAS, !IO),
    (
        RLAS = ok(Line0),
        ( chop_crlf_or_lf(Line0, Line) ->
            ( Line = "" ->
                Res = ok(reverse(RevLines0))
            ;
                read_header_lines(Stream, [Line | RevLines0], Res, !IO)
            )
        ;
            Res = error(io.make_io_error("incomplete line"))
        )
    ;
        RLAS = eof,
        Res = eof
    ;
        RLAS = error(Error),
        Res = error(Error)
    ).

:- pred chop_crlf_or_lf(string::in, string::out) is semidet.

chop_crlf_or_lf(Line0, Line) :-
    ( string.remove_suffix(Line0, "\r\n", Line1) ->
        Line = Line1
    ;
        % Don't know if we really need this.
        string.remove_suffix(Line0, "\n", Line1),
        Line = Line1
    ).

%-----------------------------------------------------------------------------%

read_message_id_from_string(Input, Res) :-
    ( string.sub_string_search(Input, "\n\n", HeaderEndPos) ->
        string.unsafe_between(Input, 0, HeaderEndPos, Header),
        Lines = string.split_at_string("\n", Header),
        extract_message_id(Lines, no, Res)
    ;
        Res = format_error("missing header and body separator line")
    ).

%-----------------------------------------------------------------------------%

:- pred extract_message_id(list(string)::in, maybe(message_id)::in,
    read_message_id_result::out) is det.

extract_message_id([], MaybeMessageId, Res) :-
    (
        MaybeMessageId = yes(MessageId),
        Res = yes(MessageId)
    ;
        MaybeMessageId = no,
        Res = no
    ).

extract_message_id([Line | Lines], MaybeMessageId0, Res) :-
    ( is_header_field(Line, Lines, RestLines, FieldName, FieldBody) ->
        ( strcase_equal(FieldName, "Message-Id") ->
            (
                MaybeMessageId0 = yes(_),
                Res = format_error("multiple Message-Id header fields")
            ;
                MaybeMessageId0 = no,
                ( parse_message_id_body(FieldBody, MessageId) ->
                    extract_message_id(Lines, yes(MessageId), Res)
                ;
                    Res = format_error("bad Message-Id field body")
                )
            )
        ;
            extract_message_id(RestLines, MaybeMessageId0, Res)
        )
    ;
        Res = format_error("expected header field")
    ).

:- pred is_header_field(string::in, list(string)::in, list(string)::out,
    string::out, string::out) is semidet.

is_header_field(Line, Lines, RestLines, FieldName, FieldBody) :-
    string.sub_string_search(Line, ":", Colon),
    require_det (
        End = string.count_code_units(Line),
        string.unsafe_between(Line, 0, Colon, Name0),
        string.unsafe_between(Line, Colon + 1, End, Body0),
        FieldName = string.rstrip(Name0),
        Body1 = string.lstrip(Body0),
        takewhile(folded_line, Lines, BodyFolded, RestLines),
        string.append_list([Body1 | BodyFolded], FieldBody)
    ).

:- pred folded_line(string::in) is semidet.

folded_line(Line) :-
    string.index(Line, 0, Char),
    'WSP'(Char).

:- pred 'WSP'(char::in) is semidet.

'WSP'(' ').
'WSP'('\t').

%-----------------------------------------------------------------------------%

:- pred parse_message_id_body(string::in, message_id::out) is semidet.

parse_message_id_body(Input, MessageId) :-
    % We don't need to be too strict about this as we just treat the message-id
    % as an opaque string.
    some [!Pos] (
        Start = 0,
        !:Pos = Start,
        unsafe_index_next(Input, !Pos, ('<')),
        advance_while(plausible_msg_id_char, Input, !Pos),
        unsafe_index_next(Input, !Pos, ('@')),
        advance_while(plausible_msg_id_char, Input, !Pos),
        unsafe_index_next(Input, !Pos, ('>')),
        End = !.Pos,
        advance_while('WSP', Input, !Pos),
        not unsafe_index_next(Input, !.Pos, _, _), % eof

        End >= Start + 5,
        string.unsafe_between(Input, Start, End, MessageId)
    ).

:- pred plausible_msg_id_char(char::in) is semidet.

plausible_msg_id_char(C) :-
    char.to_int(C, I),
    % Printable US-ASCII plus SP.
    0x20 =< I, I < 0x7f,
    % XXX these could be escaped in quoted-strings so we might need proper
    % parsing after all
    C \= ('@'),
    C \= ('<'),
    C \= ('>').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
