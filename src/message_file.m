%-----------------------------------------------------------------------------%

:- module message_file.
:- interface.

:- import_module io.

:- import_module binary_string.

:- type message_id
    --->    message_id(string).

:- type message(T)
    --->    message(binary_string).

:- type crlf        ---> crlf.
:- type lf          ---> lf.

:- type read_message_id_result
    --->    yes(message_id)
    ;       no
    ;       format_error(string)
    ;       error(string).

:- pred read_message_id_from_file(string::in, read_message_id_result::out,
    io::di, io::uo) is det.

    % The input string is a message, assumed to use LF or CRLF ending
    % respectively.
    %
:- pred read_message_id_from_message_lf(message(lf)::in,
    read_message_id_result::out) is det.
:- pred read_message_id_from_message_crlf(message(crlf)::in,
    read_message_id_result::out) is det.

:- func crlf_to_lf(message(crlf)) = message(lf).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module parsing_utils.
:- import_module string.
:- import_module unit.

:- import_module string_util.

:- type intermediate_result
    --->    ok
    ;       format_error(string)
    ;       error(string).

%-----------------------------------------------------------------------------%

read_message_id_from_file(FileName, Res, !IO) :-
    io.open_input(FileName, ResOpen, !IO),
    (
        ResOpen = ok(Stream),
        read_header_lines(Stream, Res0, [], RevLines, !IO),
        io.close_input(Stream, !IO),
        (
            Res0 = ok,
            list.reverse(RevLines, Lines),
            extract_message_id(Lines, no, Res)
        ;
            Res0 = format_error(Error),
            Res = format_error(Error)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ;
        ResOpen = error(Error),
        Res = error(io.error_message(Error))
    ).

:- pred read_header_lines(io.input_stream::in, intermediate_result::out,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

read_header_lines(Stream, Res, !RevLines, !IO) :-
    io.read_line_as_string(Stream, RLAS, !IO),
    (
        RLAS = ok(Line0),
        ( chop_crlf_or_lf(Line0, Line) ->
            ( Line = "" ->
                Res = ok
            ;
                cons(Line, !RevLines),
                read_header_lines(Stream, Res, !RevLines, !IO)
            )
        ;
            Res = format_error("incomplete line")
        )
    ;
        RLAS = eof,
        Res = ok
    ;
        RLAS = error(Error),
        Res = error(io.error_message(Error))
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

read_message_id_from_message_crlf(message(Input), Res) :-
    read_message_id_from_message(Input, from_string("\r\n"), Res).

read_message_id_from_message_lf(message(Input), Res) :-
    read_message_id_from_message(Input, from_string("\n"), Res).

:- pred read_message_id_from_message(binary_string::in, binary_string::in,
    read_message_id_result::out) is det.

read_message_id_from_message(Input, Delim, Res) :-
    read_header_lines_from_string(Input, Delim, 0, length(Input), Lines, Res0),
    (
        Res0 = ok,
        extract_message_id(Lines, no, Res)
    ;
        Res0 = format_error(Error),
        Res = format_error(Error)
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred read_header_lines_from_string(binary_string::in, binary_string::in,
    int::in, int::in, list(string)::out, intermediate_result::out) is det.

read_header_lines_from_string(Input, Delim, Pos0, EndPos, Lines, Res) :-
    ( Pos0 >= EndPos ->
        Lines = [],
        Res = ok
    ; binary_string.sub_string_search_start(Input, Delim, Pos0, Pos1) ->
        ( Pos1 = Pos0 ->
            % Blank line separates header and body.
            Lines = [],
            Res = ok
        ;
            Pos2 = Pos1 + length(Delim),
            % Ignore non-UTF-8 lines.
            ( string_between(Input, Pos0, Pos1, Line) ->
                Lines = [Line | Lines0],
                read_header_lines_from_string(Input, Delim, Pos2, EndPos,
                    Lines0, Res)
            ;
                read_header_lines_from_string(Input, Delim, Pos2, EndPos,
                    Lines, Res)
            )
        )
    ;
        Lines = [],
        Res = format_error("incomplete line")
    ).

%-----------------------------------------------------------------------------%

:- pred extract_message_id(list(string)::in, read_message_id_result::in,
    read_message_id_result::out) is det.

extract_message_id([], Res, Res).
extract_message_id([Line | Lines], Res0, Res) :-
    ( is_header_field(Line, Lines, RestLines, FieldName, FieldBody) ->
        ( strcase_equal(FieldName, "Message-Id") ->
            % If there are multiple Message-Id header fields then use the last
            % one. That seems to be what GMime and Dovecot do.
            ( parse_message_id_body(FieldBody, MessageId) ->
                extract_message_id(RestLines, yes(MessageId), Res)
            ;
                Res = format_error("bad Message-Id field body: "
                    ++ FieldBody)
            )
        ;
            extract_message_id(RestLines, Res0, Res)
        )
    ;
        Res = format_error("expected header field: " ++ Line)
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

%-----------------------------------------------------------------------------%

:- pred parse_message_id_body(string::in, message_id::out) is semidet.

parse_message_id_body(Input, MessageId) :-
    % We aren't too strict about this as we (mostly!) just treat the message-id
    % as an opaque string, and we have to cope with many malformed ids anyway.
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, no_skip_whitespace, msg_id, ParseResult)
    ),
    ParseResult = ok(MessageId).

:- pred msg_id(src::in, message_id::out, ps::in, ps::out) is semidet.

msg_id(Src, message_id(MessageId), !PS) :-
    skip_CFWS(Src, !PS),
    ( next_char(Src, '<', !PS) ->
        true
    ;
        % Tolerate invalid syntax.
        true
    ),
    current_offset(Src, Start, !PS),
    % We don't interpret internal CFWS or quotes or anything.
    until_end_of_msg_id(Src, !PS),
    % Ignore everything afterwards, such as a comment.
    current_offset(Src, End, !PS),
    input_substring(Src, Start, End, MessageId).

:- pred until_end_of_msg_id(src::in, ps::in, ps::out) is semidet.

until_end_of_msg_id(Src, !PS) :-
    (
        next_char(Src, C, !PS),
        C \= ('>'),
        C \= ('\r'),
        C \= ('\n')
    ->
        until_end_of_msg_id(Src, !PS)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

% Partially from rfc5322.m

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, !PS) :-
    semidet_true.

:- pred 'WSP'(char::in) is semidet.

'WSP'(' ').
'WSP'('\t').

:- pred skip_FWS(src::in, ps::in, ps::out) is det.

skip_FWS(Src, !PS) :-
    % We assume unfolding has already occurred.
    skip_WSP_chars(Src, !PS).

:- pred skip_WSP_chars(src::in, ps::in, ps::out) is det.

skip_WSP_chars(Src, !PS) :-
    (
        next_char(Src, C, !PS),
        'WSP'(C)
    ->
        skip_WSP_chars(Src, !PS)
    ;
        true
    ).

:- pred skip_CFWS(src::in, ps::in, ps::out) is det.

skip_CFWS(Src, !PS) :-
    skip_FWS(Src, !PS),
    ( skip_comment(Src, !PS) ->
        skip_CFWS(Src, !PS)
    ;
        true
    ).

:- pred skip_comment(src::in, ps::in, ps::out) is semidet.

skip_comment(Src, !PS) :-
    next_char(Src, '(', !PS),
    comment_tail(Src, !PS).

:- pred comment_tail(src::in, ps::in, ps::out) is semidet.

comment_tail(Src, !PS) :-
    next_char(Src, C, !PS),
    ( C = (')') ->
        % End of comment.
        true
    ;
        ( C = ('\\') ->
            next_char(Src, _, !PS)
        ; C = ('(') ->
            % Nested comment.
            comment_tail(Src, !PS)
        ;
            'WSP'(C)
        ;
            true
        ),
        comment_tail(Src, !PS)
    ).

%-----------------------------------------------------------------------------%

crlf_to_lf(message(BinaryStringCrLf)) = message(BinaryStringLf) :-
    CrLf = from_string("\r\n"),
    Lf = from_string("\n"),
    binary_string.replace_all(BinaryStringCrLf, CrLf, Lf, BinaryStringLf).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
