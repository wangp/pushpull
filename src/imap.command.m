% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module imap.command.
:- interface.

:- type command
    --->    tag - command_in_state.

:- type command_in_state
    --->    command_any(command_any)
    ;       command_auth(command_auth)
    ;       command_nonauth(command_nonauth)
    ;       command_select(command_select).

    % Valid in all states.
:- type command_any
    --->    capability
    ;       logout
    ;       noop.

    % Valid only in Authenticated or Selected state.
:- type command_auth
    --->    append(mailbox, list(flag), maybe(date_time), binary_string)
    %;      create
    %;      delete
    ;       select(mailbox, list(select_param))
    ;       examine(mailbox, list(select_param))
    %;      list
    %;      lsub
    %;      rename
    %;      select
    %;      status
    %;      subscribe
    %;      unsubscribe.
    ;       idle.

    % Valid only when in Not Authenticated state.
:- type command_nonauth
    --->    login(
                astring,    % userid
                astring     % password
            )
    ;       authenticate(
                % This uses the [RFC 4959] "SASL-IR" extended AUTHENTICATE
                % command which takes a second argument.
                astring,    % authentication mechanism name
                astring     % initial client response
                            % Must be base64 encoded, or "=" to represent as
                            % zero-length string.
            ).
    %;       starttls.

    % Valid only when in Selected state.
:- type command_select
    --->    check
    ;       close
    ;       expunge
    %;      copy(sequence_set, mailbox)
    ;       fetch(sequence_set(message_seq_nr), fetch_items,
                maybe(fetch_modifier))
    ;       uid_fetch(sequence_set(uid), fetch_items,
                maybe(fetch_modifier))
    %;      store(sequence_set, ...)
    ;       uid_store(sequence_set(uid), store_operation, store_silence,
                list(flag))
    ;       search(search)
    ;       uid_search(search).

    % A crlf_wait in the command stream means the client must wait for a
    % continuation response from the server before continuing with the rest of
    % the stream.
:- type chunk
    --->    string(string)
    ;       binary(binary_string)
    ;       crlf_wait.

:- pred make_command_stream(command::in, list(chunk)::out) is det.

:- func idle_done_command_stream = list(chunk).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module string.

:- type acc == cord(chunk). % could probably be reverse list

:- type sp ---> sp.
:- type dquote ---> dquote.

:- typeclass add(T) where [
    pred add(T::in, acc::in, acc::out) is det
].

%-----------------------------------------------------------------------------%

make_command_stream(Command, List) :-
    add(Command, init, Acc),
    List = list(Acc).

idle_done_command_stream = [string("DONE")].

:- pred add_sp_sep_list(list(T)::in, acc::in, acc::out) is det <= add(T).

add_sp_sep_list([], !Acc).
add_sp_sep_list([X | Xs], !Acc) :-
    add(X, !Acc),
    list.foldl(add_sp_then, Xs, !Acc).

:- pred add_sp_then(T::in, acc::in, acc::out) is det <= add(T).

add_sp_then(X) -->
    add(sp),
    add(X).

:- instance add(maybe(T)) <= add(T) where [
    add(yes(X), !Acc) :- add(X, !Acc),
    add(no, !Acc)
].

:- instance add(chunk) where [
    add(X, A, snoc(A, X))
].

:- instance add(string) where [
    add(S, A, snoc(A, string(S)))
].

:- instance add(sp) where [
    add(sp) --> add(" ")
].

:- instance add(dquote) where [
    add(dquote) --> add("\"")
].

:- instance add(int) where [
    add(I) --> add(from_int(I))
].

:- instance add(integer) where [
    add(N) --> add(integer.to_string(N))
].

:- instance add(message_seq_nr) where [
    add(message_seq_nr(N)) --> add(N)
].

:- instance add(uid) where [
    add(uid(N)) --> add(N)
].

:- instance add(mod_seq_value) where [
    add(mod_seq_value(N)) --> add(N)
].

:- instance add(mod_seq_valzer) where [
    add(mod_seq_valzer(N)) --> add(N)
].

:- instance add(sequence_set(T)) <= add(T) where [
    add(SequenceSet, !Acc) :-
    (
        SequenceSet = cons(Elem, Elems),
        add(Elem, !Acc),
        add(",", !Acc),
        add(Elems, !Acc)
    ;
        SequenceSet = last(Elem),
        add(Elem, !Acc)
    )
].

:- instance add(seq_number(T)) <= add(T) where [
    add(number(X)) --> add(X),
    add(star) --> add("*")
].

:- instance add(sequence_set_element(T)) <= add(T) where [
    add(element(E)) --> add(E),
    add(range(L, R)) -->
    (
        add(L),
        add(":"),
        add(R)
    )
].

:- instance add(astring) where [
    add(astring(S)) --> add(S),
    add(imap_string(S)) --> add(S)
].

:- instance add(imap_string) where [
    add(String, !Acc) :-
    (
        String = quoted(Q),
        add(dquote, !Acc),
        add(escape_for_quoted_string(Q), !Acc),
        add(dquote, !Acc)
    ;
        String = literal(Binary),
        BraceCount = string.format("{%d}", [i(length(Binary))]),
        add(BraceCount, !Acc),
        add(crlf_wait, !Acc), % Need to wait for server response.
        add(binary(Binary), !Acc)
    )
].

:- func escape_for_quoted_string(string) = string.

escape_for_quoted_string(S0) = S :-
    string.replace_all(S0, "\\", "\\\\", S1),
    string.replace_all(S1, """", "\\\"", S).

:- instance add(command) where [
    add(Tag - Command) -->
    (
        add(Tag),
        add(sp),
        add(Command)
    )
].

:- instance add(tag) where [
    add(tag(Tag), !Acc) :- add(Tag, !Acc)
].

:- instance add(command_in_state) where [
    add(command_any(X)) --> add(X),
    add(command_auth(X)) --> add(X),
    add(command_nonauth(X)) --> add(X),
    add(command_select(X)) --> add(X)
].

:- instance add(command_any) where [
    add(capability) --> add("CAPABILITY"),
    add(logout) --> add("LOGOUT"),
    add(noop) --> add("NOOP")
].

:- instance add(command_auth) where [
    add(append(Mailbox, Flags, MaybeDateTime, Literal), !Acc) :-
    (
        add("APPEND", !Acc),
        add(sp, !Acc),
        add(Mailbox, !Acc),
        (
            Flags = []
        ;
            Flags = [_ | _],
            add(" (", !Acc),
            add_sp_sep_list(Flags, !Acc),
            add(")", !Acc)
        ),
        (
            MaybeDateTime = yes(DateTime),
            add(sp, !Acc),
            add(DateTime, !Acc)
        ;
            MaybeDateTime = no
        ),
        add(sp, !Acc),
        add(literal(Literal), !Acc)
    ),
    add(Command, !Acc) :-
    (
        (
            Command = select(Mailbox, Params),
            CommandString = "SELECT"
        ;
            Command = examine(Mailbox, Params),
            CommandString = "EXAMINE"
        ),
        add(CommandString, !Acc),
        add(sp, !Acc),
        add(Mailbox, !Acc),
        (
            Params = []
        ;
            Params = [_ | _],
            add(" (", !Acc),
            add_sp_sep_list(Params, !Acc),
            add(")", !Acc)
        )
    ),
    add(idle) -->
    (
        % RFC 2177
        add("IDLE")
    )
].

:- instance add(command_nonauth) where [
    add(login(UserId, Password)) -->
    (
        add("LOGIN"),
        add(sp),
        add(UserId),
        add(sp),
        add(Password)
    ),
    add(authenticate(AuthMechName, InitialClientResponse)) -->
    (
        add("AUTHENTICATE"),
        add(sp),
        add(AuthMechName),
        add(sp),
        add(InitialClientResponse)
    )
].

:- instance add(command_select) where [
    add(Command, !Acc) :-
    (
        Command = check,
        add("CHECK", !Acc)
    ;
        Command = close,
        add("CLOSE", !Acc)
    ;
        Command = expunge,
        add("EXPUNGE", !Acc)
    ;
        (
            Command = fetch(SequenceSet, Items, MaybeModifier),
            add("FETCH ", !Acc),
            add(SequenceSet, !Acc)
        ;
            Command = uid_fetch(SequenceSet, Items, MaybeModifier),
            add("UID FETCH ", !Acc),
            add(SequenceSet, !Acc)
        ),
        add(sp, !Acc),
        add(Items, !Acc),
        (
            MaybeModifier = yes(Modifier),
            add(" (", !Acc),
            add(Modifier, !Acc),
            add(")", !Acc)
        ;
            MaybeModifier = no
        )
    ;
        Command = uid_store(SequenceSet, Operation, Silence, Flags),
        add("UID STORE ", !Acc),
        add(SequenceSet, !Acc),
        add(sp, !Acc),
        (
            Operation = replace,
            add("FLAGS", !Acc)
        ;
            Operation = add,
            add("+FLAGS", !Acc)
        ;
            Operation = remove,
            add("-FLAGS", !Acc)
        ),
        (
            Silence = silent,
            add(".SILENT", !Acc)
        ;
            Silence = not_silent
        ),
        add(" (", !Acc),
        add_sp_sep_list(Flags, !Acc),
        add(")", !Acc)
    ;
        (
            Command = search(Search),
            add("SEARCH", !Acc)
        ;
            Command = uid_search(Search),
            add("UID SEARCH", !Acc)
        ),
        Search = search(MaybeCharset, SearchKey, MaybeResultOptions),
        (
            MaybeResultOptions = yes(ReturnOpts),
            % RFC 4731
            add(" RETURN (", !Acc),
            add_sp_sep_list(ReturnOpts, !Acc),
            add(")", !Acc)
        ;
            MaybeResultOptions = no
        ),
        (
            MaybeCharset = yes(CharSet),
            add(" CHARSET ", !Acc),
            add(CharSet, !Acc)
        ;
            MaybeCharset = no
        ),
        add(sp, !Acc),
        % Could raise the conjunction but don't need to.
        add(SearchKey, !Acc)
    )
].

:- instance add(mailbox) where [
    add(inbox) --> add("INBOX"),
    add(astring(S)) --> add(S)
].

:- instance add(select_param) where [
    add(condstore) --> add("CONDSTORE")
].

:- instance add(charset) where [
    add(charset(CharSet)) --> add(CharSet)
].

:- instance add(search_key) where [
    add(SearchKey, !Acc) :-
    (
        SearchKey = (all),
        add("ALL", !Acc)
    ;
        SearchKey = answered,
        add("ANSWERED", !Acc)
    ;
        SearchKey = bcc(X),
        add("BCC ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = body(X),
        add("BODY ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = cc(X),
        add("CC ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = deleted,
        add("DELETED", !Acc)
    ;
        SearchKey = flagged,
        add("FLAGGED", !Acc)
    ;
        SearchKey = from(X),
        add("FROM ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = new,
        add("NEW", !Acc)
    ;
        SearchKey = old,
        add("OLD", !Acc)
    ;
        SearchKey = recent,
        add("RECENT", !Acc)
    ;
        SearchKey = seen,
        add("SEEN", !Acc)
    ;
        SearchKey = subject(X),
        add("SUBJECT ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = text(X),
        add("TEXT ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = to(X),
        add("TO ", !Acc),
        add(X, !Acc)
    ;
        SearchKey = unanswered,
        add("UNANSWERED", !Acc)
    ;
        SearchKey = undeleted,
        add("UNDELETED", !Acc)
    ;
        SearchKey = unflagged,
        add("UNFLAGGED", !Acc)
    ;
        SearchKey = unseen,
        add("UNSEEN", !Acc)
    ;
        SearchKey = draft,
        add("DRAFT", !Acc)
    ;
        SearchKey = header(FieldName, String),
        add("HEADER ", !Acc),
        add(FieldName, !Acc),
        add(sp, !Acc),
        add(String, !Acc)
    ;
        SearchKey = not(Key),
        add("NOT ", !Acc),
        add(Key, !Acc)
    ;
        SearchKey = or(KeyA, KeyB),
        add("OR ", !Acc),
        add(KeyA, !Acc),
        add(sp, !Acc),
        add(KeyB, !Acc)
    ;
        SearchKey = and(Key, Keys),
        add("(", !Acc),
        add_sp_sep_list([Key | Keys], !Acc),
        add(")", !Acc)
    ;
        % RFC 4551
        SearchKey = modseq(ModSeqValzer),
        add("MODSEQ ", !Acc),
        add(ModSeqValzer, !Acc)
    )
].

:- instance add(search_return_option) where [
    add(min) --> add("MIN"),
    add(max) --> add("MAX"),
    add(all) --> add("ALL"),
    add(count) --> add("COUNT")
].

:- instance add(fetch_items) where [
    add(Items, !Acc) :-
    (
        Items = macro(all),
        add("ALL", !Acc)
    ;
        Items = macro(fast),
        add("FAST", !Acc)
    ;
        Items = macro(full),
        add("FULL", !Acc)
    ;
        Items = atts(Att, Atts),
        (
            Atts = [],
            add(Att, !Acc)
        ;
            Atts = [_ | _],
            add("(", !Acc),
            add_sp_sep_list([Att | Atts], !Acc),
            add(")", !Acc)
        )
    )
].

:- instance add(fetch_att) where [
    add(Att, !Acc) :-
    (
        Att = body,
        add("BODY", !Acc)
    ;
        Att = bodystructure,
        add("BODYSTRUCTURE", !Acc)
    ;
        Att = envelope,
        add("ENVELOPE", !Acc)
    ;
        Att = flags,
        add("FLAGS", !Acc)
    ;
        Att = internaldate,
        add("INTERNALDATE", !Acc)
    ;
        Att = rfc822,
        add("RFC822", !Acc)
    ;
        Att = rfc822_header,
        add("RFC822.HEADER", !Acc)
    ;
        Att = rfc822_size,
        add("RFC822.SIZE", !Acc)
    ;
        Att = rfc822_text,
        add("RFC822.TEXT", !Acc)
    ;
        Att = uid,
        add("UID", !Acc)
    ;
        (
            Att = body(SectionSpec, MaybePartial),
            add("BODY", !Acc)
        ;
            Att = body_peek(SectionSpec, MaybePartial),
            add("BODY.PEEK", !Acc)
        ),
        add("[", !Acc),
        add(SectionSpec, !Acc),
        add("]", !Acc),
        add(MaybePartial, !Acc)
    ;
        % RFC 4551
        Att = modseq,
        add("MODSEQ", !Acc)
    )
].

:- instance add(section) where [
    add(entire_message) --> [],
    add(msgtext(MsgText)) --> add(MsgText)
].

:- instance add(section_msgtext) where [
    add(header) -->
    (
        add("HEADER")
    ),
    add(header_fields(FieldName, FieldNames)) -->
    (
        add("HEADER.FIELDS ("),
        add(FieldName),
        add_sp_sep_list(FieldNames),
        add(")")
    ),
    add(header_fields_not(FieldName, FieldNames)) -->
    (
        add("HEADER.FIELDS.NOT ("),
        add(FieldName),
        add_sp_sep_list(FieldNames),
        add(")")
    ),
    add(text) -->
    (
        add("TEXT")
    )
].

:- instance add(partial) where [
    add(partial(Start, End)) -->
    (
        add("<"),
        add(Start),
        add("."),
        add(End),
        add(">")
    )
].

:- instance add(fetch_modifier) where [
    add(changedsince(ModSeqValue)) -->
    (
        add("CHANGEDSINCE "),
        add(ModSeqValue)
    )
].

:- instance add(flag) where [
    add(Flag) --> add(flag_to_imap_syntax(Flag))
].

:- instance add(atom) where [
    add(atom(Atom)) --> add(Atom)
].

:- instance add(date_time) where [
    add(DateTime, !Acc) :-
    (
        DateTime = date_time(Day, Month, Year,
            time(Hours, Minutes, Seconds), zone(Zone)),
        month(MonthStr, Month),
        string.format("\"%2d-%s-%04d %02d:%02d:%02d %s\"",
            [i(Day), s(MonthStr), i(Year),
            i(Hours), i(Minutes), i(Seconds), s(Zone)], Fmt),
        add(Fmt, !Acc)
    )
].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
