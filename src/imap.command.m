%-----------------------------------------------------------------------------%

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
    %--->   append
    %;      create
    %;      delete
    --->    examine(command.mailbox).
    %;      list
    %;      lsub
    %;      rename
    %;      select
    %;      status
    %;      subscribe
    %;      unsubscribe.

    % Valid only when in Not Authenticated state.
:- type command_nonauth
    --->    login(astring, astring). % userid, password
    %;       authenticate
    %;       starttls.

    % Valid only when in Selected state.
:- type command_select
    --->    check
    ;       close
    ;       expunge
    %;      copy(sequence_set, mailbox)
    ;       fetch(sequence_set(message_seq_nr), fetch_items)
    ;       uid_fetch(sequence_set(uid), fetch_items)
    %;      store(sequence_set, ...)
    ;       search(search)
    ;       uid_search(search).

:- type mailbox
    --->    inbox
    ;       astring(astring). % not INBOX (case-insensitive)

:- func crlf = string.

    % A crlf in the command stream means the client must wait for a
    % continuation response from the server before continuing with the rest of
    % the stream.  The final crlf that terminates the command is NOT in the
    % command stream.
    %
:- pred make_command_stream(command::in, list(string)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module require.
:- import_module string.

:- type acc == cord(string). % could probably be reverse list

:- type sp ---> sp.
:- type dquote ---> dquote.

:- typeclass add(T) where [
    pred add(T::in, acc::in, acc::out) is det
].

%-----------------------------------------------------------------------------%

crlf = "\r\n".

make_command_stream(Command, List) :-
    add(Command, init, Acc),
    List = list(Acc).

:- pred add_sp_then(T::in, acc::in, acc::out) is det <= add(T).

add_sp_then(X, !Acc) :-
    add(sp, !Acc),
    add(X, !Acc).

:- pred add_comma_then(T::in, acc::in, acc::out) is det <= add(T).

add_comma_then(X, !Acc) :-
    add(",", !Acc),
    add(X, !Acc).

:- instance add(maybe(T)) <= add(T) where [
    add(yes(X), !Acc) :- add(X, !Acc),
    add(no, !Acc)
].

:- instance add(string) where [
    add(S, A, snoc(A, S))
].

:- instance add(sp) where [
    add(sp, !Acc) :- add(" ", !Acc)
].

:- instance add(dquote) where [
    add(dquote, !Acc) :- add("\"", !Acc)
].

:- instance add(int) where [
    add(I, !Acc) :- add(from_int(I), !Acc)
].

:- instance add(integer) where [
    add(N, !Acc) :- add(integer.to_string(N), !Acc)
].

:- instance add(message_seq_nr) where [
    add(message_seq_nr(N), !Acc) :- add(N, !Acc)
].

:- instance add(uid) where [
    add(uid(N), !Acc) :- add(N, !Acc)
].

:- instance add(mod_seq_valzer) where [
    add(mod_seq_valzer(N), !Acc) :- add(N, !Acc)
].

:- instance add(sequence_set(T)) <= add(T) where [
    add(SequenceSet, !Acc) :-
    (
        SequenceSet = number(First, Rest),
        add(First, !Acc),
        list.foldl(add_comma_then, Rest, !Acc)
    ;
        SequenceSet = range(First, Rest),
        add(First, !Acc),
        list.foldl(add_comma_then, Rest, !Acc)
    )
].

:- instance add(seq_number(T)) <= add(T) where [
    add(number(X), !Acc) :- add(X, !Acc),
    add(star, !Acc) :- add("*", !Acc)
].

:- instance add(seq_range(T)) <= add(T) where [
    add(seq_range(L, R), !Acc) :-
    (
        add(L, !Acc),
        add(":", !Acc),
        add(R, !Acc)
    )
].

:- instance add(astring) where [
    add(AString, !Acc) :-
    (
        AString = astring(S),
        add(S, !Acc)
    ;
        AString = imap_string(IString),
        add(IString, !Acc)
    )
].

:- instance add(imap_string) where [
    add(String, !Acc) :-
    (
        String = quoted(Q),
        add(dquote, !Acc),
        add(escape_for_quoted_string(Q), !Acc),
        add(dquote, !Acc)
    ;
        String = literal(L),
        BraceCount = string.format("{%d}", [i(count_code_units(L))]),
        add(BraceCount, !Acc),
        add(crlf, !Acc), % Need to wait for server response.
        add(L, !Acc)
    )
].

:- func escape_for_quoted_string(string) = string.

escape_for_quoted_string(S0) = S :-
    string.replace_all(S0, "\\", "\\\\", S1),
    string.replace_all(S1, """", "\\\"", S).

:- instance add(command) where [
    add(Tag - Command, !Acc) :-
    (
        add(Tag, !Acc),
        add(sp, !Acc),
        add(Command, !Acc)
    )
].

:- instance add(tag) where [
    add(tag(Tag), !Acc) :- add(Tag, !Acc)
].

:- instance add(command_in_state) where [
    add(Command, !Acc) :-
    (
        Command = command_any(X),
        add(X, !Acc)
    ;
        Command = command_auth(X),
        add(X, !Acc)
    ;
        Command = command_nonauth(X),
        add(X, !Acc)
    ;
        Command = command_select(X),
        add(X, !Acc)
    )
].

:- instance add(command_any) where [
    add(capability, !Acc) :-
    (
        add("CAPABILITY", !Acc)
    ),
    add(logout, !Acc) :-
    (
        add("LOGOUT", !Acc)
    ),
    add(noop, !Acc) :-
    (
        add("NOOP", !Acc)
    )
].

:- instance add(command_auth) where [
    add(examine(Mailbox), !Acc) :-
    (
        add("EXAMINE", !Acc),
        add(sp, !Acc),
        add(Mailbox, !Acc)
    )
].

:- instance add(command_nonauth) where [
    add(login(UserId, Password), !Acc) :-
    (
        add("LOGIN", !Acc),
        add(sp, !Acc),
        add(UserId, !Acc),
        add(sp, !Acc),
        add(Password, !Acc)
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
            Command = fetch(SequenceSet, Items),
            add("FETCH ", !Acc),
            add(SequenceSet, !Acc)
        ;
            Command = uid_fetch(SequenceSet, Items),
            add("UID FETCH ", !Acc),
            add(SequenceSet, !Acc)
        ),
        add(sp, !Acc),
        add(Items, !Acc)
    ;
        (
            Command = search(search(MaybeCharset, SearchKey)),
            add("SEARCH", !Acc)
        ;
            Command = uid_search(search(MaybeCharset, SearchKey)),
            add("UID SEARCH", !Acc)
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

:- instance add(command.mailbox) where [
    add(Mailbox, !Acc) :-
    (
        Mailbox = inbox,
        add("INBOX", !Acc)
    ;
        Mailbox = astring(S),
        add(S, !Acc)
    )
].

:- instance add(charset) where [
    add(charset(CharSet), !Acc) :-
        add(CharSet, !Acc)
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
        SearchKey = and(KeyA, Keys),
        add("(", !Acc),
        add(KeyA, !Acc),
        list.foldl(add_sp_then, Keys, !Acc),
        add(")", !Acc)
    ;
        % RFC 4551
        SearchKey = modseq(ModSeqValzer),
        add("MODSEQ ", !Acc),
        add(ModSeqValzer, !Acc)
    )
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
            add(Att, !Acc),
            list.foldl(add_sp_then, Atts, !Acc),
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

:- instance add(section_spec) where [
    add(msgtext(MsgText), !Acc) :-
        add(MsgText, !Acc)
].

:- instance add(section_msgtext) where [
    add(MsgText, !Acc) :-
    (
        MsgText = header,
        add("HEADER", !Acc)
    ;
        MsgText = text,
        add("TEXT", !Acc)
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
