% Bower - a frontend for the Notmuch email system
% Copyright (C) 2012 Peter Wang

:- module path_expand.
:- interface.

:- import_module io.
:- import_module list.

:- import_module shell_word.

:- type home
    --->    home(string).

:- pred get_home_dir(home::out, io::di, io::uo) is det.

:- pred expand_tilde_home(home::in, string::in, string::out) is det.

:- pred expand_tilde_home_in_shell_words(home::in,
    list(word)::in, list(word)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

get_home_dir(home(Home), !IO) :-
    io.get_environment_var("HOME", MaybeHome, !IO),
    (
        MaybeHome = yes(Home)
    ;
        MaybeHome = no,
        % Not really, but not reachable in practice.
        Home = "~"
    ).

expand_tilde_home(home(HomeDir), String0, String) :-
    (
        string.remove_prefix("~", String0, String1),
        (
            string.unsafe_index_next(String1, 0, _, NextChar)
        =>
            NextChar = ('/')
        )
    ->
        String = HomeDir ++ "/" ++ String1
    ;
        String = String0
    ).

%-----------------------------------------------------------------------------%

expand_tilde_home_in_shell_words(Home, Words0, Words) :-
    list.map(expand_tilde_home_in_word(Home), Words0, Words).

:- pred expand_tilde_home_in_word(home::in, word::in, word::out) is det.

expand_tilde_home_in_word(Home, Word0, Word) :-
    Word0 = word(Segments0),
    (
        Segments0 = [],
        Segments = []
    ;
        Segments0 = [Head0 | Tail],
        expand_tilde_home_in_word_segment(Home, Head0, Head),
        Segments = [Head | Tail]
    ),
    Word = word(Segments).

:- pred expand_tilde_home_in_word_segment(home::in, segment::in, segment::out)
    is det.

expand_tilde_home_in_word_segment(Home, Segment0, Segment) :-
    (
        Segment0 = unquoted(Str0),
        expand_tilde_home(Home, Str0, Str),
        Segment = unquoted(Str)
    ;
        Segment0 = quoted(_),
        Segment = Segment0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
