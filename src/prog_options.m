% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module prog_options.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module maybe.

:- type prog_options
    --->    prog_options(
                test_auth_only :: bool
            ).

:- pred parse_options(list(string)::in, list(string)::out,
    maybe_error(prog_options)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module getopt.

%-----------------------------------------------------------------------------%

:- type option
    --->    test_auth_only.

:- pred short_option(char::in, option::out) is semidet.

short_option(_, test_auth_only) :-
    semidet_fail.

:- pred long_option(string::in, option::out) is semidet.

long_option("test-auth-only", test_auth_only).

:- pred option_default(option::out, option_data::out) is multi.

option_default(test_auth_only, bool(no)).

%-----------------------------------------------------------------------------%

parse_options(Args, NonOptionArgs, Res) :-
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args, NonOptionArgs, MaybeOptionTable),
    (
        MaybeOptionTable = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, test_auth_only, TestAuth),
        Options = prog_options(TestAuth),
        Res = ok(Options)
    ;
        MaybeOptionTable = error(OptionError),
        Res = error(option_error_to_string(OptionError))
    ).

    % For compatibility with older getopt.process_options predicate
    % which returned an error as a string instead of option_error.
    %
:- func option_error_to_string(string) = string.
:- pragma consider_used(option_error_to_string/1).

option_error_to_string(S) = S.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
