% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2022 Peter Wang

:- module prog_options.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module maybe.

:- type prog_options
    --->    prog_options(
                help :: bool,
                version :: bool,
                test_auth_only :: bool,
                allow_mass_delete :: maybe(int)
            ).

:- pred parse_options(list(string)::in, list(string)::out,
    maybe_error(prog_options)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       version
    ;       test_auth_only
    ;       allow_mass_delete.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("version", version).
long_option("test-auth-only", test_auth_only).
long_option("allow-mass-delete", allow_mass_delete).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help, bool(no)).
option_default(version, bool(no)).
option_default(test_auth_only, bool(no)).
option_default(allow_mass_delete, maybe_int(no)).

%-----------------------------------------------------------------------------%

parse_options(Args, NonOptionArgs, Res) :-
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args, NonOptionArgs, MaybeOptionTable),
    (
        MaybeOptionTable = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, help, Help),
        getopt.lookup_bool_option(OptionTable, version, Version),
        getopt.lookup_bool_option(OptionTable, test_auth_only, TestAuth),
        getopt.lookup_maybe_int_option(OptionTable, allow_mass_delete,
            AllowMassDelete),
        (
            AllowMassDelete = yes(AllowMassDeleteInt),
            AllowMassDeleteInt =< 0
        ->
            Res = error("option `--allow-mass-delete' requires " ++
                "a positive integer argument")
        ;
            Options = prog_options(Help, Version, TestAuth, AllowMassDelete),
            Res = ok(Options)
        )
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
