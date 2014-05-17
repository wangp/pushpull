%-----------------------------------------------------------------------------%

:- module prog_config.
:- interface.

:- import_module imap.
:- import_module imap.types.

:- type prog_config
    --->    prog_config(
                db_filename :: string,
                maildir     :: maildir,
                hostport    :: string,
                username    :: username,
                password    :: password,
                mailbox     :: mailbox
            ).

:- type maildir
    --->    maildir(string).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
