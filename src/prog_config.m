%-----------------------------------------------------------------------------%

:- module prog_config.
:- interface.

:- import_module imap.
:- import_module imap.types.

:- type prog_config
    --->    prog_config(
                db_filename :: string,
                maildir_root:: maildir_root,
                local_mailbox_name :: local_mailbox_name,
                hostport    :: string,
                username    :: username,
                password    :: password,
                mailbox     :: mailbox
            ).

:- type maildir_root
    --->    maildir_root(string).

:- type local_mailbox_name
    --->    local_mailbox_name(string).

:- type local_mailbox_path
    --->    local_mailbox_path(string).

:- func make_local_mailbox_path(prog_config, local_mailbox_name)
    = local_mailbox_path.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.

make_local_mailbox_path(Config, local_mailbox_name(MailboxName)) = Path :-
    MaildirRoot = Config ^ maildir_root,
    MaildirRoot = maildir_root(DirName),
    Path = local_mailbox_path(DirName / MailboxName).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
