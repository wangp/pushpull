%-----------------------------------------------------------------------------%

:- module prog_config.
:- interface.

:- import_module bool.
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
                mailbox     :: mailbox,
                idle_timeout_secs :: int,
                sync_on_idle_timeout :: bool
            ).

:- type maildir_root
    --->    maildir_root(string).

:- type local_mailbox_name
    --->    local_mailbox_name(string).

:- type local_mailbox_path
    --->    local_mailbox_path(string).

:- func make_local_mailbox_path(prog_config, local_mailbox_name)
    = local_mailbox_path.

:- func max_idle_timeout_secs = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module int.

make_local_mailbox_path(Config, local_mailbox_name(MailboxName)) = Path :-
    MaildirRoot = Config ^ maildir_root,
    MaildirRoot = maildir_root(DirName),
    Path = local_mailbox_path(DirName / MailboxName).

max_idle_timeout_secs = 29 * 60.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
