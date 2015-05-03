plugsink
========

plugsink is a bidirectional IMAP/Maildir synchronisation tool.
It synchronises one IMAP folder with one Maildir folder in a single run.
I wrote it for me, but you might like it, too.

**WARNING**: Use at your own risk.

I use plugsink to synchronise my GMail account, and during development
usually test against a Dovecot server in a virtual machine.
Beyond that, it has not received any testing with any other IMAP server.
Do your own testing and read the source code.


Requirements
============

  * Linux system (the inotify API is used, so other Unix-like systems will
    require some porting effort)

  * Mercury compiler (release-of-the-day since about 2015-03xx); available
    at <http://dl.mercurylang.org/index.html>

  * OpenSSL


Compiling
=========

With Mercury installed and `mmc` in your PATH, run:

    make PARALLEL=-j6

If successful, you will get a binary named `plugsink`.


Invocation
==========

Run it like this:

    plugsink *config-file* *pairing*

The configuration file can be placed anywhere you like.
See `plugsink.conf.sample` for the details.

The *pairing* argument names the pair of Maildir folder and IMAP folder to
be synchronised.  For example, if the configuration file contains:

    [pairing All Mail]
    local = All Mail
    remote = [Google Mail]/All Mail

then running the command `plugsink config-file "All Mail"` will synchronise
the IMAP folder "[Google Mail]/All Mail" with the local folder `All Mail`
under the top-level Maildir directory.


Idling
======

After the initial synchronisation cycle, plugsink can maintain an open
connection to the IMAP server if the configuration key is set:

    imap.idle = yes

Once a change is made to the Maildir folder, or a change is made to the
IMAP folder, a new synchronisation cycle will be performed.

Send a `SIGINT` signal (Ctrl-C) to the process to tell it to quit.

Send a `SIGHUP` signal to the process to start a new synchronisation
cycle immediately.


What is synchronised
====================

New messages detected on one side since the last synchronisation cycle will
be copied to the other side, and vice versa.

Flag additions or removals on one side are applied to corresponding messages
on the other side, and vice versa.  The standard Maildir flags are mapped to
the IMAP system flags:

    R       \Answered
    F       \Flagged
    T       \Deleted
    S       \Seen
    D       \Draft

(Other IMAP flags are recorded in the database but not otherwise visible as
there is no standard way to express them in Maildir.)

plugsink does not permanently delete messages from the Maildir folder.
If a message in the IMAP folder is deleted then the corresponding Maildir
message will be marked as deleted -- the message gains the `T` flag.
If you access the Maildir through a mail client and use its "expunge"
function then it will probably permanently delete the messages so marked.

plugsink does not issue the EXPUNGE command to permanently delete messages
from the IMAP folder.  If a message in the Maildir folder is deleted then
the corresponding message in the IMAP folder will only be marked as deleted
-- the message gains the `\Deleted` flag.  If you access the IMAP folder
through a mail client and use its "expunge" function (or similar) then it
will permanently delete the messages so marked.

Note: GMail has an Auto-Expunge option.  When enabled (the default), a
message will be removed from the IMAP folder as soon as it is marked
deleted.  The message should still be available in another folder (label)
such as "All Mail" or "Trash".  I have this option enabled and it seems
okay.


Warning
=======

Be aware of what is known to the state database.  For now, this requires
poking around the state database with SQLite commands.

This is a bad sequence of events:

 1. you synchronise a Maildir folder `Foo` with an IMAP folder `RemoteFoo`
 2. you rename `Foo` to `Bar`
 3. you create a empty folder `Foo`, and synchronise the new `Foo` with
    `RemoteFoo` again

The user's intention may have been to re-download all the `RemoteFoo`
messages into `Foo`.  Instead, plugsink thinks that all the messages it
knew about from `Foo` have been deleted, and proceeds to mark all the
corresponding messages in `RemoteFoo` as deleted.

In the future we may try to prevent such bad behaviour, somehow.


Synchronisation algorithm
=========================

To be documented.


Debugging
=========

Set the environment variable `DEBUG_IMAP=1` to trace IMAP commands and
responses.


Author
======

Peter Wang <novalazy@gmail.com>

