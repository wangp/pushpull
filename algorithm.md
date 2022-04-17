Synchronisation algorithm
=========================

The state database contains two main tables.

The **mailbox_pair** table contains the following columns:

  - **mailbox_pair_id** (integer)

    An identifier for this (Maildir folder, IMAP mailbox, UIDVALIDITY) triple.

  - **local_mailbox** (text)

    The name of the Maildir folder.

  - **remote_mailbox** (text)

    The name of the IMAP mailbox.

  - **uidvalidity** (number)

    The UIDVALIDITY value provided by the IMAP server for the IMAP mailbox.
    If the UIDVALIDITY value changes then all old UIDs for the IMAP mailbox
    are no longer valid, so we will start over with a new `mailbox_pair_id`.

  - **last_modseqvalzer** (integer)

    A saved MODSEQ value from the last synchronisation cycle.
    The state database contains a snapshot of the IMAP mailbox as least at
    recent as this MODSEQ value. Individual rows in the `pairing` table
    may be more recent.

The **pairing** table contains the following columns:

  - **mailbox_pair_id** (integer)

    Identifies the triple (Maildir folder, IMAP mailbox, UIDVALIDITY)
    that this table row applies to.

  - **message_id** (text)

    An email message ID, not necessarily unique.

  - **local_uniquename** (text or NULL)

    The uniquename portion of the Maildir message file, i.e. the filename
    excluding flags.
    Is NULL if there is no file for this message in the Maildir folder.

  - **local_expunged** (integer)

    Set to 1 (true) if a Maildir message file previously existed for this
    message but has been "expunged" (permanently deleted).
    `local_uniquename` must be NULL if `local_expunged` is true.

  - **local_flags** (text)

    Flags of the local message. Each string value encodes three flag sets:

        flag-set [+flag-set] [-flag-set]

    The first flag-set lists the current flags on the message that
    do not need to be propagated to the other mailbox.

    The optional second flag-set lists the current flags that are yet to be
    added to the corresponding message in the other mailbox.

    The optional third flag-set lists old flags that are yet to be
    removed from the corresponding message in the other mailbox.

  - **local_flags_attn** (integer)

    This is set to 1 (true) when `local_flags` is modified such that there are
    flag changes to be propagated to the other side.

    It is set to 0 (false) once there are no flag changes to propagate.
    The + and - flag-sets may remain non-empty due to conflicting flag changes.

  - **remote_uid** (integer or NULL)

    The UID of the message in the IMAP mailbox.
    Is NULL if there is no UID for this message in the IMAP mailbox.

  - **remote_expunged** (integer)

    Set to 1 (true) if the UID previously existed for this message but has been
    "expunged" (permanently deleted).
    `remote_uid` must be NULL if `remote_expunged` is true.

  - **remote_flags** (text)

    Flags of the remote message. Same format as `local_flags`.

  - **remote_flags_attn** (integer)

    Analogous to `local_flags_attn`.

  - **remote_modseqvalzer** (integer)

    The MODSEQ value when the remote message info was most recently updated in
    this table row, or else 0.

    (This is used to make progress when we have to fetch updates for a lot of
    messages in the IMAP mailbox, that we may otherwise have trouble finishing
    before being interrupted. The `remote_modseqvalzer` column is used to
    figure out which UID was most recently updated, so that we can resume
    updating from the next UID onwards. For that to work, lower UIDs must be
    updated before higher UIDs.)

Synchronisation starts in **sync_and_repeat**.

 1. **lookup_remote_mailbox_modseqvalzer**

    Get the saved MODSEQ value from the `mailbox_pair` table. This reduces the
    amount of data that the IMAP server will need to send in the next step.

 2. **update_db_remote_mailbox**

    2a. Fetch information from the IMAP server about changes to the IMAP
    mailbox.

    If a UID has been seen before, update the `pairing` table row for that UID.
    In particular, the `remote_flags` are updated, and `remote_flags_attn` is
    set to true if there are any flag changes that will need to be propagated
    to the local message.

    If a UID has not been seen before, then a new table row is added for that
    UID, without a corresponding local message.

    2b. Save the new highest MODSEQ value in the `mailbox_pair` table.
    At the next sync cycle, we only need to fetch updates from the IMAP server
    that occur after this new MODSEQ value.

    2c. Get the list of all valid UIDs in the IMAP mailbox.
    For each row in the `pairing` table with a UID that no longer exists:
      - set `remote_uid` to NULL
      - set `remote_expunged` to 1
      - add `\Deleted` to `remote_flags`
      - maybe set `remote_flags_attn` to true

    At this point, the state database contains an updated snapshot of the
    remote mailbox state, and the `remote_flags` column contains flag changes
    that need to be propagated to the Maildir folder.

 3. **update_db_local_mailbox**

    Scan the Maildir folder.

    If a uniquename (representing a Maildir message) has been seen before,
    update the `pairing` table row with that uniquename. In particular, the
    `local_flags` are updated, and `local_flags_attn` is set to true if there
    are any flag changes that will need to be propagated to the remote message.

    If a uniquename has not been seen before, then a new table row is added for
    that uniquename, without a corresponding remote message.

    Lastly, for each row in the `pairing` table with a uniquename not found in
    the Maildir folder:
      - set `local_uniquename` to NULL
      - set `local_expunged` to 1
      - add `\Deleted` to `local_flags`
      - maybe set `local_flags_attn` to true

    At this point, the state database contains an updated snapshot of the
    local mailbox state, and the `local_flags` column contains flag changes
    that need to be propagated to the IMAP mailbox.

 4. **propagate_flag_deltas_from_remote**

    Apply non-conflicting flag changes from remote messages to the
    corresponding local messages. This includes renaming Maildir message files
    to reflect their new flags, and recording the changes in the `pairing`
    table.

    A message may have *conflicting* flag changes from both sides. For example,
    one side may want to apply the change `+X` while the other side wants to
    apply the change `-X`. Since we don't know which way to resolve the
    conflict, we do not apply either flag change, and leave it to be resolved
    one way or other by the user.

    Complication: a remote message may have flag changes to propagate to a
    previously paired local message, but the local message is expunged.

      - If the remote message was undeleted then we will want to create a new
        local message to match the remote message. This is done by resetting
        the local message info in the `pairing` table row back to an unpaired
        state.

      - Otherwise, the remote message should be in a deleted state and the
        local message is in an expunged state. We leave the flag changes
        unapplied.

 5. **propagate_flag_deltas_from_local**

    Apply non-conflicting flag changes from local messages to the corresponding
    remote messages. This involves sending commands to the IMAP server to
    modify the flags of remote messages, and recording the changes in the
    `pairing` table.

    Complication: as above, there may be flag changes to propagate from a local
    message to a remote message, but the previously paired UID is expunged.
    Again, if the local message was undeleted then we want to create a new
    remote message to match the local message, and this is done by resetting
    the remote message info in the `pairing` table row back to an unpaired
    state.

 6. **download_unpaired_remote_messages**

    Download any remote messages that are not paired with a local message.

    When a message is downloaded, we search for an unpaired local message with
    the same Message-ID and same message contents. If there is such a message,
    then we can "pair" the local message and the remote message to avoid
    creating a duplicate copy of the message. This is important for syncing a
    non-empty Maildir folder with a non-empty IMAP mailbox for the first time.
    There will be two table rows (one for the unpaired local message, one for
    the unpaired remote message) which need to be merged, and the redundant
    table row deleted.

    If there is no unpaired local message file with the same contents as the
    remote message, we create a new message file in the Maildir folder
    (with the same flags as the remote message), and update the `pairing` table.

 7. **propagate_flag_deltas**

    In the previous step, when a remote message is paired with an existing
    local message, there may be flag changes to be propagated in either
    direction. Here we repeat steps 4 and 5 to effect those flag changes.

 8. **upload_unpaired_local_messages**

    Upload any local messages that are not paired with a remote message to the
    IMAP mailbox (with the same flags as the local message), and record the
    changes to the `pairing` table.

    Complication: ideally the IMAP server will directly tell us the UID of a
    newly added message, so that we can record it in the `pairing` table.
    If the IMAP server doesn't tell us the UID, we will search for it by
    Message-ID. In the pathological case, the Message-ID is not unique in the
    mailbox - then we don't update the table row. At the next sync cycle,
    the uploaded message will appear as a new remote message, that we will
    download and finally pair up to the local message.

 9. **delete_expunged_pairings**

    Delete `pairing` table rows for which `local_expunged` and
    `remote_expunged` are both true. Those messages no longer exist in either
    mailbox.
