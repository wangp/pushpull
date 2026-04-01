#!/bin/sh
#
# A couple of times now, GMail has started returning System Error in response
# to a "UID SEARCH RETURN (ALL) MODSEQ nnnnnnnn" request, e.g.
#
#   2025-08-07 11:07:58 - Synchronising from MODSEQ 13778789
#   4 UID SEARCH RETURN (ALL) MODSEQ 13778790
#   * BYE System Error
#   2025-08-07 11:08:08 - Error: unexpected response to UID SEARCH: System Error
#
# Retrying the request does not help.
#
# A workaround is to reset the last_modseqvalzer column for that mailbox in the
# mailbox_pair table to make the UID SEARCH start from an earlier modseq value
# (possibly 0). Somehow that resolves the issue.
#
# Usage: reset_lastmodseq.sh [DB-FILE]
#
set -eu

DB_FILE=${1:-state.db}

if [ ! -f "$DB_FILE" ]; then
    echo "Database file not found: $DB_FILE"
    exit 1
fi

sqlite3 -header -column "$DB_FILE" \
    'SELECT mailbox_pair_id, local_mailbox, remote_mailbox, uidvalidity, last_modseqvalzer
    FROM mailbox_pair
    ORDER BY mailbox_pair_id;'

echo
printf "Enter mailbox_pair_id: "
read -r PAIR_ID

case "$PAIR_ID" in
    [0-9]*) ;;
    *)
        echo "Invalid mailbox_pair_id"
        exit 1
        ;;
esac

EXISTS=$(sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM mailbox_pair WHERE mailbox_pair_id = $PAIR_ID;")
if [ "$EXISTS" -eq 0 ]; then
    echo "mailbox_pair_id not found"
    exit 1
fi

MAX_MODSEQ=$(sqlite3 "$DB_FILE" \
    "SELECT COALESCE(MAX(remote_modseqvalzer), 0)
    FROM pairing
    WHERE mailbox_pair_id = $PAIR_ID;")

echo
echo "Max known modseq value for mailbox_pair_id=$PAIR_ID: $MAX_MODSEQ"

echo
printf "Set last_modseqvalzer to: "
read -r NEW_VAL

case "$NEW_VAL" in
    [0-9][0-9]*) ;;
    *)
        echo "Invalid value"
        exit 1
        ;;
esac

SQL_STMT="UPDATE mailbox_pair SET last_modseqvalzer = $NEW_VAL WHERE mailbox_pair_id = $PAIR_ID;"

echo
echo "Will run:"
echo "$SQL_STMT"
echo

printf "Execute statement? [y/N]: "
read -r CONFIRM

case "$CONFIRM" in
    y | Y)
        sqlite3 "$DB_FILE" "$SQL_STMT"
        echo "Update applied."
        ;;
    *)
        echo "Aborted."
        ;;
esac
