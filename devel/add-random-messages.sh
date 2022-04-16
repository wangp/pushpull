#!/bin/bash
#
# Add random messages to a Maildir directory.
#
set -eu

if test "$#" != 2 ; then
    echo "usage: $0 <maildir> <message-count>" >&2
    exit 1
fi

maildir=$1
count=$2

if ! test -d "$maildir" ; then
    echo "Directory does not exist: $maildir" >&2
    exit 1
fi

if ! test "$count" -gt 0 2>/dev/null ; then
    echo "Invalid number of messages: $count" >&2
    exit 1
fi

mkdir -p "$maildir"/{cur,new,tmp}

random_message() {
    local i=$1
    local pseudornd=$2
    cat <<EOF
Date: $( date "+%a, %d %b %Y %H:%M:%S %z" )
Message-ID: <${pseudornd}@example.com>
From: random@example.com
To: random@example.com
Subject: random message ${i}

random message ${i}
EOF
}

for (( i = 0 ; i < "$count" ; i++ )) ; do
    echo "Generating message $i"
    pseudornd=$( date '+%s.M%N' )"Q${i}"
    filename="${pseudornd}.random:2,"
    tmp_filename="${maildir}/tmp/${filename}"
    cur_filename="${maildir}/cur/${filename}"
    random_message "$i" "$pseudornd" >"$tmp_filename"
    mv "$tmp_filename" "$cur_filename"
done

echo "done."
