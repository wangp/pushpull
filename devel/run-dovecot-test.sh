#!/bin/sh
#
# Run a test instance of Dovecot.
#
set -eux
sudo dovecot -F -c dovecot-test.conf
