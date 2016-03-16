#!/bin/sh
set -e

if [ $# -eq 1 ]
then
  user="$1"
else
  user=cloaktest1
fi

. ./prepare_db.funcs.sh

conditionally_create_user "$user"
regenerate_database "$user" "$user"
