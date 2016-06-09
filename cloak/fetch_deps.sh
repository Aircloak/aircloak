#!/usr/bin/env bash

set -eo pipefail

# Hacky work around current issues with hex.pm. At the moment, fetching deps fails on every 4th dep. It seems
# the problem might be caused by the combination of the new hex client, hosting files on fastly, and the fact
# that we're running behind a proxy. For a bit more info see irc discussion at
# https://botbot.me/freenode/elixir-lang/2016-05-03/?msg=65362671&page=7
#
# The cause is currently not known, so here we work around by retrying a couple of times. Since fetched deps
# are cached on the disk, this should ultimately succeed.
retries=10
while [ $retries -gt 0 ]; do
  if mix deps.get $@; then
    exit 0
  else
    retries=$((retries - 1))
    if [ $retries -gt 0 ]; then sleep 1; fi
  fi
done
exit 1
