#!/bin/bash

set -eo pipefail

# --all-warnings prints all warnings to stderr, but returns exit status 0, so we need to capture it and analyze
#
# The first compilation ensures that the build is up to date, so the second compilation will only emit something
# if there are warnings.
mix compile
warnings=$(mix compile --all-warnings 2>&1 > /dev/null)

if [ "$warnings" != "" ]; then
  echo "$warnings" >&2
  exit 1
fi
