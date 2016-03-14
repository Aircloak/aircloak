#!/usr/bin/env bash

set -e -o pipefail

mix compile

echo "Running dialyzer..."

# Notes:
#
# - We need to capture the output, since mix task always exits with the code 0.
# - We're disabling undefined callbacks since this warning appears all over the
#   place, including some Elixir modules. This needs to be investigated further.
errors="$(mix dialyze 2>&1 > /dev/null)"
if [ "$errors" == "" ]; then
  echo "Dialyzer succeeded!"
  exit 0
else
  echo "Dialyzer errors:"
  echo "$errors"
  exit 1
fi
