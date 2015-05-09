#!/usr/bin/env bash

# This script is a hack to run dialyzer from travis. It is made as a separate script to avoid
# collisions with make. In general, developers should locally run make dialyzer and not this script.
# That said, you can still run this script locally, if you need to test why dialyzer fails on travis.
# In general, if you modify this file, try it at least once locally, before pushing to GitHub.

echo "Starting dialyzer..."

mkdir -p $HOME/air-plt

# Prepares local plts. We generate everything in the current folder and redirect to /dev/null, since
# these commands will generate a lot of errors that are irrelevant, but might seem confusing when looking at
# the travis output.
if [ ! -f $HOME/air-plt/dialyzer.plt ]; then
  echo "rebuidling base plt"
  dialyzer --build_plt --apps erts kernel stdlib crypto sasl --output_plt $HOME/air-plt/dialyzer.plt &> /dev/null
fi

echo "rebuilding deps plt"
REBAR_PLT_DIR="$HOME/air-plt" make rebuild-plt &> /dev/null

# Here we start dialyzing our app. We filter out many known outputs, and inspect whether the result is
# an empty string (no errors) or not (errors). The reason for this hack is that we already ignore some
# dialyzer errors, so the exit code of make dialyzer is always non-zero. Thus, we have to do this
# trickery and analyze the output of dialyzer ourselves.
echo "dialyzing air"
output=$(
  REBAR_PLT_DIR="$HOME/air-plt" make dialyzer |
  grep -v -E "^WARNING: protobuf output" |
  grep -v -E "^dialyzer \\\\" |
  grep -v -E "no_native" |
  grep -v -E "pa deps" |
  grep -v -E "\-\-plts" |
  grep -v -E "grep -E" |
  grep -v -E "^  Checking whether" |
  grep -v -E "^  Proceeding with analysis" |
  grep -v -E "^ done in" |
  grep -v -E "done \(passed successfully\)" |
  grep -v -E "^done \(warnings were emitted\)" |
  grep -v -E "^Unknown functions:" | # we ignore unknown functions/types errors (due to protobufs)
  grep -v -E "^Unknown types:" |
  grep -v -E "^  [A-Za-z0-9_]+\:[A-Za-z0-9_]+\/[0-9]$"  # filters out mod:fun/? (unknown functions)
)

if [[ -n $output ]]; then
  echo "Dialyzer errors: "
  echo $output
  exit 1
else
  echo "Dialyzer succeeded."
  exit 0
fi
