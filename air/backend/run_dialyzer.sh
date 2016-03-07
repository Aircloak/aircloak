#!/usr/bin/env bash

set -e -o pipefail

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
REBAR_PLT_DIR="$HOME/air-plt" make rebuild-plt &> /dev/null || true

echo "dialyzing air"
env REBAR_PLT_DIR="$HOME/air-plt" make dialyzer
