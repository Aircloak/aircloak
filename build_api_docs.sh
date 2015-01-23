#!/usr/bin/env bash

# terminate on first error
set -e

WEB_ROOT=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $WEB_ROOT/slate
bundle install
bundle exec middleman build
rm -rf $WEB_ROOT/public/apidocs
cp -rp $WEB_ROOT/slate/build $WEB_ROOT/public/apidocs