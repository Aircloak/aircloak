#!/bin/bash

set -eo pipefail

# This script removes old cache folders on the CI server. Cache folders are organized in the following way:
#
# aircloak
#   tmp
#     air
#       hash1
#       hash2
#       ...
#     cloak
#       hash1
#       hash2
#       ...
#     ...
#
# Here, each hash is an md5 of combined contents of .debian-version and .tool-versions.
#
# Such structure ensures that by upgrading e.g. an Erlang version, we don't reuse the older compiled binaries which are
# potentially incompatible. The problem is however, that the older cache files are not deleted, and so the disk usage
# grows indefinitely.
#
# This script can be used to periodically cleanup the cache files. The script walks down each component in the tmp
# folder, and keeps only the most recent folder.

function remove_if_exists {
  if ls $1 1> /dev/null 2>&1; then rm $1; fi
}

aircloak_ci_folder=$1
for branch_dir in $(ls -d $aircloak_ci_folder/data/cache/branches/*); do
  cache_dir="$branch_dir/src/tmp"
  if [ -d $cache_dir ]; then
    # remove temp dockerfiles if they exist
    remove_if_exists $branch_dir/src/tmp/*.dockerfile

    # remove all but the most recent folder in each component
    for component_dir in $(ls -d $cache_dir/*); do
      if ls -d $component_dir/* 1> /dev/null 2>&1; then
        for old_cache_dir in $(ls -t -d $component_dir/* | tail -n +2); do
          echo "removing old cache folder: $old_cache_dir"
          rm -rf $old_cache_dir || true
        done
      fi
    done
  fi
done
