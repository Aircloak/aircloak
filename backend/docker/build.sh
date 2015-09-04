#!/bin/bash

set -e

# At /aircloak/source we have mounted the source directory on the client.
# It contains the raw source for the application.
#
# We use the folder `artifacts` under the source directory to cache builds,
# so we avoid repeatedly performing dependency fetching and compiling.
# This should speed up subsequent builds.
# The finished release will be present in `artifacts/cache/rel`

function same_file {
  first_file=$1
  other_file=$2
  if [ -f "$other_file" ]; then
    first_sha=`sha1sum $first_file | awk '{print $1}'`
    other_sha=`sha1sum $other_file | awk '{print $1}'`
    if [[ "$first_sha" == "$other_sha" ]]; then
      return 0
    else
      return 1
    fi
  else
    return 1
  fi
}

function log {
  statement=$1
  echo "[aircloak] $statement"
}

function copy {
  item=$1
  # We delete the original first to make sure
  # we get a clean and updated build
  rm -rf $CACHE_DIR/$item
  cp -R $SOURCE_DIR/$item $CACHE_DIR/$item
}

SOURCE_DIR="/aircloak/source"
ARTIFACTS_DIR="$SOURCE_DIR/artifacts"
CACHE_DIR="$ARTIFACTS_DIR/cache"
mkdir -p $CACHE_DIR

# if same_file "$SOURCE_DIR/rebar.config.lock" "$CACHE_DIR/rebar.config.lock"; then
#   log "Dependencies are unchanged"
# else
#   log "Dependencies have changed compared to cached dependencies. Refetching..."
#   # We have to copy over the hosts SSH keys in order to get the dependencies
#   # from github. These are only copied over to the running container instance,
#   # and are therefore automatically gone as soon as the build has completed.
#   cp -R ~/ssh/* ~/.ssh/
#   echo -e "Host github.com\n\tStrictHostKeyChecking no\n" >> /home/deployer/.ssh/config
#   rm -rf $CACHE_DIR
#   mkdir -p $CACHE_DIR
# fi

log "Copying source files"
cd $CACHE_DIR
copy apps
copy include
copy rel
copy generate_cloak_conf.escript

log "Making release"
make rel
echo "Release generated"