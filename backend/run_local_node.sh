#!/bin/bash

set -eo pipefail

cd $(dirname $0)

. ../config/config.sh

mkdir -p .data

config_base="./rel/files/"
app_base="./.data"
config_dest="./.data/config"

mkdir -p $config_dest

num=$1

if [ $num -eq 1 ]; then
  node_name="air"
else
  node_name="air$num"
fi

function adapt_port {
  ((tcp_port=$(get_tcp_port dev $1)+$2-1))
  curl -s -L http://127.0.0.1:$ETCD_CLIENT_PORT/v2/keys/tcp_ports/$1 -XPUT -d value=$tcp_port > /dev/null
}

./copy_configs.sh
. rel/files/set_etcd_port.sh dev

adapt_port air_backend/http $num
adapt_port airpub/http $num

cat "$config_base/sys.config" | \
    sed "s,.data/1,.data/$num," \
    > "$config_dest/app$num.config"

cat "$config_base/vm.args" | \
    sed "s,name air,name $node_name," | \
    sed "s,\$INET_DIST_LISTEN_MIN,$(get_tcp_port dev air_backend/inet_dist_listen_min)," | \
    sed "s,\$INET_DIST_LISTEN_MAX,$(get_tcp_port dev air_backend/inet_dist_listen_max)," \
    > "$config_dest/vm$num.args"

echo "Created $config_dest/app$num.config and $config_dest/vm$num.args"
echo "Starting node '$node_name'"

eval "erl -pa apps/*/ebin -pa deps/*/ebin -s air -s reloader -config $config_dest/app$num.config -setcookie air -cloak in_development true -args_file $config_dest/vm$num.args $BEAM_ARGS"