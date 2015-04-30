#!/bin/bash

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

((http=11000+num-1))

cat "$config_base/sys.config" | \
    sed "s,.data/1,.data/$num," | \
    sed "s,11000,$http," > "$config_dest/app$num.config"

cat "$config_base/vm.args" | \
    sed "s,name air,name $node_name,"  > "$config_dest/vm$num.args"

echo "Created $config_dest/app$num.config and $config_dest/vm$num.args"
echo "Setting ulimit to 5000"
ulimit -n 5000
echo "Starting node '$node_name'"

eval "erl -pa ebin -pa deps/*/ebin -s air -s reloader -config $config_dest/app$num.config -setcookie air -cloak in_development true -args_file $config_dest/vm$num.args $BEAM_ARGS"