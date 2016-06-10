#!/bin/bash

config_base="./rel/files/"
app_base="./.data"
config_dest="./.data/config"

mkdir -p $config_dest

num=$1

if [ $num -eq 1 ]; then
  cloak_name="cloak"
else
  cloak_name="cloak$num"
fi

((listen_port=34000+100*(num-1)))
((http=8098+100*(num-1)))

cat "$app_base/app.config" | \
  sed "s/{min_age_of_timeslot_for_sync_in_seconds, 300}/{min_age_of_timeslot_for_sync_in_seconds, 10}/" | \
  sed "s,.data/1,.data/$num," | \
  sed "s,34000,$listen_port," | \
  sed "s,https://infrastructure-api.aircloak.com,https://infrastructure-api.air-local:20000," | \
  sed "s,address\, \"127\.0\.0\.1\",address \, \"0.0.0.0\"," | \
  sed "s,8098,$http," > "$config_dest/app$num.config"

cat "$config_base/vm.args" | \
  sed "s,name cloak,name $cloak_name,"  > "$config_dest/vm$num.args"

echo "Created $config_dest/app$num.config and $config_dest/vm$num.args"
echo "Setting ulimit to 5000"
ulimit -n 5000
echo "Starting node '$cloak_name'"

eval "erl -pa ebin -pa deps/*/ebin -s cloak -s reloader -config $config_dest/app$num.config -setcookie cloak -cloak in_development true -args_file $config_dest/vm$num.args $BEAM_ARGS"
