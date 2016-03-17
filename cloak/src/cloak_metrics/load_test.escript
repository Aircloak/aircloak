% Pushes a year's worth of random data for 100 metrics into a graphite database.

#!/usr/bin/env escript

-define(TIMESPAN, 365*24*60*60).
-define(INTERVAL, 10).
-define(METRICS, 100).

main(_Args) ->
  [code:add_path(Path) || Path <- filelib:wildcard("./deps/**/ebin")],
  code:add_path("./ebin/"),
  random:seed(now()),
  Now = timestamp(),
  {ok, Socket} = cloak_metrics_tcp_transport:init([{host, "localhost"}, {port, 2004}]),
  generate(Now - ?TIMESPAN, Now - ?TIMESPAN, Now, Socket).

generate(_, Here, To, _) when Here > To -> ok;
generate(Start, Here, To, Socket) ->
  Progress = 100 * (Here - Start) / (To - Start),
  io:format("\r~.2f%", [Progress]),
  Encoded = cloak_metrics_carbon_pickle:encode(metrics(Here)),
  ok = cloak_metrics_tcp_transport:send(Encoded, Socket),
  generate(Start, Here + ?INTERVAL, To, Socket).

metrics(Timestamp) ->
  [
    {
      iolist_to_binary(io_lib:format("load_test.datapoint_~p", [Index])),
      {Timestamp, random:uniform(100)}
    } || Index <- lists:seq(1, ?METRICS)
  ].

-define(UNIX_EPOCH, 62167219200).   %% = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

timestamp_now() ->
  datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

unix_time_to_datetime(UnixTime) ->
  calendar:gregorian_seconds_to_datetime(UnixTime + ?UNIX_EPOCH).