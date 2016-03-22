#!/usr/bin/env escript

main(_Args) ->
  [code:add_path(Path) || Path <- filelib:wildcard("./deps/**/ebin")],
  code:add_path("./ebin/"),
  erlang_common:start(),
  application:start(cloak_metrics),
  cloak_metrics:start_server([
    {noise_fun, cloak_metrics:no_noise_fun()},
    {bucketizer, cloak_metrics:identity_bucketizer()},
    {path_prefix, [simulated]},
    {reporters, [
      cloak_metrics:graphite_pickle_reporter("localhost", 2004)
    ]}
  ]),
  start_counter(query, 100),
  start_histogram(query, 1, 50, 200),
  wait_forever().

wait_forever() ->
  receive
    _ -> ok
  end,
  wait_forever().

spawn_loop(Loop) ->
  random:seed(now()),
  proc_lib:spawn_link(Loop).

start_counter(Name, Frequency) ->
  spawn_loop(fun() -> feed_counter(Name, Frequency) end).

feed_counter(Name, Frequency) ->
  cloak_metrics:count(Name),
  timer:sleep(random:uniform(Frequency)),
  feed_counter(Name, Frequency).

start_histogram(Name, Frequency, From, To) ->
  spawn_loop(fun() -> feed_histogram(Name, Frequency, From, To) end).

feed_histogram(Name, Frequency, From, To) ->
  cloak_metrics:histogram(Name, random:uniform(To - From) + From),
  timer:sleep(random:uniform(Frequency)),
  feed_histogram(Name, Frequency, From, To).