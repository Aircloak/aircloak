%% @doc The result sender receives results that have been aggregated,
%%      noised up and sanitized, and send them to the URL specified
%%      in the batch task request.
-module(result_sender).
-behaviour(gen_fsm).

%% API
-export([
  start_link/1,
  send_results/3
]).

%% gen_fsm callbacks
-export([
  init/1,
  create_aggregate_results/2,
  send_results_state/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
]).

-include("cloak.hrl").

-record(state, {
  task_id :: task_id(),
  result_destination :: result_destination(),
  raw_results = [],
  reply
}).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Takes a set of noised results, and sends
%%      them to the URL specified in the query as the
%%      result endpoint.
-spec send_results(task_id(), result_destination(), [#bucket_report{}]) -> pid().
send_results(TaskId, ResultDestination, Results) ->
  Args = [
    {task_id, TaskId},
    {result_destination, ResultDestination},
    {results, Results}
  ],
  {ok, Pid} = supervisor:start_child(result_sender_sup, [Args]),
  Pid.

start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).


%% -------------------------------------------------------------------
%% FSM callbacks
%% -------------------------------------------------------------------

init(Args) ->
  State = #state{
    task_id = proplists:get_value(task_id, Args),
    result_destination = proplists:get_value(result_destination, Args),
    raw_results = proplists:get_value(results, Args)
  },
  {ok, create_aggregate_results, State, 0}.

create_aggregate_results(timeout, #state{raw_results = RawResults, task_id = TaskId} = S0) ->
  Reply = convert_results(TaskId, RawResults),
  S1 = S0#state{raw_results = [], reply = Reply},
  {next_state, send_results_state, S1, 0}.

send_results_state(timeout, #state{reply = Reply, result_destination = ResultDestination} = State) ->
  send_reply(ResultDestination, Reply),
  {stop, normal, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

convert_results(TaskId, Results) ->
  Buckets = [convert_bucket_to_json(Bucket) ||
      #bucket_report{label=#bucket_label{label=Label}}=Bucket <- Results,
      Label =/= ?JOB_EXECUTION_ERROR],
  Exceptions = [[{error, iolist_to_binary(Error)}, {count, Count}] ||
      #bucket_report{label=#bucket_label{label=?JOB_EXECUTION_ERROR, value=Error},
          noisy_count=Count} <- Results],
  ?INFO("json report: ~p buckets, ~p exceptions", [length(Buckets), length(Exceptions)]),
  mochijson2:encode([
    {task_id, TaskId},
    {buckets, Buckets},
    {exceptions, Exceptions}
  ]).

convert_bucket_to_json(#bucket_report{label=#bucket_label{label=Label, value=Value}, noisy_count=Count}) ->
  [{label, iolist_to_binary(Label)}] ++ [{value, iolist_to_binary(V)} || V <- [Value], V /= undefined] ++
      [{count, Count}].

send_reply(air_socket, Reply) ->
  'Elixir.Cloak.AirSocket':send_task_results(iolist_to_binary(Reply));
send_reply({url, Url}, Reply) ->
  Format = "application/json",
  CompressedReply = zlib:gzip(Reply),
  Headers = [{"Content-Encoding", "gzip"}],
  Request = {binary_to_list(Url), Headers, Format, CompressedReply},
  ?INFO("Sending results to ~p", [Url]),
  case httpc:request(post, Request, [], []) of
    {ok, _Result} ->
      ok;
    {error, Reason} ->
      ?ERROR("Failed to return results. Reason: ~p", [Reason])
  end;
send_reply({process, Pid}, Reply) ->
  Pid ! {reply, Reply}.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

convert_results_test() ->
  TaskId = "task",
  Results = [
    #bucket_report{label=#bucket_label{label= <<"foo1">>, value=undefined}, count=1234, noisy_count=1234},
    #bucket_report{label=#bucket_label{label= <<"foo2">>, value="bar1"}, count=2345, noisy_count=2345},
    #bucket_report{label=#bucket_label{label=?JOB_EXECUTION_ERROR, value="x1"}, count=1, noisy_count=1},
    #bucket_report{label=#bucket_label{label= <<"foo3">>, value="bar2"}, count=4567, noisy_count=4567},
    #bucket_report{label=#bucket_label{label= <<"foo5">>, value=undefined}, count=5678, noisy_count=5678},
    #bucket_report{label=#bucket_label{label=?JOB_EXECUTION_ERROR, value="x2"}, count=2, noisy_count=2},
    #bucket_report{label=#bucket_label{label= <<"foo6">>, value=undefined}, count=6789, noisy_count=6789}
  ],
  JSONBuckets = [
    [{label, <<"foo1">>}, {count, 1234}],
    [{label, <<"foo2">>}, {value, <<"bar1">>}, {count, 2345}],
    [{label, <<"foo3">>}, {value, <<"bar2">>}, {count, 4567}],
    [{label, <<"foo5">>}, {count, 5678}],
    [{label, <<"foo6">>}, {count, 6789}]
  ],
  JSONExceptions = [
    [{error, <<"x1">>}, {count, 1}],
    [{error, <<"x2">>}, {count, 2}]
  ],
  JSONResult = mochijson2:encode([
    {task_id, TaskId},
    {buckets, JSONBuckets},
    {exceptions, JSONExceptions}
  ]),
  ?assertEqual(JSONResult, convert_results(TaskId, Results)).

-endif.
