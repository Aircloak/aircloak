%% @doc The result sender receives buckets that have been aggregated,
%%      noised up and sanitized, and send them to the URL specified
%%      in the batch query request.
-module(result_sender).
-behaviour(gen_fsm).

%% API
-export([
  start_link/1,
  send_result/3
]).

%% gen_fsm callbacks
-export([
  init/1,
  convert_result/2,
  send_result_state/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
]).

-include("cloak.hrl").

-record(state, {
  query_id :: query_id(),
  result_destination :: result_destination(),
  result :: result() | undefined,
  reply = undefined :: #{} | undefined
}).

-type result() :: {error, binary()} | {buckets, [binary()], [#bucket{}]}.
-type result_destination() :: {url, binary()} | {process, pid()} | air_socket.

-export_type([
  result_destination/0
]).

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Takes a set of noised buckets, and sends
%%      them to the URL specified in the query as the
%%      result endpoint.
-spec send_result(query_id(), result_destination(), result()) -> pid().
send_result(QueryId, ResultDestination, Result) ->
  Args = [
    {query_id, QueryId},
    {result_destination, ResultDestination},
    {result, Result}
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
    query_id = proplists:get_value(query_id, Args),
    result_destination = proplists:get_value(result_destination, Args),
    result = proplists:get_value(result, Args)
  },
  {ok, convert_result, State, 0}.

convert_result(timeout, #state{result = Result, query_id = QueryId} = S0) ->
  Reply = case Result of
    {buckets, Columns, Buckets} ->
      ?INFO("Processing buckets report for query ~s: ~p buckets", [QueryId, length(Buckets)]),
      #{query_id => QueryId, columns => Columns, rows => expand_buckets(Columns, Buckets)};
    {error, Reason} ->
      ?INFO("Processing error report for query ~s: ~p", [QueryId, Reason]),
      #{query_id => QueryId, error => Reason}
  end,
  S1 = S0#state{result = undefined, reply = Reply},
  {next_state, send_result_state, S1, 0}.

send_result_state(timeout, #state{reply = Reply, result_destination = ResultDestination} = State) ->
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

%% Converts the buckets to rows.
expand_buckets([<<"count(*)">>], Buckets) ->
  lists:map(fun(#bucket{noisy_count = Count}) -> [Count] end, Buckets);
expand_buckets(C, Buckets) ->
  lists:flatmap(fun (#bucket{property = Property, noisy_count = Count}) ->
    lists:duplicate(Count, Property)
  end, Buckets).

send_reply(air_socket, Reply) ->
  'Elixir.Cloak.AirSocket':send_query_result(Reply);
send_reply({url, Url}, Reply) ->
  Format = "application/json",
  CompressedReply = zlib:gzip('Elixir.Poison':'encode!'(Reply)),
  Headers = [{"Content-Encoding", "gzip"}],
  Request = {binary_to_list(Url), Headers, Format, CompressedReply},
  ?INFO("Sending result to ~p", [Url]),
  case httpc:request(post, Request, [], []) of
    {ok, _Result} ->
      ok;
    {error, Reason} ->
      ?ERROR("Failed to return result. Reason: ~p", [Reason])
  end;
send_reply({process, Pid}, Reply) ->
  Pid ! {reply, Reply}.
