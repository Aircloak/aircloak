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
  send_result_state/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
]).

-include_lib("aircloak_common/include/elixir_logger.hrl").

-record(state, {
  query_id :: query_id(),
  result_destination :: result_destination(),
  result :: result() | undefined,
  reply = undefined :: #{} | undefined
}).

-type query_id() :: binary().
-type result() :: {error, binary()} | {buckets, [binary()], [#{}]}.
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
  State = #state{
    query_id = QueryId,
    result_destination = ResultDestination,
    reply = Result
  },
  {ok, Pid} = supervisor:start_child(result_sender_sup, [State]),
  Pid.

start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).


%% -------------------------------------------------------------------
%% FSM callbacks
%% -------------------------------------------------------------------

init(InitialState) ->
  {ok, send_result_state, InitialState, 0}.

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
