%% @doc A server process that takes care of aggregating statistics and forwarding aggregated results
%%      to dispatchers. Conceptually, this is a responsibility of cloak_metrics server, but was moved
%%      to a separate process to make cloak_metrics more fluent.
-module(cloak_metrics_aggregator).
-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API
-export([
  start_link/0, aggregate_and_dispatch/4,
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()}.
%% @doc Starts the server.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec aggregate_and_dispatch(pid(), cloak_metrics_data:metrics(), [pid()], float()) -> ok.
%% @doc Requests aggregation and dispatching of collected data.
aggregate_and_dispatch(Server, Collected, Dispatchers, Timespan) ->
  gen_server:cast(Server, {aggregate_and_dispatch, Collected, Dispatchers, Timespan}).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  {ok, undefined}.

%% @hidden
handle_call(Unknown, _, State) ->
  lager:error("Unknown call in ~p: ~p", [{?MODULE, Unknown}]),
  {reply, error, State}.

%% @hidden
handle_cast({aggregate_and_dispatch, Collected, Dispatchers, Timespan}, State) ->
  case cloak_metrics_data:aggregate(Collected, Timespan) of
    [] -> ok; % no data -> moving on
    Aggregated -> [
        cloak_metrics_dispatcher:send(DispatcherPid, Aggregated) || DispatcherPid <- Dispatchers
      ]
  end,
  {noreply, State};
handle_cast(Unknown, State) ->
  lager:error("Unknown cast in ~p: ~p", [{?MODULE, Unknown}]),
  {noreply, State}.

%% @hidden
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.
