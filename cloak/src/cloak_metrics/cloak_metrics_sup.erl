%% @hidden
%%  Manages singleton instance of the cloak_metrics server. This allows clients
%%  to start and configure the server dynamically in run-time.
-module(cloak_metrics_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0, start_metrics_server/1, init/1
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_metrics_server(cloak_metrics:options()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_metrics_server(Options) ->
  supervisor:start_child(?MODULE, [Options]).


%% -------------------------------------------------------------------
%% supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  {ok, {{simple_one_for_one,10,10}, [
    {undefined, {cloak_metrics, start_link, []}, permanent, 5000, worker, [cloak_metrics]}
  ]}}.
