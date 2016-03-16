%% @doc Supervisor that dynamically starts cloak_db pools. Each pool maps to a
%%      unique connection parameters (host, user, database, port).
%%      See {@link cloak_db} for more details.
-module(cloak_db_pool_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  init/1,
  start_pool/1,
  stop_pool/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @doc Starts the pool
-spec start_pool({cloak_db_pool, cloak_db:connection_params()}) -> {ok, pid()}.
start_pool({cloak_db_pool, PoolParams}) ->
  supervisor:start_child(?MODULE, [PoolParams]).

%% @doc Stops the pool
-spec stop_pool(pid()) -> ok | {error, term()}.
stop_pool(Pid) -> supervisor:terminate_child(?MODULE, Pid).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init ([]) ->
  {ok, {{simple_one_for_one, 10, 10}, [
    {undefined, {cloak_db_pool, start_link, []},
    transient, 5000, worker, [cloak_db_pool]}
  ]}}.
