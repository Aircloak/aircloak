%% @doc The interface point to talk to any cloak database. Underneath, we use multiple pools of processes.
%%      Each pool manages a distinct connection type, a unique combination of host, user, database, port.
%%      This allows us to reuse database connections, and control the pool size for each host separately.
%%      Namely, we use a larger pool size for connections on local host, and smaller pools for all
%%      other connections.
-module(cloak_db).

%% API
-export([
  multi_call/3,
  multi_call/6,
  call/2,
  call/3
]).

-include("cloak.hrl").

-export_type([
  connection_params/0
]).

-type request_fun() :: fun((sql_conn:connection()) -> any()).
-type connection_param() ::
  {host, string() | binary() | atom()} |
  {database, string() | binary()} |
  {user, string() | binary()} |
  {port, pos_integer()}.
-type connection_params() :: [connection_param()].
-type operation_name() :: atom() | string() | binary().

-define(DEFAULT_TIMEOUT, 20000).
-define(DEFAULT_MULTI_TIMEOUT, 1000).
-define(DEFAULT_MULTI_RETRIES, 20).
-define(DEFAULT_RANDOM_TIMEOUT, 500).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Calls the given function with cloak connections as specified.
%%      Timeout handling is specified with two parameters. The individual timeout to get a single connection
%%      and the number of retries. Furthermore we have a random timeout between the trials to get all
%%      connections setup. Randomness should ensure that two competing processes requiring more than one
%%      connection do not make each other fail at each retry (see Dining Philosophers problem).
-spec multi_call(operation_name(), [connection_params()], request_fun()) -> any().
multi_call(OperationName, ConnectionParams, Fun) ->
  multi_call(OperationName, ConnectionParams, Fun, ?DEFAULT_MULTI_TIMEOUT, ?DEFAULT_MULTI_RETRIES,
      ?DEFAULT_RANDOM_TIMEOUT).

-spec multi_call(operation_name(), [connection_params()], request_fun(), non_neg_integer()|infinity,
    non_neg_integer(), pos_integer()) -> any().
multi_call(OperationName, ConnectionParamsList, Fun, AcquireTimeout, Retries, RandomTimeout) ->
  QueuedAt = os:timestamp(),
  try
    perform_multi_call(
        [cloak_db_pool:local_pid(ConnectionParams) || ConnectionParams <- ConnectionParamsList],
        fun(Connections) ->
          ?REPORT_DURATION("db_operation.queued", QueuedAt),
          run_operation(OperationName, Fun, Connections)
        end,
        AcquireTimeout,
        Retries,
        RandomTimeout
      )
  catch exit:({timeout, _} = Reason) ->
    ?ERROR("database operation timeout for '~p' on ~s (~p)",
        [OperationName, [proplists:get_value(host, CPs) || CPs <- ConnectionParamsList],
            erlang:get_stacktrace()]),
    exit(Reason)
  end.

%% @doc Calls the given function on the local cloak database using the default connection pool.
-spec call(operation_name(), request_fun()) -> any().
call(OperationName, Fun) -> call(OperationName, [{host, "127.0.0.1"}], Fun).

%% @doc Calls the given function on the cloak database using provided connection parameters.
%%      If only `[{host, Host}]' is provided, remaining connection parameters are taken
%%      from `app.config'. Database connections are reused across multiple requests.
-spec call(operation_name(), connection_params(), request_fun()) -> any().
call(OperationName, ConnectionParams, Fun) ->
  multi_call(OperationName, [ConnectionParams], fun([Connection]) -> Fun(Connection) end,
      ?DEFAULT_TIMEOUT, 1, 1).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

run_operation(undefined, Fun, Connections) ->
  do_run_operation(Fun, Connections);
run_operation(OperationName, Fun, Connections) ->
  MetricName = <<"db_operation.queries.", (cloak_util:binarify(OperationName))/binary >>,
  cloak_metrics:count(MetricName),
  ?MEASURE(MetricName, do_run_operation(Fun, Connections)).

do_run_operation(Fun, Connections) ->
  cloak_metrics:count("db_operation.started"),
  Res = ?MEASURE("db_operation.duration", Fun(Connections)),
  cloak_metrics:count("db_operation.finished"),
  Res.


%% -------------------------------------------------------------------
%% Pool related functions
%% -------------------------------------------------------------------


%% @doc Takes one connection from each given pool, and performs the call on every connection.
perform_multi_call(Pools, Fun, AcquireTimeout, Retries, RandomSleep) ->
  case repeated_try_acquire_all_pools(Pools, AcquireTimeout, Retries, RandomSleep) of
    {error, Reason} ->
      exit(Reason);
    {ok, Connections} ->
      try
        Fun([cloak_db_pool:connection_pid(Connection) || Connection <- Connections])
      after
        lists:foreach(fun cloak_db_pool:release_connection/1, Connections)
      end
  end.

repeated_try_acquire_all_pools(_Pools, _AcquireTimeout, 0, _RandomSleep) ->
  {error, {timeout, acquire_pools}};
repeated_try_acquire_all_pools(Pools, AcquireTimeout, Retries, RandomSleep) ->
  case try_acquire_all_pools(Pools, [], AcquireTimeout) of
    error ->
      timer:sleep(random_util:uniform(RandomSleep)),
      repeated_try_acquire_all_pools(Pools, AcquireTimeout, Retries - 1, RandomSleep);
    {ok, PoolConnections} ->
      {ok, lists:reverse(PoolConnections)}
  end.

try_acquire_all_pools([], PoolConnections, _AcquireTimeout) ->
  {ok, PoolConnections};
try_acquire_all_pools([Pool|Pools], PoolConnections, AcquireTimeout) ->
  case cloak_db_pool:acquire_connection(Pool, AcquireTimeout) of
    {ok, Connection} ->
      try_acquire_all_pools(Pools, [Connection|PoolConnections], AcquireTimeout);
    {error, timeout} ->
      lists:foreach(fun cloak_db_pool:release_connection/1, PoolConnections),
      error
  end.
