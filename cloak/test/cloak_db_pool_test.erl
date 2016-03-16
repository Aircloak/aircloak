%% @doc Testing the {@link anonymizer}.
%% @end
-module(cloak_db_pool_test).

-include("deps/proper/include/proper.hrl").
-include("src/cloak.hrl").
-include("test/prop_tracer.hrl").

%% Proper callbacks
-export([
  command/1,
  next_state/3,
  precondition/2,
  postcondition/3
]).

%% Action functions
-export([
  acquire_connection/1,
  use_connection/1,
  release_connection/1,
  run_query/1,
  failing_client/1
]).

-record(seq_state, {
  acquired_conns = [],
  released_conns = [],
  failed_conns = []
}).


%% ---------------------------------------------------------------------
%% PropEr Test
%% ---------------------------------------------------------------------

prop_cloak_db_pool() ->
  proper_spec(commands(?MODULE, #seq_state{}), fun proper_statem:run_commands/2).

prop_parallel_cloak_db_pool() ->
  proper_spec(parallel_commands(?MODULE, parallel), fun proper_statem:run_parallel_commands/2).

proper_spec(Commands, CommandRunner) ->
  application:ensure_all_started(gproc),
  ?FORALL(
        Cmds,
        Commands,
        ?TRACEFAIL(
              Commands,
              begin
                {ok, PoolSupervisorPid} = cloak_db_pool_sup:start_link(),
                try
                  CommandRunner(?MODULE, Cmds)
                after
                  MRef = monitor(process, PoolSupervisorPid),
                  unlink(PoolSupervisorPid),
                  exit(PoolSupervisorPid, shutdown),
                  receive
                    {'DOWN', MRef, process, _, _} -> ok
                  after 1000 ->
                    exit(PoolSupervisorPid, kill)
                  end
                end
              end,
              Result =:= ok
            )
      ).

%% Sequential test -> open, use, or release a connection.
command(#seq_state{acquired_conns=AcquiredConns}) ->
  oneof(
        [{call, ?MODULE, acquire_connection, [db_test:conn_params()]}] ++
        [{call, ?MODULE, failing_client, [db_test:conn_params()]}] ++
        [{call, ?MODULE, use_connection, [elements(AcquiredConns)]} || AcquiredConns =/= []] ++
        [{call, ?MODULE, release_connection, [elements(AcquiredConns)]} || AcquiredConns =/= []]
      );
%% Parallel test -> concurrent clients run queries, occasional clients fail
command(parallel) ->
  frequency([
        {10, {call, ?MODULE, run_query, [db_test:conn_params()]}},
        {1, {call, ?MODULE, failing_client, [db_test:conn_params()]}}
      ]).

precondition(_, _) -> true.

postcondition(#seq_state{} = SeqState, {call, ?MODULE, acquire_connection, _}, {ok, _} = Conn) ->
  check_acquired_conn(SeqState, Conn);
postcondition(#seq_state{} = SeqState, {call, ?MODULE, failing_client, _}, {ok, _} = Conn) ->
  check_acquired_conn(SeqState, Conn);
postcondition(#seq_state{}, {call, ?MODULE, use_connection, _}, {{select, 1}, [{1}]}) -> true;
postcondition(#seq_state{}, {call, ?MODULE, release_connection, _}, ok) -> true;
postcondition(parallel, {call, ?MODULE, run_query, _}, {{select, 1}, [{1}]}) -> true;
postcondition(parallel, {call, ?MODULE, failing_client, _}, {ok, _}) -> true;
postcondition(_, _, _) -> false.

check_acquired_conn(SeqState, Conn) ->
  % Occupied connection should not be acquired.
  not lists:member(Conn, SeqState#seq_state.acquired_conns) andalso
  % Failed connection should never be reused.
  not lists:member(Conn, SeqState#seq_state.failed_conns) andalso
  (
    % If some connections are returned to the pool, they should be reused.
    SeqState#seq_state.released_conns =:= [] orelse lists:member(Conn, SeqState#seq_state.released_conns)
  ).

next_state(#seq_state{} = SeqState, Result, {call, ?MODULE, acquire_connection, [_Conn]}) ->
  SeqState#seq_state{
    acquired_conns=[Result | SeqState#seq_state.acquired_conns],
    released_conns=lists:delete(Result, SeqState#seq_state.released_conns)
  };
next_state(#seq_state{} = SeqState, Result, {call, ?MODULE, failing_client, [_Conn]}) ->
  SeqState#seq_state{
    failed_conns=[Result | SeqState#seq_state.failed_conns],
    released_conns=lists:delete(Result, SeqState#seq_state.released_conns)
  };
next_state(#seq_state{} = SeqState, _Result, {call, ?MODULE, release_connection, [Conn]}) ->
  SeqState#seq_state{
    acquired_conns=lists:delete(Conn, SeqState#seq_state.acquired_conns),
    released_conns=[Conn | SeqState#seq_state.released_conns]
  };
next_state(#seq_state{} = SeqState, _Result, _Action) ->
  SeqState;
next_state(parallel, _, _Action) ->
  parallel.


%% ---------------------------------------------------------------------
%% Action functions
%% ---------------------------------------------------------------------

acquire_connection(ConnectionParams) ->
  cloak_db_pool:acquire_connection(cloak_db_pool:local_pid(ConnectionParams), timer:seconds(5)).

use_connection({ok, Connection}) ->
  sql_conn:simple_query("SELECT 1", cloak_db_pool:connection_pid(Connection)).

release_connection({ok, Connection}) ->
  cloak_db_pool:release_connection(Connection).

run_query(ConnectionParams) ->
  AcquireConnectionRes = acquire_connection(ConnectionParams),
  Res = use_connection(AcquireConnectionRes),
  release_connection(AcquireConnectionRes),
  Res.

failing_client(ConnectionParams) ->
  Me = self(),
  Pid = spawn(fun() ->
    Me ! {conn, acquire_connection(ConnectionParams)}
  end),
  MRef = monitor(process, Pid),
  receive
    {'DOWN', MRef, process, _, _} ->
      receive
        {conn, Res} -> Res
      after 1000 -> {error, timeout}
      end
  after 1000 -> {error, not_stopped}
  end.
