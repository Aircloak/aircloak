%% @doc Implementation of the database connection pooling.
%%
%%      You normally don't need to use this module directly. If you need to perform some database operations,
%%      you should use {@link cloak_db} module which will internally rely on services from this module to
%%      retrieve connections.
%%
%%      We maintain one pool per each unique connection type (determined by the sorted list of connection
%%      parameters). A pool is started lazily through the {@link local_pid/1} function.
%%
%%      The pool is initially empty. Connections are created on demand. When a client needs a connection,
%%      the pool will return an idle one if it exists. Otherwise, it will create the new connection and return
%%      it to the pool. Once the connection is returned to the pool it can be reused by subsequent clients.
%%      However, if the connection is not used for some time (as configured in app.config), it will be closed.
%%
%%      Such approach allows us to handle bursts when we need to temporary hold an unknown number of connections
%%      to multiple servers. Once those connections are not needed anymore, they are closed.
%%
%%      This pool tries to avoid common pitfalls of popular libraries (e.g. Poolboy), where a single server
%%      bottleneck can affect the overall performance. Connection requests are still serialized through the
%%      pool process, but as much of the work as possible is left for other processes. In particular,
%%      {@link cloak_db_conn_manager} powered processes are responsible for starting workers, notifying
%%      clients, and handling crashes. The {@link db_job} server is responsible for limiting concurrency.
%%      Consequently, the `cloak_db_pool' server is nothing more than a simple stack of idle connections.
-module(cloak_db_pool).
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  stop/1,
  local_pid/1,
  acquire_connection/2,
  release_connection/1,
  checkin/2,
  remove_connection/2,
  connection_pid/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("cloak.hrl").

-record(state, {
  connection_params :: cloak_db:connection_params(),
  available_connections = [] :: [pid()]
}).

-type connection() :: {ConnectionManager :: pid(), ConnectionPid :: pid()}.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the empty pool of database connection processes. Used internally by the {@link cloak_db_pool_sup}.
-spec start_link(cloak_db:connection_params()) -> {ok, pid()}.
start_link(ConnectionParams) ->
  gen_server:start_link(
        {via, local_service, {cloak_db_pool, ConnectionParams}},
        ?MODULE,
        ConnectionParams,
        []
      ).

%% @doc Stops the pool of connections on the cloak database identified by provided connection parameters.
-spec stop(cloak_db:connection_params()) -> ok.
stop(ConnectionParams) ->
  case local_service:whereis_name({cloak_db_pool, lists:keysort(1, normalize_conn_params(ConnectionParams))}) of
    undefined -> ok;
    Pid -> cloak_db_pool_sup:stop_pool(Pid)
  end.

%% @doc Retrieves the pool responsible for given connection parameters. Creates the new pool if needed.
-spec local_pid(cloak_db:connection_params()) -> pid().
local_pid(ConnectionParams) ->
  local_service:get_or_create(
        {cloak_db_pool, lists:keysort(1, normalize_conn_params(ConnectionParams))},
        fun cloak_db_pool_sup:start_pool/1
      ).

%% @doc Acquires a connection from the pool. If there are no idle connections in the pool, the new one is
%%      created. The timeout is a positive integer and will happen only if the connection isn't established
%%      in the given period. In this case, the function will return `{error, timeout}'.
%%
%%      The returned value contains the pid of the connection as well as its connection manager (see
%%      {@link cloak_db_conn_manager}). Clients can use {@link connection_pid/1} to retrieve the connection
%%      pid. Then, they can use {@link sql_conn} to perform operations on the connection.
%%
%%      The caller process should return the connection with {@link release_connection/1}. If
%%      the caller process terminates, the connection will be closed as well, because it might be left in a
%%      dirty state.
-spec acquire_connection(pid(), pos_integer()) -> {ok, connection()} | {error, any()}.
acquire_connection(Pool, AcquireTimeout) ->
  %% We use infinity timeout, relying on the implementation to enforce the actual timeout.
  ?MEASURE("cloak_db.connection.acquire_time",
      gen_server:call(Pool, {acquire_connection, AcquireTimeout}, infinity)).

%% @doc Releases the acquired connection and returns it to the pool so it can be reused.
%%      The connection is marked as idle, and an internal timer is set. If the connection is not used for some
%%      time (configured in app.config), it will be terminated.
-spec release_connection(connection()) -> ok.
release_connection({ConnectionManager, _ConnectionPid}) ->
  cloak_db_conn_manager:release(ConnectionManager).

%% @doc Used by {@link cloak_db_conn_manager} to return the connection back to the pool.
-spec checkin(pid(), pid()) -> ok.
checkin(PoolServer, ConnectionManager) ->
  %% This is synchronous to make the client wait until the connection is returned to the pool. This plays
  %% nicely with the db_job queue, making sure that we have a connection available when the client decrements
  %% the jobs counter.
  gen_server:call(PoolServer, {checkin, ConnectionManager}).

%% @doc Used by {@link cloak_db_conn_manager} to remove the idle connection from the pool.
%%      Returns true if the connection can be removed.
-spec remove_connection(pid(), pid()) -> boolean().
remove_connection(PoolServer, ConnectionManager) ->
  gen_server:call(PoolServer, {remove_connection, ConnectionManager}).

%% @doc Retrieves the pid of the connection process from the data returned by {@link acquire_connection/2}.
-spec connection_pid(connection()) -> pid().
connection_pid({_ConnectionManager, ConnectionPid}) -> ConnectionPid.


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init(ConnectionParams) ->
  process_flag(trap_exit, true),
  ?DEBUG("Starting database pool for ~p", [ConnectionParams]),
  {ok, #state{connection_params=ConnectionParams}}.

%% @hidden
handle_call({acquire_connection, AcquireTimeout}, Client,
      #state{available_connections=[], connection_params=ConnectionParams} = State
    ) ->
  ?DEBUG("Starting a new connection process"),
  %% Note: due to inner working of cloak_db_conn_manager, the connection will be open, and the client
  %% notified concurrently. Thus, this process won't wait until the connection is open.
  cloak_db_conn_manager:start_link(self(), Client, ConnectionParams, AcquireTimeout),
  %% Reply will be sent by the connection manager.
  {noreply, State};
handle_call({acquire_connection, _AcquireTimeout}, Client,
      #state{available_connections=[ConnectionManager | OtherConnections]} = State
    ) ->
  %% We'll reuse the most recently returned connection. This favors the situation when we have a few clients
  %% periodically using connections. In such cases, we'll end up keeping as few connections as needed in the
  %% pool.
  ?DEBUG("Reusing an open connection process"),
  cloak_db_conn_manager:assign(ConnectionManager, Client),
  %% Reply will be sent by the connection manager.
  {noreply, State#state{available_connections=OtherConnections}};
handle_call({remove_connection, ConnectionManager}, _From,
      #state{available_connections=AvailableConnections} = State
    ) ->
  case lists:delete(ConnectionManager, AvailableConnections) of
    %% It is possible that the connection is not in the list if was assigned to some client around the
    %% same it decided to terminate itself. By returning false, we're letting the connection know it's about
    %% to be used, so it doesn't terminate.
    AvailableConnections -> {reply, false, State};
    RemainingConnections ->
      ?DEBUG("Removing an idle connection process"),
      %% The connection manager is about to terminate, so we'll unlink to avoid a needless `EXIT` message.
      unlink(ConnectionManager),
      {reply, true, State#state{available_connections=RemainingConnections}}
  end;
handle_call({checkin, ConnectionManager}, _, #state{available_connections=AvailableConnections} = State) ->
  {reply, ok, State#state{available_connections=[ConnectionManager | AvailableConnections]}};
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info({'EXIT', Pid, _Reason}, #state{available_connections=AvailableConnections} = State) ->
  %% Handle possible termination of an idle connection manager.
  {noreply, State#state{available_connections=lists:delete(Pid, AvailableConnections)}};
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

normalize_conn_params([{host, Host}]) -> [{host, Host} | cloak_conf:get_val(cloak_db, connection)];
normalize_conn_params([{host, Host}, {node, Node}]) ->
  [{host, Host} | rpc:call(Node, cloak_conf, get_val, [cloak_db, connection])];
normalize_conn_params(Params) -> Params.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlang_common/include/eunit_helpers.hrl").

pool_test_() ->
  {setup,
    fun() ->
      gproc:start_link(),
      cloak_db_pool_sup:start_link()
    end,
    fun(_) ->
      error_logger:tty(false),
      unlink(whereis(cloak_db_pool_sup)),
      exit(whereis(cloak_db_pool_sup), kill),
      timer:sleep(100),
      error_logger:tty(true)
    end,
    [
      {"basic acquire and release",
        fun() ->
          {ok, Conn1} = acquire_test_connection(),
          {ok, Conn2} = acquire_test_connection(),
          release_connection(Conn1),
          %% We now have one idle connection in the pool so it should be acquired.
          ?assertEqual({ok, Conn1}, acquire_test_connection()),
          %% Now there are no more connections, so a new one should be created.
          {ok, Conn3} = acquire_test_connection(),
          ?assertNotEqual(Conn1, Conn3),
          ?assertNotEqual(Conn2, Conn3),
          stop_pool()
        end
      },
      {"connection timeout",
        fun() ->
          {ok, {ConnectionManager, ConnectionPid} = Conn} = acquire_test_connection(),
          MRefMgr = monitor(process, ConnectionManager),
          MRefConn = monitor(process, ConnectionPid),
          release_connection(Conn),
          %% Verify that connection process and its manager terminated.
          ?assertReceived({'DOWN', MRefMgr, process, ConnectionManager, normal}, 1000),
          ?assertReceived({'DOWN', MRefConn, process, ConnectionPid, shutdown}, 1000),
          %% Subsequent acquire should return a different connection.
          ?assertNotEqual({ok, Conn}, acquire_test_connection()),
          stop_pool()
        end
      },
      {"proper race condition handling on connection expiry",
        fun() ->
          {ok, {ConnectionManager, ConnectionPid} = Conn} = acquire_test_connection(),
          MRefMgr = monitor(process, ConnectionManager),
          MRefConn = monitor(process, ConnectionPid),
          release_connection(Conn),
          %% Hack into the pool state and clear its list of available connections
          sys:replace_state(pool(), fun(State) -> State#state{available_connections=[]} end),
          %% Verify that connection process and its manager terminated.
          ?assertReceived({'DOWN', MRefMgr, process, ConnectionManager, normal}, 1000),
          ?assertReceived({'DOWN', MRefConn, process, ConnectionPid, shutdown}, 1000),
          stop_pool()
        end
      },
      {"client crash terminates the connection",
        fun() ->
          PoolPid = pool(),
          Me = self(),
          error_logger:tty(false),
          %% A client which takes the connection, then exits with some reason.
          Pid = spawn(fun() ->
                receive go_on -> ok end,
                Me ! acquire_test_connection(),
                exit(some_exit_reason)
              end),
          MRefPid = monitor(process, Pid),
          Pid ! go_on,
          {ConnectionManager, ConnectionPid} = Conn = receive {ok, C} -> C after 1000 -> ?assert(false) end,
          MRefMgr = monitor(process, ConnectionManager),
          MRefConn = monitor(process, ConnectionPid),
          %% Verify exits
          ?assertReceived({'DOWN', MRefPid, process, Pid, some_exit_reason}, 1000),
          ?assertReceived({'DOWN', MRefMgr, process, ConnectionManager, some_exit_reason}, 1000),
          %% Connection is always stopped politely by the connection manager, thus the exit reason is shutdown
          ?assertReceived({'DOWN', MRefConn, process, ConnectionPid, shutdown}, 1000),
          %% Subsequent acquire should return a different connection.
          ?assertNotEqual({ok, Conn}, acquire_test_connection()),
          error_logger:tty(true),
          %% Verify that the pool is still alive
          ?assertEqual(PoolPid, pool()),
          stop_pool()
        end
      },
      {"stopping pool terminates all open connections",
        fun() ->
          %% Create a couple of connections
          Conns = [element(2, acquire_test_connection()) || _ <- lists:seq(1, 3)],
          %% Return one connection to the pool
          release_connection(hd(Conns)),
          %% Monitor all connections and their managers
          Pids = lists:flatten([[Pid1, Pid2] || {Pid1, Pid2} <- Conns]),
          MRefs = [monitor(process, Pid) || Pid <- Pids],
          %% Stop the pool
          stop_pool(),
          %% Verify that all processes have been terminated.
          [?assertReceived({'DOWN', MRef, process, _, shutdown}, 1000) || MRef <- MRefs]
        end
      }
    ]
  }.

acquire_test_connection() ->
  acquire_connection(pool(), timer:seconds(1)).

pool() ->
  local_pid(pool_params()).

pool_params() ->
  db_test:conn_params().

stop_pool() ->
  Pool = pool(),
  MRef = erlang:monitor(process, Pool),
  stop(pool_params()),
  receive
    {'DOWN', MRef, process, _, _} -> ok
  after 500 ->
    exit(Pool, kill)
  end.

-endif.
