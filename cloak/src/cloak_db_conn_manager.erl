%% @doc Manages the life cycle of the connection.
%%
%%      This process creates a database connection and sends its pid to clients. The pool will manage a list
%%      of idle connection managers, and assign one to the client. If there are no idle managers, the new
%%      one is created and assigned to the client.
%%
%%      Assigning to the client is done from the manager process. It sets up monitor to the client process and
%%      then sends the connection pid to the client. The client therefore communicates directly with the
%%      connection, while the manager concurrently takes care of the proper cleanup.
%%
%%      This lifts some responsibility from the pool. In particular, the pool doesn't need to handle client
%%      crashes. It also doesn't need to wait until the connection is open. Finally, it doesn't need to manage
%%      connection expiry. All of this is done by the manager, which allows us to manage multiple connections
%%      concurrently, and thus more efficiently.
%%
%%      Once the client is done, it will perform the check in through the connection manager (wrapped via
%%      {@link cloak_db_pool:release_connection/1}). As a result, the connection manager will report to the
%%      pool that it is idle. The manager will also start an internal idle timer. This timer is cleared if the
%%      connection is assigned to another client. Otherwise, if the timeout occurs, the manager will deregister
%%      from the pool and close the connection.
-module(cloak_db_conn_manager).
-behaviour(gen_server).

%% API
-export([
  start_link/4,
  release/1,
  assign/2
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
  pool_server :: pid(),
  conn_pid = undefined :: undefined | pid(),
  client_mref = undefined :: undefined | reference(),
  idle_timer_ref = undefined :: undefined | reference()
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the connection manager, opens the connection, and sends its pid to the client.
%%      Note: the opening of the connection is done after this function returns, to avoid blocking the
%%      pool server.
-spec start_link(pid(), {pid(), reference()}, cloak_db:connection_params(), pos_integer()) -> {ok, pid()}.
start_link(PoolServer, Client, ConnectionParams, AcquireTimeout) ->
  gen_server:start_link(?MODULE, {PoolServer, Client, ConnectionParams, AcquireTimeout}, []).

%% @doc Releases the assigned connection and returns it to the pool.
-spec release(pid()) -> ok.
release(ConnectionManager) ->
  %% This is synchronous to make the client wait until the connection is returned to the pool. This plays
  %% nicely with the db_job queue, making sure that we have a connection available when the client decrements
  %% the jobs counter.
  gen_server:call(ConnectionManager, release).

%% @doc Assigns the already open connection to the client.
-spec assign(pid(), {pid(), reference()}) -> ok.
assign(ConnectionManager, Client) ->
  gen_server:cast(ConnectionManager, {assign, Client}).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init({PoolServer, Client, ConnectionParams, AcquireTimeout}) ->
  process_flag(trap_exit, true),
  %% Immediately monitor the client so we can react if it dies
  State = monitor_client(Client, #state{pool_server=PoolServer}),
  %% Deferred opening of the connection so the pool is not blocked.
  erlang:send_after(0, self(), {connect_and_notify, Client, ConnectionParams, AcquireTimeout}),
  %% We'll start monitoring the client immediately.
  {ok, State}.

%% @hidden
handle_call(release, _, #state{pool_server=PoolServer, client_mref=ClientMRef} = State) ->
  demonitor(ClientMRef),
  cloak_db_pool:checkin(PoolServer, self()),
  {
    reply,
    ok,
    State#state{
      client_mref=undefined,
      idle_timer_ref=erlang:start_timer(cloak_conf:get_val(cloak_db, idle_timeout), self(), expire)
    }
  };
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
handle_cast({assign, Client}, #state{conn_pid=ConnPid, idle_timer_ref=TRef} = State) ->
  %% Immediately monitor the client so we can react if it dies
  State1 = monitor_client(Client, State#state{idle_timer_ref=undefined}),
  gen_server:reply(Client, {ok, {self(), ConnPid}}),
  case TRef of
    undefined -> ok;
    _ -> erlang:cancel_timer(TRef)
  end,
  cloak_metrics:count("cloak_db.connection.reuse"),
  {noreply, State1};
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info({connect_and_notify, Client, ConnectionParams, AcquireTimeout}, State) ->
  %% We start a separate process which deals with the timeout. If the connection is not established
  %% in the given time, that process will kill this manager, and inform the client about the timeout.
  StopperPid = spawn_timeout_stopper(self(), Client, AcquireTimeout),
  {ok, ConnPid} = sql_conn:start_link(ConnectionParams),
  %% We're here, so first we need to unlink from the stopper and terminate it.
  unlink(StopperPid),
  exit(StopperPid, kill),
  %% Notify the client.
  gen_server:reply(Client, {ok, {self(), ConnPid}}),
  cloak_metrics:count("cloak_db.connection.create"),
  {noreply, State#state{conn_pid=ConnPid}};
handle_info({timeout, TRef, expire}, #state{idle_timer_ref=TRef, pool_server=PoolServer} = State) ->
  %% Expired -> try to remove the connection from the pool and stop the server
  case cloak_db_pool:remove_connection(PoolServer, self()) of
    true ->
      %% Connection successfully removed -> stop the server
      cloak_metrics:count("cloak_db.connection.expire"),
      {stop, normal, State};
    false ->
      %% Connection was not in the pool. This will happen if the pool assigned the connection to a client at
      %% the same time the connection expired. We won't stop, and will instead wait for the client to connnect.
      %% A grace period is given to the connecting client to arrive. If this doesn't happen, we'll terminate.
      {noreply, State, cloak_conf:get_val(cloak_db, idle_timeout)}
  end;
handle_info({'DOWN', ClientMRef, process, _, Reason}, #state{client_mref=ClientMRef} = State) ->
  %% Client has terminated, -> stop with the same reason.
  {stop, Reason, State#state{client_mref=undefined}};
handle_info({'EXIT', ConnPid, Reason}, #state{conn_pid=ConnPid} = State) ->
  %% Connection has terminated -> stop with the same reason.
  {stop, Reason, State#state{conn_pid=undefined}};
handle_info({'EXIT', _OtherPid, Reason}, State) when Reason =/= normal ->
  %% Some other linked process has died abnormally. Shouldn't really happen, but just to be on the safe side,
  %% we'll terminate here as well.
  {stop, Reason, State};
handle_info(timeout, State) ->
  %% Connecting client didn't arrive -> stop the server.
  %% See `handle_info({timeout, TRef, expire}, ...)` for explanation.
  cloak_metrics:count("cloak_db.connection.expire"),
  {stop, normal, State};
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
  %% Politely stopping the connection similarly to what a supervisor does.
  %% Theoretically, we don't need to to this, since the connection is our OTP child, so it will stop when
  %% we stop. However, this will happen only when the connection is idle. So if it's running some intensive
  %% query, it might waste resources in vain. Therefore, we try to politely stop the connection, and if that
  %% fails, we brutally kill it.
  case State#state.conn_pid of
    undefined -> ok;
    ConnPid ->
      MRef = erlang:monitor(process, ConnPid),
      exit(ConnPid, shutdown),
      receive
        {'DOWN', MRef, process, _, _} -> ok
      after 1000 ->
        %% Connection refused to terminate (perhaps it's busy), so let's kill it brutally.
        exit(ConnPid, kill)
      end
  end.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

monitor_client({Pid, _Tag}, #state{client_mref=undefined} = State) ->
  State#state{client_mref=monitor(process, Pid)}.

spawn_timeout_stopper(ConnectionManager, Client, AcquireTimeout) ->
  spawn_link(
        fun() ->
          timer:sleep(AcquireTimeout),
          unlink(ConnectionManager),
          exit(ConnectionManager, kill),
          gen_server:reply(Client, {error, timeout})
        end
      ).