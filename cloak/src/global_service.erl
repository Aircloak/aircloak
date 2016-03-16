%% @doc This module can be used to dynamically register global services (processes). This is
%%      occasionally needed when the servers that we need can't be known upfront, and are determined
%%      at runtime. For example, we use this to manage cluster-wide caches of task buckets.
%%
%%      See documentation of {@link get_or_create/3} for details.
-module(global_service).

%% API
-export([
  get_or_create/2,
  get_or_create/3,
  stop/1
]).

%% Internal API (needed for rpc calls)
-export([
  get_or_create_local/2,
  resolve_conflict/3
]).

%% gen_server via callbacks
-export([
  register_name/2,
  unregister_name/1,
  whereis_name/1,
  send/2
]).

-ifdef(TEST).
  %% Needed for test purposes
  -export([
    start_test_service/2,
    init_test_service/3,
    dont_register_test_service/1
  ]).
-endif.

-include("cloak.hrl").

-type key() :: any().

%% Internal
% Magical number of retries while trying to acquire the local lock for a service creation.
% The purpose of this is to avoid deadlocks.
-define(LOCK_ACQUIRE_RETRIES, 10).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Works like {@link get_or_create/3}, using all online nodes in the ring as candidates.
-spec get_or_create(key(), {module(), atom(), [any()]}) -> pid().
get_or_create(Key, CreatorMfa) ->
  {M, F} = cloak_conf:get_val(global_service, default_nodes),
  get_or_create(Key, apply(M, F, []), CreatorMfa).

%% @doc Retrieves the process associated with the given key, or starts it using the provided starter function.
%%      If created, the process is supervised under the {@link global_service_sup} supervisor.
%%
%%      Example:
%%        ```
%%        global_service:get_or_create(
%%              Key,
%%              nodes([visible, this]),
%%              {my_service, start_link, [arg1, arg2]}
%%            )
%%        '''
%%
%%      Service module:
%%        ```
%%        -module(my_service).
%%
%%        ...
%%
%%        start_link(Key, Arg1, Args) ->
%%          gen_server:start_link(
%%              {via, global_service, Key}, % required: registers a process
%%              ...
%%            ).
%%
%%        ...
%%        '''
%%
%%      The `CandidateNodes' argument contains the list of nodes on which the service can be created. The process
%%      will be created on one of these nodes. Assuming a constant list of nodes, a service with a particular
%%      key is always created on the same node. This approach ensures fair cluster distribution, since hashing
%%      per key is used to select the host node.
%%
%%      The `CreatorMfa' identifies the function that starts the process. This function will receive one
%%      extra argument in the first place - the service key. The created process must be registered via
%%      `global_service:register_name/2', using the provided key. This is best done using `via' tuple in
%%      combination with `gen_server:start_link' function.
%%
%%      Isolation per key is guaranteed. You can be sure that no other process on the target node is
%%      running the creator function for the given key.
%%
%%      The process is registered via `global'. Lookups to existing services will therefore be fast, since
%%      they amount to a local ETS lookup.
-spec get_or_create(key(), [node()], {module(), atom(), [any()]}) -> pid().
get_or_create(Key, [_|_] = CandidateNodes, CreatorMfa) ->
  case whereis_name(Key) of
    undefined ->
      TargetNode = lists:nth(erlang:phash2(Key, length(CandidateNodes)) + 1, CandidateNodes),
      case rpc:call(TargetNode, ?MODULE, get_or_create_local, [Key, CreatorMfa]) of
        {ok, ServicePid} -> ServicePid;
        {error, Error} ->
          ?ERROR("Error starting global service ~p on ~p: ~p", [Key, TargetNode, Error]),
          error({global_service_start, Error});
        aborted ->
          ?ERROR("Aborted starting global service ~p", [Key]),
          error({global_service_start, aborted})
      end;
    ServicePid -> ServicePid
  end.

%% @doc Stops the service.
-spec stop(key()) -> ok.
stop(Key) ->
  case whereis_name(Key) of
    undefined -> ok;
    Pid ->
      rpc:call(erlang:node(Pid), global_service_sup, stop_service, [Key], timer:seconds(15)),
      % In case rpc timed out for some reason
      exit(Pid, kill)
  end,
  ok.


%% -------------------------------------------------------------------
%% gen_server via callbacks
%% -------------------------------------------------------------------

%% @hidden
-spec register_name(key(), pid()) -> yes | no.
register_name(Key, Pid) -> global:register_name(global_key(Key), Pid, fun global_service:resolve_conflict/3).

%% @hidden
-spec unregister_name(key()) -> true.
unregister_name(Key) -> global:unregister_name(global_key(Key)).

%% @hidden
-spec whereis_name(key()) -> pid() | undefined.
whereis_name(Key) -> global:whereis_name(global_key(Key)).

%% @hidden
-spec send(key(), any()) -> pid().
send(Key, Message) -> global:send(global_key(Key), Message).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

global_key(Key) -> {global_service, Key}.

%% @hidden
%% This function runs on the node where the process is supposed to exist.
get_or_create_local(Key, CreatorMfa) ->
  run_isolated(Key, fun() -> do_get_or_create(Key, CreatorMfa) end).

%% Isolated execution of the provided lambda. It is guaranteed that no concurrent execution of the lambda
%% with the same transaction id takes place on the same node. Multiple processes may of course run this lambda
%% concurrently if a different transaction id is provided.
%%
%% Note that we're isolating only on the current node. This is enough, because this function runs on the
%% target node, so there's no need to perform cluster-wide locking.
run_isolated(TransactionId, Fun) ->
  global:trans({{global_service, TransactionId}, self()}, Fun, [node()], ?LOCK_ACQUIRE_RETRIES).

%% Called after the lock is obtained to create the process.
do_get_or_create(Key, CreatorMfa) ->
  % Another recheck is performed to resolve possible race condition.
  case whereis_name(Key) of
    undefined ->
      case global_service_sup:start_service(Key, CreatorMfa) of
        % Edge case that can happen when running during cluster reconfiguration. If the service is
        % already running, we'll just return its pid.
        {error, {already_started, ServicePid}} ->
          {ok, ServicePid};
        {error, Error} ->
          ?ERROR("Error starting global service ~p: ~p", [Key, Error]),
          {error, Error};
        {ok, ServicePid} ->
          % assert that a process has registered
          case whereis_name(Key) of
            ServicePid -> {ok, ServicePid};
            _ ->
              global_service_sup:stop_service(Key),
              {error, not_registered}
          end
      end;
    ServicePid -> {ok, ServicePid}
  end.

%% @hidden
%% This function is called if two disconnected nodes join and each one runs the service with a particular
%% name. When those nodes connect, we have a conflict which is resolved simply by terminating both processes
%% with a shutdown reason. The client is responsible to start the service again. Usually, this will happen
%% automatically on the next call to get_or_create.
resolve_conflict(_, Pid1, Pid2) ->
  exit(Pid1, {shutdown, global_service_conflict}),
  exit(Pid2, {shutdown, global_service_conflict}).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

start_test_service(Key, Opt) ->
  % We start the process synchronously via proc_lib (the same mechanism used by OTP processes). The function
  % returns only after the spawned process has acknowledged that it has been started.
  % This allows us to return only after the process has registered.
  proc_lib:start_link(?MODULE, init_test_service, [self(), Key, Opt]).

% Called internally by proc_lib to initialize the process and acknowledge to the parent.
init_test_service(Parent, Key, {delay, Timeout}) ->
  timer:sleep(Timeout),
  init_test_service(Parent, Key, undefined);
init_test_service(Parent, Key, _) ->
  % First we register, then we acknowledge to the parent.
  case register_name(Key, self()) of
    yes ->
      proc_lib:init_ack(Parent, {ok, self()}),
      timer:sleep(infinity);
    no ->
      proc_lib:init_ack(Parent, {error, {already_started, whereis_name(Key)}})
  end.

dont_register_test_service(_Key) ->
  {ok, spawn_link(fun() -> timer:sleep(infinity) end)}.

global_service_test_() ->
  StartMfa = {?MODULE, start_test_service, [undefined]},
  {
    setup,
    fun() -> global_service_sup:start_link() end,
    fun({ok, GlobalServiceSupPid}) ->
      unlink(GlobalServiceSupPid),
      exit(GlobalServiceSupPid, kill)
    end,
    [
      {"Basic discovery behavior",
        fun() ->
          Pid = get_or_create(key1, [node()], StartMfa),
          ?assertEqual(Pid, get_or_create(key1, [node()], StartMfa)),
          ?assertNotEqual(Pid, get_or_create(key2, [node()], StartMfa)),
          stop(key1),
          ?assertNotEqual(Pid, get_or_create(key1, [node()], StartMfa))
        end
      },
      {"Process must register",
        fun() ->
          ?assertError(
                {global_service_start, not_registered},
                get_or_create(key4, [node()], {?MODULE, dont_register_test_service, []})
              )
        end
      },
      {"Resolve kills both processes",
        fun() ->
          Pid1 = get_or_create(key1, [node()], StartMfa),
          Pid2 = get_or_create(key2, [node()], StartMfa),
          ?assertNotEqual(undefined, whereis_name(key1)),
          ?assertNotEqual(undefined, whereis_name(key2)),
          resolve_conflict(foo, Pid1, Pid2),
          ?assertEqual(undefined, whereis_name(key1)),
          ?assertEqual(undefined, whereis_name(key2)),
          ?assertNotEqual(Pid1, get_or_create(key1, [node()], StartMfa))
        end
      }
    ]
  }.

-endif.