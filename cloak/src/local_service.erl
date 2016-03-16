%% @doc This module can be used to dynamically register singleton services (processes). This is
%%      occasionally needed when the servers that we need can't be known upfront, and are determined
%%      at runtime. For example, we use this to have a single server per each distinct type of database
%%      connection.
%%
%%      See documentation of {@link get_or_create/3} for details.
-module(local_service).

%% API
-export([
  get_or_create/2,
  get_or_create/3
]).

%% gen_server via callbacks
-export([
  register_name/2,
  unregister_name/1,
  whereis_name/1,
  send/2
]).

-include("cloak.hrl").

-type key() :: any().
-type creator_fun() :: fun((key()) -> {ok, pid()}).

-export_type([
  key/0
]).

%% Internal
% Magical number of retries while trying to acquire the local lock for a service creation.
% The purpose of this is to avoid deadlocks.
-define(LOCK_ACQUIRE_RETRIES, 10).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Same as {@link get_or_create/3}, using five seconds default timeout.
-spec get_or_create(key(), creator_fun()) -> pid().
get_or_create(Key, CreatorFun) -> get_or_create(Key, timer:seconds(5), CreatorFun).

%% @doc Retrieves a process associated with a given key, or creates it using the provided creator function.
%%
%%      Example:
%%        ```
%%        local_service:get_or_create(Key, Timeout,
%%              fun(Key) ->
%%                gen_server:start_link(
%%                      {via, local_service, Key}, % required: registers a process
%%                      ...
%%                    )
%%              end
%%            )
%%        '''
%%
%%      The creator function receives the provided key and must create the process and return `{ok, Pid}'.
%%      The created process must register itself using `local_service:register_name/2'.
%%      This is best done using `via' tuple in combination with gen_server, as illustrated in the example
%%      above.
%%
%%      The function is invoked in the caller process. Isolation per key is guaranteed. You can be sure that
%%      no other process on this node is running the creator function for the given key.
%%
%%      The created process must register itself in at most `Timeout' milliseconds, otherwise an error will
%%      be raised in the caller process.
%%
%%      Internally, this module relies on `gproc' for registration, and `global' for per key local locking.
-spec get_or_create(key(), non_neg_integer(), creator_fun()) -> pid().
get_or_create(Key, Timeout, CreatorFun) ->
  case whereis_name(Key) of
    undefined ->
      local_run_isolated(Key, fun() -> do_get_or_create(Key, Timeout, CreatorFun) end);
    ServicePid -> ServicePid
  end.


%% -------------------------------------------------------------------
%% gen_server via callbacks
%% -------------------------------------------------------------------

%% @hidden
-spec register_name(key(), pid()) -> yes | no.
register_name(Key, Pid) -> gproc:register_name(gproc_key(Key), Pid).

%% @hidden
-spec unregister_name(key()) -> true.
unregister_name(Key) -> gproc:unregister_name(gproc_key(Key)).

%% @hidden
-spec whereis_name(key()) -> pid() | undefined.
whereis_name(Key) -> gproc:whereis_name(gproc_key(Key)).

%% @hidden
-spec send(key(), any()) -> any().
send(Key, Message) -> gproc:send(gproc_key(Key), Message).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

gproc_key(Key) -> {n, l, {local_cloak_service, Key}}.

%% Isolated execution of the provided lambda. It is guaranteed that no concurrent execution of the lambda
%% with the same transaction id takes place on the same node. Multiple processes may of course run this lambda
%% concurrently if different transaction id is provided.
local_run_isolated(TransactionId, Fun) ->
  global:trans({TransactionId, self()}, Fun, [node()], ?LOCK_ACQUIRE_RETRIES).

%% Called after the lock is obtained to create the process.
do_get_or_create(Key, Timeout, CreatorFun) ->
  %% Another recheck is performed to resolve possible race condition.
  case whereis_name(Key) of
    undefined ->
      {ok, ServicePid} = CreatorFun(Key),
      % assert that a process has registered
      {ServicePid, _} = gproc:await(node(), gproc_key(Key), Timeout),
      ServicePid;
    ServicePid -> ServicePid
  end.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

test_creator(LocalName) ->
  timer:sleep(50), % simulates long registration (for race condition test)
  {ok, spawn(fun() -> register_name(LocalName, self()), timer:sleep(1000) end)}.

simple_test_() ->
  {
    setup,
    fun() -> gproc:start_link() end,
    fun(_) -> ok end, % teardown
    [
      {"Basic discovery behavior",
         fun() ->
          Pid = get_or_create(key1, fun test_creator/1),
          ?assertEqual(Pid, get_or_create(key1, fun test_creator/1)),
          ?assertNotEqual(Pid, get_or_create(key2, fun test_creator/1)),
          exit(Pid, terminate),
          ?assertNotEqual(Pid, get_or_create(key1, fun test_creator/1))
        end
      },
      {"Race condition resolved while two processes registering concurrently",
        fun() ->
          Me = self(),
          spawn(fun() -> Pid1 = get_or_create(key3, fun test_creator/1), Me ! {pid, Pid1} end),
          spawn(fun() -> Pid2 = get_or_create(key3, fun test_creator/1), Me ! {pid, Pid2} end),
          Pid1 = receive {pid, P1} -> P1 after 500 -> undefined1 end,
          Pid2 = receive {pid, P2} -> P2 after 500 -> undefined2 end,
          ?assertEqual(Pid1, Pid2)
        end
      },
      {"Process must register",
        fun() ->
          ?assertError(
                timeout,
                get_or_create(key4, 100, fun(_) -> {ok, spawn(fun() -> timer:sleep(1000) end)} end)
              )
        end
      }
    ]
  }.

-endif.
