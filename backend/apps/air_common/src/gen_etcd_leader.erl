%% @doc Generic leader behaviour powered by etcd.
%%      This module relies on etcd to implement a strongly consistent leader
%%      election. By using etcd, we keep the behaviour consistent with the
%%      rest of the air system, and rely on the mature Raft implementation.
%%
%%      Initially, each process tries to proclaim itself as a leader, but only
%%      one will in fact succeed (guaranteed by etcd atomic operations). That
%%      instance is then considered as the leader, while others are followers.
%%
%%      Internally, the leader process has a limited lease which it tries to
%%      periodically renew before it expires. In normal mode of operations, this
%%      renewal will succeed. If something goes wrong (for example the OS process
%%      terminates), the lease will expire, and we'll have the new elections.
%%
%%      The callback module can perform custom actions in corresponding callback
%%      functions. These actions will usually involve starting or stopping the
%%      service-specific process, and (un)setting some system-wide etcd
%%      registration key.
-module(gen_etcd_leader).
-behaviour(gen_etcd_watcher).

%% API functions
-export([
  start_link/3
]).

%% gen_etcd_watcher callbacks
-export([
  init/1,
  handle_key_change/2,
  handle_info/2,
  terminate/2
]).

-include("air.hrl").

-define(RENEW_INTERVAL, timer:seconds(10)).
-define(LEASE_SEC, 30).

-type callback_arg() :: term().
-type callback_state() :: term().
-type terminate_reason() :: normal | shutdown | {shutdown, term()} | term().
-type timeout_spec() :: non_neg_integer() | infinity | hibernate.
-type init_response() ::
  {ok, callback_state()} |
  {ok, callback_state(), timeout_spec()} |
  {stop, term()} |
  ignore.
-type message_response() ::
  {noreply, callback_state()} |
  {noreply, callback_state(), timeout_spec()} |
  {stop, term(), callback_state()}.

-record(state, {
  election_key :: air_etcd:key(),
  service_name :: string() | binary(),
  callback_module :: module(),
  callback_state :: callback_state() | undefined,
  unique_id :: binary(),
  timer_ref :: timer:tref() | undefined,
  status :: undefined | leader | follower
}).


%% -------------------------------------------------------------------
%% Behaviour definition
%% -------------------------------------------------------------------

-callback init(callback_arg()) -> init_response().
-callback handle_leader(callback_state()) -> {ok, callback_state()}.
-callback handle_follower(callback_state()) -> {ok, callback_state()}.
-callback handle_info(term(), callback_state()) -> message_response().
-callback terminate(terminate_reason(), callback_state()) -> term().


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the leader candidate process.
%%      The `ServiceName' must be the same for all processes which aspire to be leader
%%      for the particular service.
-spec start_link(module(), string() | binary(), callback_arg()) -> {ok, pid()} | {error, term()}.
start_link(CallbackModule, ServiceName, Arg) ->
  gen_etcd_watcher:start_link(?MODULE, election_key(ServiceName), {CallbackModule, ServiceName, Arg}).


%% -------------------------------------------------------------------
%% gen_etcd_watcher callbacks
%% -------------------------------------------------------------------

init({CallbackModule, ServiceName, Arg}) ->
  process_flag(trap_exit, true),
  State = #state{
    election_key=election_key(ServiceName),
    service_name=ServiceName,
    callback_module=CallbackModule,
    unique_id=base64:encode(term_to_binary({node(), self(), crypto:rand_bytes(100)}))
  },
  case CallbackModule:init(Arg) of
    {ok, CallbackState} ->
      {ok, check_status(State#state{callback_state=CallbackState})};
    {ok, CallbackState, Timeout} ->
      {ok, check_status(State#state{callback_state=CallbackState}), Timeout};
    Other -> Other
  end.

handle_key_change(ElectionKey, #state{election_key=ElectionKey} = State) ->
  {noreply, check_status(State)}.

handle_info(renew_registration, #state{election_key=ElectionKey, unique_id=UniqueId} = State) ->
  NewState = case air_etcd:compare_and_swap(ElectionKey, UniqueId, UniqueId, ?LEASE_SEC) of
    ok -> act_as_leader(State);
    error -> act_as_follower(State)
  end,
  {noreply, NewState};
handle_info(Message, State) ->
  case invoke_callback(handle_info, [Message], State) of
    {noreply, CallbackState} ->
      {noreply, update_callback_state(State, CallbackState)};
    {noreply, CallbackState, Timeout} ->
      {noreply, update_callback_state(State, CallbackState), Timeout};
    {stop, Reason, CallbackState} ->
      {stop, Reason, update_callback_state(State, CallbackState)}
  end.

terminate(Reason, #state{election_key=ElectionKey, unique_id=UniqueId} = State) ->
  air_etcd:compare_and_delete(ElectionKey, UniqueId),
  invoke_callback(terminate, [Reason], State),
  ok.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

election_key(ServiceName) ->
  iolist_to_binary(["/leader_election/", ServiceName]).

check_status(#state{election_key=ElectionKey, unique_id=UniqueId} = State) ->
  case air_etcd:fetch(ElectionKey) of
    error -> try_register_as_leader(State);
    {ok, UniqueId} -> act_as_leader(State);
    {ok, _} -> act_as_follower(State)
  end.

try_register_as_leader(#state{election_key=ElectionKey, unique_id=UniqueId} = State) ->
  case air_etcd:create_new(ElectionKey, UniqueId, ?LEASE_SEC) of
    ok -> act_as_leader(State);
    error -> check_status(State)
  end.

act_as_leader(#state{status=leader} = State) -> State;
act_as_leader(#state{status=_, service_name=ServiceName} = State) ->
  ?INFO("acting as leader for ~s", [ServiceName]),
  State1 = State#state{status=leader},
  State2 = start_expiry_timer(State1),
  {ok, CallbackState} = invoke_callback(handle_leader, [], State2),
  update_callback_state(State2, CallbackState).

act_as_follower(#state{status=follower} = State) -> State;
act_as_follower(#state{status=_, service_name=ServiceName} = State) ->
  ?INFO("acting as follower for ~s", [ServiceName]),
  State1 = State#state{status=follower},
  State2 = stop_expiry_timer(State1),
  {ok, CallbackState} = invoke_callback(handle_follower, [], State2),
  update_callback_state(State2, CallbackState).

start_expiry_timer(State) ->
  State#state{timer_ref=timer:send_interval(?RENEW_INTERVAL, renew_registration)}.

stop_expiry_timer(#state{timer_ref=TimerRef} = State) ->
  timer:cancel(TimerRef),
  State#state{timer_ref=undefined}.

invoke_callback(Fun, Args, #state{callback_module=CallbackModule, callback_state=CallbackState}) ->
  apply(CallbackModule, Fun, Args ++ [CallbackState]).

update_callback_state(State, CallbackState) ->
  State#state{callback_state=CallbackState}.
