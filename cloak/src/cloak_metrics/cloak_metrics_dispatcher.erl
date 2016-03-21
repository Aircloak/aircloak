%% @doc A server process that takes care of dispatching statistics to a single
%%      reporter. Handles encoder and reporter errors and implements reconnection logic.
-module(cloak_metrics_dispatcher).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  start_link/1, send/2,
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-include("../cloak.hrl").

%% Internal
-record(state, {
  encoder_fun,
  transport_args,
  transport_state,
  send_fun,
  reconnect_fun,
  reconnect_interval,
  reconnect_queued=false
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec start_link(cloak_metrics:reporter_def()) -> {ok, pid()} | {error, term()}.
%% @doc Starts the server.
start_link(ReporterDef) ->
  gen_server:start_link(?MODULE, ReporterDef, []).

-spec send(pid(), any()) -> ok.
%% @doc Sends the data to the corresponding reporter.
send(Server, Data) ->
  gen_server:cast(Server, {send, Data}).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init(ReporterDef) ->
  self() ! {init, ReporterDef},
  {ok, #state{}}.

%% @hidden
handle_call(Unknown, _, State) ->
  ?ERROR("Unknown call in ~p: ~p", [{?MODULE, Unknown}]),
  {reply, error, State}.

%% @hidden
handle_cast({send, Data}, State) -> {noreply, do_send(Data, State)};
handle_cast(Unknown, State) ->
  ?ERROR("Unknown cast in ~p: ~p", [{?MODULE, Unknown}]),
  {noreply, State}.

%% @hidden
handle_info({init, ReporterDef}, State) -> {noreply, do_init(ReporterDef, State)};
handle_info(reconnect, State) -> {noreply, do_reconnect(State)};
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_init({Encoder, Transport, TransportArgs, ReconnectInterval}, State) ->
  {InitFun, SendFun, ReconnectFun} = transport_funs(Transport),
  call_transport_fun(
        InitFun, [TransportArgs],
        State#state{
          encoder_fun=encoder_fun(Encoder),
          transport_args=TransportArgs,
          send_fun=SendFun,
          reconnect_fun=ReconnectFun,
          reconnect_interval=ReconnectInterval
        }
      ).

transport_funs(TransportModule) when is_atom(TransportModule) ->
  {
    fun TransportModule:init/1,
    fun TransportModule:send/2,
    fun TransportModule:reconnect/2
  };

transport_funs(Lambdas) when is_list(Lambdas) ->
  {
    proplists:get_value(init, Lambdas),
    proplists:get_value(send, Lambdas),
    proplists:get_value(reconnect, Lambdas)
  }.

encoder_fun(Module) when is_atom(Module) -> fun Module:encode/1;
encoder_fun(Fun) when is_function(Fun) -> Fun.

do_send(_, #state{reconnect_queued=true} = State) -> State;
do_send(_, #state{transport_state=undefined} = State) -> State;
do_send(Data, State) ->
  case safe_call(State#state.encoder_fun, [Data]) of
    % Encoder failed. Nothing we can do here, so just ignore, and wait for
    % the next packet. Error is already logged by the safe_call.
    {fail, _} -> State;
    {success, Encoded} ->
      call_transport_fun(
            State#state.send_fun, [Encoded, State#state.transport_state],
            State
          )
  end.

do_reconnect(State) ->
  call_transport_fun(
        State#state.reconnect_fun, [State#state.transport_args, State#state.transport_state],
        State#state{reconnect_queued=false}
      ).

call_transport_fun(Fun, Args, State) ->
  handle_transport_response(safe_call(Fun, Args), State).

safe_call(Fun, Args) ->
  try
    {success, apply(Fun, Args)}
  catch Type:Error ->
    ?ERROR("error: ~p:~p at ~p", [Type, Error, erlang:get_stacktrace()]),
    {fail, {Type, Error}}
  end.

handle_transport_response({success, ok}, State) -> State;
handle_transport_response({success, {ok, TransportState}}, State) ->
  State#state{transport_state=TransportState};
handle_transport_response({success, {error, Reason}}, State) ->
  ?ERROR("transport error ~p", [Reason]),
  queue_reconnect(State);
handle_transport_response({fail, _}, State) ->
  queue_reconnect(State).

queue_reconnect(#state{reconnect_queued=true} = State) -> State;
queue_reconnect(State) ->
  ?INFO("queued reconnect in ~p seconds.", [round(State#state.reconnect_interval / 1000)]),
  erlang:send_after(State#state.reconnect_interval, self(), reconnect),
  State#state{reconnect_queued=true}.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include("eunit_helpers.hrl").

start_dispatcher(Transport, Args) ->
  start_link({
        fun(X) -> X end, Transport, Args, 50
      }).

test_init({transport_args, TestPid}) -> TestPid ! initalized, {ok, {TestPid, 1}}.

test_send(Data, {Pid, Cnt}) -> Pid ! {sent_data, Data, Cnt}, {ok, {Pid, Cnt + 1}}.

basic_test() ->
  {ok, Pid} = start_dispatcher(
        [{init, fun test_init/1}, {send, fun test_send/2}],
        {transport_args, self()}
      ),
  ?assertReceived(initalized, 100),
  ?assertNotReceived({sent_data, _}, 100),
  send(Pid, test_data),
  ?assertReceived({sent_data, test_data, 1}, 100),
  send(Pid, test_data),
  ?assertReceived({sent_data, test_data, 2}, 100).

test_invalid_init(_) -> {error, fail}.

test_reconnect({transport_args, TestPid}, PrevState) -> TestPid ! {reconnect, PrevState}, {ok, {TestPid, 1}}.

reconnect_after_bad_init_test() ->
  'Elixir.Logger':remove_backend(console),
  {ok, Pid} = start_dispatcher(
        [{init, fun test_invalid_init/1}, {send, fun test_send/2}, {reconnect, fun test_reconnect/2}],
        {transport_args, self()}
      ),
  send(Pid, test_data),
  ?assertNotReceived({sent_data, test_data, 1}, 100),
  'Elixir.Logger':add_backend(console, [{flush, true}]),
  ?assertReceived({reconnect, undefined}, 100),
  send(Pid, test_data_2),
  ?assertReceived({sent_data, test_data_2, 1}, 100).

test_send_with_error(error, _) -> {error, fail};
test_send_with_error(throw, _) -> throw(error);
test_send_with_error(Data, {Pid, State}) -> Pid ! {sent_data, Data}, {ok, {Pid, State}}.

test_error(Pid, ErrorType) ->
  send(Pid, ErrorType),
  NotSent = make_ref(),
  send(Pid, ErrorType),
  ?assertNotReceived({sent_data, ErrorType}, 100),
  ?assertNotReceived({sent_data, NotSent}, 100),
  Me = self(),
  ?assertReceived({reconnect, {Me, _}}, 100),
  Sent = make_ref(),
  send(Pid, Sent),
  ?assertReceived({sent_data, Sent}, 100).

reconnect_after_send_test() ->
  {ok, Pid} = start_dispatcher(
        [{init, fun test_init/1}, {send, fun test_send_with_error/2}, {reconnect, fun test_reconnect/2}],
        {transport_args, self()}
      ),
  send(Pid, test_data),
  ?assertReceived({sent_data, test_data}, 100),
  'Elixir.Logger':remove_backend(console),
  test_error(Pid, error),
  test_error(Pid, throw),
  'Elixir.Logger':add_backend(console, [{flush, true}]).

invalid_encoder(throw) -> throw(error);
invalid_encoder(Other) -> Other.

invalid_encoder_test() ->
  {ok, Pid} = start_link({
        fun invalid_encoder/1,
        [{init, fun test_init/1}, {send, fun test_send/2}],
        {transport_args, self()},
        50
      }),
  send(Pid, test_data),
  ?assertReceived({sent_data, test_data, _}, 100),
  'Elixir.Logger':remove_backend(console),
  send(Pid, throw),
  ?assertNotReceived({sent_data, throw, _}, 100),
  'Elixir.Logger':add_backend(console, [{flush, true}]),
  send(Pid, test_data_2),
  ?assertReceived({sent_data, test_data_2, _}, 100).

-endif.
