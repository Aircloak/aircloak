%% @doc This behaviour can be used to run a process that reacts on changes
%%      under some etcd key.
%%
%%      The usage is consistent with OTP behaviours. You need to develop a module
%%      which implements required callbacks. Most important of these is
%%      `handle_key_change' which will be invoked whenever key changes.
%%
%%      Notes:
%%      <ul>
%%        <li>  handle_key_change is invoked on every change, such as set, delete, and
%%              expire. It is the responsibility of a client to fetch the corresponding
%%              value and do something with it.
%%        </li>
%%        <li>
%%          You can supply the parent key (folder). The callback will be invoked
%%          whenever any subkey changes.
%%        </li>
%%      </ul>
-module(gen_etcd_watcher).
-behaviour(gen_server).

%% API functions
-export([
  start_link/3
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

-include("air.hrl").

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
  key :: air_etcd:key(),
  callback_module :: module(),
  callback_state :: callback_state() | undefined,
  req_ref :: reference() | undefined,
  response_body
}).


%% -------------------------------------------------------------------
%% Behaviour definition
%% -------------------------------------------------------------------

-callback init(callback_arg()) -> init_response().
-callback handle_key_change(binary(), callback_state()) -> message_response().
-callback handle_info(term(), callback_state()) -> message_response().
-callback terminate(terminate_reason(), callback_state()) -> term().


%% -------------------------------------------------------------------
%% API Functions
%% -------------------------------------------------------------------

%% @doc Starts the watcher process
-spec start_link(module(), air_etcd:key(), callback_arg()) -> {ok, pid()} | {error, term()}.
start_link(CallbackModule, Key, Arg) ->
  gen_server:start_link(?MODULE, {CallbackModule, Key, Arg}, []).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init({CallbackModule, Key, Arg}) ->
  State = start_request(#state{key=Key, callback_module=CallbackModule}),
  case CallbackModule:init(Arg) of
    {ok, CallbackState} ->
      {ok, State#state{callback_state=CallbackState}};
    {ok, CallbackState, Timeout} ->
      {ok, State#state{callback_state=CallbackState}, Timeout};
    Other -> Other
  end.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
handle_cast({key_changed, Key, ChangeInfo}, #state{key=Key} = State) ->
  handle_callback_response(State, invoke_callback(handle_key_change, [ChangeInfo], State));
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info({hackney_response, ReqRef, Response}, #state{req_ref=ReqRef} = State) ->
  handle_hackney_message(Response, State);
handle_info({hackney_response, _AnotherRequest, _}, State) ->
  {noreply, State};
handle_info(Message, State) ->
  handle_callback_response(State, invoke_callback(handle_info, [Message], State)).

%% @hidden
terminate(Reason, #state{req_ref=ReqRef} = State) ->
  hackney:close(ReqRef),
  invoke_callback(terminate, [Reason], State).

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

start_request(#state{key=Key} = State) ->
  Url = iolist_to_binary([air_etcd:url(), "/v2/keys", Key,
      air_utils:url_params([{wait, true}, {recursive, true}])]),
  {ok, ReqRef} = hackney:request(get, Url, [], "", [async, {recv_timeout, infinity}]),
  State#state{req_ref=ReqRef, response_body=""}.

handle_hackney_message({status, _, _}, State) -> {noreply, State};
handle_hackney_message({headers, _}, State) -> {noreply, State};
handle_hackney_message(Chunk, #state{response_body=ResponseBody} = State) when is_binary(Chunk) ->
  {noreply, State#state{response_body=[ResponseBody, Chunk]}};
handle_hackney_message(done, State) ->
  handle_etcd_response(State);
handle_hackney_message(Error, State) ->
  ?ERROR("HTTP error while watching etcd key ~p: ~p", [State#state.key, Error]),
  {stop, http_error, State}.

handle_etcd_response(#state{response_body=ResponseBody} = State) ->
  % immediately start the next request
  NewState = start_request(State),
  % Parse data and invoke callback function
  {struct, Response} = mochijson2:decode(ResponseBody),
  {struct, Node} = proplists:get_value(<<"node">>, Response),
  ChangedKey = proplists:get_value(<<"key">>, Node),
  NewCallbackState = invoke_callback(handle_key_change, [ChangedKey], State),
  handle_callback_response(NewState, NewCallbackState).

handle_callback_response(State, {noreply, CallbackState}) ->
  {noreply, update_callback_state(State, CallbackState)};
handle_callback_response(State, {noreply, CallbackState, Timeout}) ->
  {noreply, update_callback_state(State, CallbackState), Timeout};
handle_callback_response(State, {stop, Reason, CallbackState}) ->
  hackney:close_request(State#state.req_ref),
  {stop, Reason, update_callback_state(State#state{req_ref=undefined}, CallbackState)}.

invoke_callback(Fun, Args, #state{callback_module=CallbackModule, callback_state=CallbackState}) ->
  apply(CallbackModule, Fun, Args ++ [CallbackState]).

update_callback_state(State, CallbackState) ->
  State#state{callback_state=CallbackState}.
