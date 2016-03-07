%% @doc In charge of renewing the service registration in etcd.
%%      This module will start a dedicated process and periodically insert
%%      provided registration data
-module(gen_air_service_registration).
-behaviour(gen_server).

%% Internal API
-export([
  start_link/1
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-define(RENEW_INTERVAL, timer:seconds(10)).
-define(REGISTRATION_EXPIRY_SEC, 30).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link({Key :: string() | binary(), Data :: string() | binary()}) -> {ok, pid()} | {error, term()}.
start_link(RegistrationData) ->
  gen_server:start_link(?MODULE, RegistrationData, []).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(KeyAndData) ->
  process_flag(trap_exit, true),
  {ok, renew_registration({KeyAndData, false}), ?RENEW_INTERVAL}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(timeout, State) ->
  {noreply, renew_registration(State), ?RENEW_INTERVAL};
handle_info(_, State) -> {noreply, State, ?RENEW_INTERVAL}.

%% @hidden
terminate(Reason, {{Key, _Data}, _PreviouslyRegistered}) ->
  ?INFO("Deregistering ~s due to ~p", [Key, Reason]),
  air_etcd:delete(Key),
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

renew_registration({{Key, Data}, PreviouslyRegistered}) ->
  case air_etcd:set(Key, Data, ?REGISTRATION_EXPIRY_SEC) of
    {ok, #set{}} ->
      if
        not PreviouslyRegistered -> ?INFO("Registered ~p", [Key]);
        true -> ok
      end,
      {{Key, Data}, true};
    Error ->
      ?ERROR("Error registering the service ~s: ~p", [Key, Error]),
      {{Key, Data}, false}
  end.
