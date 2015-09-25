%% @doc A server which detects peers and tries to connect to them.
-module(air_peer_discovery).
-behaviour(gen_server).

%% Internal API
-export([
  start_link/0
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

-define(POLL_INTERVAL, timer:seconds(10)).

-record(state, {
  poller=undefined
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, undefined, []).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  process_flag(trap_exit, true),
  net_kernel:monitor_nodes(true),
  erlang:send_after(0, self(), poll_peers),
  {ok, #state{}}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(poll_peers, State) ->
  {noreply, State#state{poller=spawn_link(fun() -> poll_peers() end)}};
handle_info({'EXIT', Poller, _}, #state{poller=Poller} = State) ->
  erlang:send_after(?POLL_INTERVAL, self(), poll_peers),
  {noreply, State#state{poller=undefined}};
handle_info({nodeup, Node}, State) ->
  ?INFO("Connected to ~p", [Node]),
  {noreply, State};
handle_info({nodedown, Node}, State) ->
  ?INFO("Disconnected from ~p", [Node]),
  {noreply, State};
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

poll_peers() ->
  [join(Peer) ||
    {_Key, NodeStr} <- air_etcd:ls("/service_instances/backend_erlang_nodes"),
    Peer <- [binary_to_atom(NodeStr, utf8)],
    not lists:any(fun(X) -> X =:= Peer end, nodes([this, visible]))
  ],
  ok.

join(Peer) ->
  case net_adm:ping(Peer) of
    pong -> true;
    _ ->
      ?WARNING("Failed connecting to peer ~p", [Peer]),
      false
  end.
