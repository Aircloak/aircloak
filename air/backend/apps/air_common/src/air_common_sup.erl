%% @doc Top level supervisor for the air_common application.
-module(air_common_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    ?CHILD(air_peer_discovery, worker),
    ?CHILD(gen_air_service_registration, worker, [node_registration_data()])
  ]}}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

node_registration_data() ->
  Node = atom_to_list(node()),
  {
    iolist_to_binary(io_lib:format("/service_instances/backend_erlang_nodes/~s", [
        re:replace(Node, "@", "_", [{return, binary}])])),
    Node
  }.
