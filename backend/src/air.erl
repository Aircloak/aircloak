%% @doc Helper for starting application locally
-module(air).

%% API
-export([
  start/0,
  load_cloak_config/0
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the application and all its dependencies.
start() ->
  application:ensure_all_started(air).

%% @doc Loads cloak settings. See `generate_cloak_conf.escript' for details.
load_cloak_config() ->
  [application:set_env(cloak, Prop, Value) || {Prop, Value} <- air_cloak_conf:config()].