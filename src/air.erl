%% @doc Helper for starting application locally
-module(air).

%% API
-export([
  start/0,
  load_cloak_config/0
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the application and all its dependencies.
start() ->
  application:ensure_all_started(air).

%% @doc Loads cloak settings. See `generate_cloak_conf.escript' for details.
load_cloak_config() ->
  [application:set_env(cloak, Prop, Value) || {Prop, Value} <- air_cloak_conf:config()],
  % Plug into the job_inserter with our custom mock.
  JI = lists:foldl(
        fun({K, _} = El, Acc) -> lists:keystore(K, 1, Acc, El) end,
        [application:get_env(cloak, job_inserter, [])],
        [{validation_module, job_insert_mock}, {insert_module, job_insert_mock}]
      ),
  application:set_env(cloak, job_inserter, JI).