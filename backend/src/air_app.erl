%% @doc OTP application behaviour callback module.
-module(air_app).
-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

%% @hidden
start(_StartType, _StartArgs) ->
  air:load_cloak_config(),
  case air_sup:start_link() of
    {ok, Pid} ->
      start_cron_jobs(),
      {ok, Pid};
    Other -> Other
  end.

%% @hidden
stop(_State) ->
  ok.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

start_cron_jobs() ->
  cluster_cron:init(),
  task_purger:setup_cron(),
  ok.
