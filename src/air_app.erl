%% @doc OTP application behaviour callback module.
-module(air_app).
-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).


%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

%% @hidden
start(_StartType, _StartArgs) ->
  air:load_cloak_config(),
  air_sup:start_link().

%% @hidden
stop(_State) ->
  ok.
