-module(airpub_app).
-behaviour(application).

-include_lib("cloak/src/cloak.hrl").

%% Application callbacks
-export([
  start/2,
  stop/1
]).


%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  airpub_sup:start_link().

stop(_State) ->
  ok.
