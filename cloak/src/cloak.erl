%% @doc This module primarily containing testing functions,
%%      and means to do simple interactive testing
%%      of cloaked nodes.
-module(cloak).

%% API
-export([
  start/0
]).

-define(MULTICALL_TIMEOUT, 5000).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

-spec start() -> {error, any()} | {ok, any()}.
start() ->
  application:ensure_all_started(cloak).
