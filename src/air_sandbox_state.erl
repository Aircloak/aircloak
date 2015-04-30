%% @doc Management of the server-wide state.
-module(air_sandbox_state).

%% API
-export([
  init/0,
  new_analyst_id/0
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Initializes the table where server-wide state is kept.
-spec init() -> ok.
init() ->
  ets:new(air_sandbox, [set, public, named_table, {read_concurrency, true}]),
  ets:insert(air_sandbox, {analyst_id, 0}),
  ok.

%% @doc Generates a new analyst_id. Analysts ids are recycled in 100,000 intervals.
-spec new_analyst_id() -> pos_integer().
new_analyst_id() ->
  ets:update_counter(air_sandbox, analyst_id, {2, 1, 100000, 1}).
