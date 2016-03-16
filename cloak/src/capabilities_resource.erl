%% @doc HTTP API endpoint for querying the capabilities of
%%      a cloak-core installation.
%%
%%      In local development it is used to query the cluster
%%      directly, whereas on real-cloaks it is used by
%%      manny-core.
%%
%%
%%      Example request:
%%
%%        ```
%%          curl http://localhost:8098/capabilities
%%        '''
-module(capabilities_resource).

%% webmachine callbacks
-export([
  init/1,
  content_types_provided/2,
  to_json/2
]).

-include("cloak.hrl").
-include_lib("webmachine/include/webmachine.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, nostate}.

content_types_provided(Req, State) ->
  {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
  {mochijson2:encode([{success, true}, {capabilities, capabilities()}]), Req, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

capabilities() ->
  [
    % allows PUT and DELETE on task resource to add and remove streaming tasks
    streaming_queries,
    % allows PUT and DELETE on task resource to add and remove periodic tasks
    periodic_queries
  ].


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

capabilities_test() ->
  Pid = server_tester:start(),
  ?assertMatch({_, {{_, 200, _}, _, _}}, server_tester:get("capabilities")),
  server_tester:stop(Pid).

-endif.
