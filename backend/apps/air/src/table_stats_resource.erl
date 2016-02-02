%% @doc Handles table stats
-module(table_stats_resource).

%% webmachine callbacks
-export([
  init/1,
  allowed_methods/2,
  post_is_create/2,
  process_post/2
]).

-include("air.hrl").
-include_lib("webmachine/include/webmachine.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, nostate}.

%% @hidden
allowed_methods(Req, State) -> {['POST'], Req, State}.

%% @hidden
post_is_create(Req, State) -> {false, Req, State}.

%% @hidden
process_post(Req, State) ->
  handle_action(post, wrq:path_info(action, Req), Req, State).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

handle_action(post, "compute", Req, State) ->
  Data = mochijson2:decode(wrq:req_body(Req)),
  Status = table_stats_calculator:run(
        ej:get({"analyst"}, Data),
        ej:get({"table_id"}, Data),
        ej:get({"cloak_url"}, Data),
        ej:get({"task_spec"}, Data)
      ),
  {{halt, 200}, resource_common:respond_json([{status, Status}], Req), State}.
