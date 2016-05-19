%% @doc Decodes the map that describes the task and returns the corresponding task specification.
%%
%%      Example of a map describing the task:
%%        ```
%%          {
%%            "task_id": "1234",
%%            "query": "<...>"
%%          }
%%        '''
-module(task_parser).

%% API
-export([
  parse/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Converts the input map to the task specification.
-spec parse(#{}) -> #task{}.
parse(Task) ->
  #task{
    task_id = maps:get(<<"id">>, Task),
    query = parse_query(maps:get(<<"query">>, Task)),
    result_destination = parse_return_url(maps:get(<<"return_url">>, Task, undefined)),
    timestamp = cloak_util:timestamp_to_epoch(os:timestamp())
  }.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

parse_return_url(undefined) -> air_socket;
parse_return_url(Url) -> {url, Url}.

parse_query(Query) ->
  % TODO: convert query from string into AST
  % TODO: validate resulting AST
  Query.
