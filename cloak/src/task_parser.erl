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
    type = parse_type(maps:get(<<"type">>, Task, <<"batch">>)),
    query = parse_query(maps:get(<<"query">>, Task)),
    report_interval = parse_report_interval(maps:get(<<"report_interval">>, Task, undefined)),
    result_destination = parse_return_url(maps:get(<<"return_url">>, Task, undefined)),
    user_expire_interval = maps:get(<<"user_expire_interval">>, Task, undefined),
    period = parse_period(maps:get(<<"period">>, Task, undefined)),
    timestamp = cloak_util:timestamp_to_epoch(os:timestamp())
  }.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

parse_type(<<"batch">>) -> batch;
parse_type(<<"streaming">>) -> streaming;
parse_type(<<"periodic">>) -> periodic.

parse_return_url(undefined) -> air_socket;
parse_return_url(Url) -> {url, Url}.

parse_report_interval(undefined) -> undefined;
parse_report_interval(ReportInterval) -> ReportInterval * 1000.

parse_period(undefined) -> undefined;
parse_period({<<"every">>, Minutes}) ->
  ?EVERY_N_SEC(binary_to_integer(Minutes) * 60);
parse_period({<<"hourly">>, Minute}) ->
  {daily, [{Hour, binary_to_integer(Minute), 0} || Hour <- lists:seq(0, 23)]};
parse_period({<<"daily">>, Hour}) ->
  case binary_to_integer(Hour) of
    Morning when Morning < 12 -> {daily, {Morning, 0, am}};
    12 -> {daily, {12, 0, pm}};
    Afternoon when Afternoon > 12 andalso Afternoon < 24 -> {daily, {Afternoon - 12, 0, pm}}
  end;
parse_period({<<"weekly">>, <<"monday">>}) -> {weekly, mon, {12, pm}};
parse_period({<<"weekly">>, <<"tuesday">>}) -> {weekly, tue, {12, pm}};
parse_period({<<"weekly">>, <<"wednesday">>}) -> {weekly, wed, {12, pm}};
parse_period({<<"weekly">>, <<"thursday">>}) -> {weekly, thu, {12, pm}};
parse_period({<<"weekly">>, <<"friday">>}) -> {weekly, fri, {12, pm}};
parse_period({<<"weekly">>, <<"saturday">>}) -> {weekly, sat, {12, pm}};
parse_period({<<"weekly">>, <<"sunday">>}) -> {weekly, sun, {12, pm}};
parse_period({<<"monthly">>, DayOfMonth}) ->
  {monthly, binary_to_integer(DayOfMonth), {12, pm}}.

parse_query(Query) ->
  % TODO: convert query from string into AST
  % TODO: validate resulting AST
  Query.
