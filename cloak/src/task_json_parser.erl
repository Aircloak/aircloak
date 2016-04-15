%% @doc Decodes json that describes the task and returns the corresponding task specification.
%%
%%      Example of a json describing the task:
%%        ```
%%          {
%%            "prefetch": [
%%              {
%%                "table": "test1",
%%                "columns": ["name", "priority"],
%%                "where": {"\$\$priority": {"$lt": 3}}
%%              }
%%            ],
%%            "post_processing": {
%%              "code":"report_property(tables.test1[0].name, tables.test1[0].priority",
%%
%%              // libraries are optional
%%              "libraries":[
%%                {"name": "lib1", "code": "function foo() ... end"},
%%                {"name": "lib2", "code": "function bar() ... end"}
%%              ]
%%            }
%%          }
%%        '''
-module(task_json_parser).

%% API
-export([
  parse/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Converts the input json to the task specification.
-spec parse(binary()) -> #task{}.
parse(Json) ->
  Proplist = [map_top_level_value(Property) ||
    Property <- cloak_util:destructure_parsed_json(mochijson2:decode(Json))
  ],
  #task{
    task_id = proplists:get_value(task_id, Proplist, batch),
    type = proplists:get_value(type, Proplist, batch),
    prefetch = proplists:get_value(prefetch, Proplist),
    code = strip_comments(proplists:get_value(code, proplists:get_value(post_processing, Proplist))),
    libraries = proplists:get_value(libraries, proplists:get_value(post_processing, Proplist), []),
    report_interval = proplists:get_value(report_interval, Proplist),
    user_expire_interval = proplists:get_value(user_expire_interval, Proplist),
    period = proplists:get_value(period, Proplist),
    timestamp = cloak_util:timestamp_to_epoch(os:timestamp())
  }.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

map_top_level_value({<<"task_id">>, TaskId}) -> {task_id, TaskId};
map_top_level_value({<<"type">>, <<"batch">>}) -> {type, batch};
map_top_level_value({<<"type">>, <<"streaming">>}) -> {type, streaming};
map_top_level_value({<<"type">>, <<"periodic">>}) -> {type, periodic};
map_top_level_value({<<"post_processing">>, PostProcessing}) ->
  {post_processing, [map_post_processing(PostProcessingElement) || PostProcessingElement <- PostProcessing]};
map_top_level_value({<<"prefetch">>, Prefetch}) ->
  {prefetch, [map_table_spec(TableSpec) || TableSpec <- Prefetch]};
map_top_level_value({<<"report_interval">>, ReportInterval}) ->
  {report_interval, ReportInterval * 1000};
map_top_level_value({<<"user_expire_interval">>, UserExpireInterval}) ->
  {user_expire_interval, UserExpireInterval};
map_top_level_value({<<"period">>, [Period]}) ->
  {period, parse_period(Period)}.

map_post_processing({<<"code">>, Code}) -> {code, Code};
map_post_processing({<<"libraries">>, Libraries}) ->
  {libraries, [map_library(Library) || Library <- Libraries]}.

map_library(Library) ->
  {
    proplists:get_value(<<"name">>, Library),
    strip_comments(proplists:get_value(<<"code">>, Library))
  }.

map_table_spec(TableSpec) ->
  [map_table_spec_property(TableSpecProperty) || TableSpecProperty <- TableSpec].

map_table_spec_property({<<"table">>, TableName}) -> {table, TableName};
map_table_spec_property({<<"user_rows">>, UserRows}) -> {user_rows, UserRows};
map_table_spec_property({<<"time_limit">>, TimeLimit}) -> {time_limit, TimeLimit};
map_table_spec_property({<<"where">>, Where}) -> {where, Where};
map_table_spec_property({<<"columns">>, Columns}) -> {columns, Columns}.

parse_period(null) -> undefined;
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

%% This function removes one-liner comments from lua code. We remove comments to
%% reduce the amount of data sent to each sandbox. Since we may have millions of
%% users, it's beneficial to strip out needless data.
%% The function only deals with one-liner comments (--). Block comments (--[[ ... --]])
%% are not stripped, because we'd need a full-blown parser for such cases.
%% Newlines are preserved, so reported errors will still point to proper line numbers.
strip_comments(LuaCode) ->
  re:replace(
        LuaCode,
        % Capture all characters (non-greedy) until the first occurence of -- not followed by a bracket
        % (if a bracket is behind, it's a possible block comment, so we'll capture this as well).
        "^(.*?)--(?![\\[\\]]).*$",
        % Replace the line with the captured group (all chars until the start of a non-block comment)
        "\\1",
        % Treat each line separately, replace all occurrences, return a binary
        [multiline, global, {return, binary}]
      ).


%% -------------------------------------------------------------------
%% Testing table insert conversion
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

strip_comments_test_() -> [
  ?_assertEqual(<<"foobar">>, strip_comments("foobar")),
  ?_assertEqual(<<"">>, strip_comments("--foobar")),
  ?_assertEqual(<<"foo">>, strip_comments("foo--bar")),
  ?_assertEqual(<<"foo">>, strip_comments("foo--bar --baz")),
  ?_assertEqual(<<"foobar">>, strip_comments("foobar--")),
  ?_assertEqual(<<"foo--[bar">>, strip_comments("foo--[bar")),
  ?_assertEqual(<<"foo">>, strip_comments("foo---[bar")),
  ?_assertEqual(<<"foo--]bar">>, strip_comments("foo--]bar")),
  ?_assertEqual(<<"foo">>, strip_comments("foo---]bar")),
  ?_assertEqual(<<"foo \nbar ">>, strip_comments("foo -- abc\nbar --def")),
  ?_assertEqual(<<"foo \nbar \n">>, strip_comments("foo -- abc\nbar --def\n")),
  ?_assertEqual(<<"foo \n\nbar \n">>, strip_comments("foo -- abc\n-- foobar\nbar --def\n")),
  ?_assertEqual(<<"foo\nbar\n">>, strip_comments("foo\nbar\n")),
  ?_assertEqual(<<"foo--[[\nbar\nbaz\n--]]">>, strip_comments("foo--[[\nbar\nbaz\n--]]"))
].

-endif.
