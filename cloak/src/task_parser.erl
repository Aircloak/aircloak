%% @doc Decodes the map that describes the task and returns the corresponding task specification.
%%
%%      Example of a map describing the task:
%%        ```
%%          {
%%            "task_id": "1234",
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
    prefetch = parse_prefetch(maps:get(<<"prefetch">>, Task)),
    code = strip_comments(maps:get(<<"code">>, Task)),
    libraries = [parse_library(Library) || Library <- maps:get(<<"libraries">>, Task, [])],
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

parse_prefetch(Prefetch) ->
  [parse_table_spec(maps:to_list(TableSpec)) || TableSpec <- Prefetch].

parse_table_spec(TableSpec) ->
  [{erlang:binary_to_existing_atom(Name, utf8), Value} || {Name, Value} <- TableSpec].

parse_return_url(undefined) -> air_socket;
parse_return_url(Url) -> {url, Url}.

parse_report_interval(undefined) -> undefined;
parse_report_interval(ReportInterval) -> ReportInterval * 1000.

parse_library(Library) ->
  {maps:get(<<"name">>, Library), strip_comments(maps:get(<<"code">>, Library))}.

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
