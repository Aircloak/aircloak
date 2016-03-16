%% @doc This module can be used to build a database query filter.
%%      See {@link build_filter/1} for detailed explanation.
-module(db_query_builder).

%% API
-export([
  build_filter/1
]).

-include("cloak.hrl").

-export_type([
  filter_spec/0
]).

-type comparison_operator() :: binary().
-type comparison() :: {comparison_operator(), column_name() | supported_sql_data()}.
-type filter_spec() ::
  [filter_spec()] |
  {column_name(), [filter_spec()]} |
  comparison().
-type query() :: {iodata(), [supported_sql_data()]}.

-record(parser_context, {
  param_index=1 :: pos_integer(), % used for generation of params
  field=undefined :: undefined | column_name(), % db field used in filters
  params=[] :: [supported_sql_data()] % values for database params (in reverse order)
}).

%% Internal
% Helper for matching a reference to a field  ($$field)
-define(FIELD_REF(Name), <<"$$", Name/binary>>).

% Helper macro for simpler definitions of binary operators in filters
-define(FILTER_BIN_OP(Operator, Value, Context),
      bin_op(
            Context#parser_context.field,
            Value,
            Context,
            fun(Field, PlaceHolder) -> [Field, Operator, PlaceHolder] end
          )
    ).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Builds the database filter query from the given input. The result consists of
%%      a query string (iolist), and a list of parameter values. This can be
%%      directly propagated to `sql_conn:extended_query' as part of any SQL query.
%%      References to fields and values are sanitized
%%
%%      Example:
%%      ```
%%        db_query_builder:build_filter(
%%              [
%%                {<<"$or">>, [
%%                  [{<<"$$x">>, [{<<"$gte">>, 10}, {<<"$lte">>, 20}]}],
%%                  [
%%                    {<<"$$x">>, [{<<"$gte">>, 100}, {<<"$lte">>, <<"$$y">>}]},
%%                    {<<"$$y">>, [{<<"$gte">>,1}, {<<"$lte">>, 12}]}
%%                  ]
%%                ]}
%%              ]
%%            )
%%        =>
%%          {
%%            "(((("x" >= $1 AND "x" <= $2)) OR (("x" >= $3 AND "x" <= "y") AND ("y" >= $4 AND "y" <= $5))))",
%%            [10, 20, 100, 1, 12]
%%          }
%%      '''
%%
%%      Note: the output of query string in result is prettified.
%%            In reality, a deep nested iolist is returned.
-spec build_filter(filter_spec()) -> query().
build_filter([]) ->
  {<<"TRUE">>, []};
build_filter(Filter) ->
  {FilterString, Context} = parse_filter(Filter, #parser_context{}),
  {FilterString, lists:reverse(Context#parser_context.params)}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% binary operators
parse_filter({<<"$eq">>, Value}, Context) -> ?FILTER_BIN_OP(<<"=">>, Value, Context);
parse_filter({<<"$neq">>, Value}, Context) -> ?FILTER_BIN_OP(<<"<>">>, Value, Context);
parse_filter({<<"$gt">>, Value}, Context) -> ?FILTER_BIN_OP(<<">">>, Value, Context);
parse_filter({<<"$gte">>, Value}, Context) -> ?FILTER_BIN_OP(<<">=">>, Value, Context);
parse_filter({<<"$lt">>, Value}, Context) -> ?FILTER_BIN_OP(<<"<">>, Value, Context);
parse_filter({<<"$lte">>, Value}, Context) -> ?FILTER_BIN_OP(<<"<=">>, Value, Context);
% field scope (e.g. $$field_name: {...})
parse_filter({?FIELD_REF(_) = Field, Term}, Context) ->
  {Res, Context1} = parse_filter(Term, Context#parser_context{field=Field}),
  {Res, Context1#parser_context{field=undefined}};
% logical operators
parse_filter({<<"$or">>, [_|_] = Terms}, Context) -> surround_join(Terms, <<" OR ">>, Context);
parse_filter({<<"$not">>, Term}, Context) ->
  {Res, Context1} = parse_filter(Term, Context),
  {[<<"NOT (">>, Res, <<")">>], Context1};
% a list always denotes and: [cond1, cond2] => cond1 and cond2
parse_filter(Terms0, Context) when is_list(Terms0) ->
  Terms = lists:filter(fun([]) -> false; (_) -> true end, Terms0),
  surround_join(Terms, <<" AND ">>, Context).

% Parses and joins the terms with joiner, then surrounds everything in parentheses.
surround_join(Terms, Joiner, Context) ->
  {JoinedTerms, Context1} = parse_join(Terms, Joiner, Context),
  FinalString = case Terms of
    [] -> [];
    [_] -> JoinedTerms;
    _ -> [<<"(">>, JoinedTerms, <<")">>]
  end,
  {FinalString, Context1}.

% Parses and joins terms with joiner.
parse_join(Terms, Joiner, Context) ->
  {ParsedTerms, Context1} = lists:foldl(
        fun(Term, {ParsedTermsAcc, ContextAcc}) ->
          {ParsedTerm, ContextAcc1} = parse_filter(Term, ContextAcc),
          {[ParsedTerm | ParsedTermsAcc], ContextAcc1}
        end,
        {[], Context},
        Terms
      ),
  {cloak_util:join(lists:reverse(ParsedTerms), Joiner), Context1}.

% Sanitizes operands, then calls the callback fun to generate the operation string
bin_op(Operand1, Operand2, Context, Fun) ->
  {ParsedOperand1, Context1} = sanitize(Operand1, Context),
  {ParsedOperand2, Context2} = sanitize(Operand2, Context1),
  {Fun(ParsedOperand1, ParsedOperand2), Context2}.

% field sanitization
sanitize(?FIELD_REF(Field), Context) -> {sql_util:sanitize_db_object(Field), Context};
% value sanitization
sanitize(Value, #parser_context{param_index=Index, params=Params} = Context) ->
  {
    sql_util:param_placeholder(Index),
    Context#parser_context{params=[Value | Params], param_index=Index+1}
  }.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(
  _filter_test(ExpectedQuery, ExpectedParams, QuerySpec),
  ?_test(begin
    {Query, Params} = db_query_builder:build_filter(QuerySpec),
    ?assertEqual(ExpectedQuery, iolist_to_binary(Query)),
    ?assertEqual(ExpectedParams, Params)
  end)
).

db_query_builder_test_() -> [
  ?_filter_test(<<"TRUE">>, [], []),
  ?_filter_test(
        <<"\"x\"=$1">>,
        [1],
        [{<<"$$x">>, [{<<"$eq">>, 1}]}]
      ),
  ?_filter_test(
        <<"\"x\"<>$1">>,
        [1],
        [{<<"$$x">>, [{<<"$neq">>, 1}]}]
      ),
  ?_filter_test(
        <<"\"x\"=\"y\"">>,
        [],
        [{<<"$$x">>, [{<<"$eq">>, <<"$$y">>}]}]
      ),
  ?_filter_test(
        <<"(\"x\"<$1 AND \"x\">$2)">>,
        [10, 0],
        [{<<"$$x">>, [{<<"$lt">>, 10}, {<<"$gt">>, 0}]}]
      ),
  ?_filter_test(
        <<"((\"x\"<$1 AND \"x\">$2) AND (\"y\"<=$3 AND \"y\">=$4))">>,
        [10, 0, 100, 90],
        [
          [{<<"$$x">>, [{<<"$lt">>, 10}, {<<"$gt">>, 0}]}],
          [{<<"$$y">>, [{<<"$lte">>, 100}, {<<"$gte">>, 90}]}]
        ]
      ),
  ?_filter_test(
        <<"((\"x\"<$1 AND \"x\">$2) OR (\"y\"<=$3 AND \"y\">=$4))">>,
        [10, 0, 100, 90],
        [
          {<<"$or">>, [
            [{<<"$$x">>, [{<<"$lt">>, 10}, {<<"$gt">>, 0}]}],
            [{<<"$$y">>, [{<<"$lte">>, 100}, {<<"$gte">>, 90}]}]
          ]}
        ]
      ),
  ?_filter_test(
        <<"NOT (((\"x\"<$1 AND \"x\">$2) AND (\"y\"<=$3 AND \"y\">=$4)))">>,
        [10, 0, 100, 90],
        [
          {<<"$not">>, [
            [{<<"$$x">>, [{<<"$lt">>, 10}, {<<"$gt">>, 0}]}],
            [{<<"$$y">>, [{<<"$lte">>, 100}, {<<"$gte">>, 90}]}]
          ]}
        ]
      )
].

-endif.
