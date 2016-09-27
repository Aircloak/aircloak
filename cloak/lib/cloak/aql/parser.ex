defmodule Cloak.Aql.Parser do
  @moduledoc "Parser for SQL queries."
  import Combine.Parsers.Base
  import Cloak.Aql.Parsers
  alias Cloak.DataSource

  @type comparator ::
      :=
    | :<
    | :<=
    | :>=
    | :>

  @type qualified_identifier :: {:identifier, :unknown | String.t, String.t}

  @type data_type :: DataSource.data_type | :interval

  @type column ::
      qualified_identifier
    | {:distinct, qualified_identifier}
    | {:function, String.t, [column]}
    | {:constant, data_type, any}

  @type negatable_condition ::
      {:comparison, String.t, :=, any}
    | {:like | :ilike, String.t, String.t}
    | {:is, String.t, :null}
    | {:in, String.t, [any]}

  @type where_clause ::
      negatable_condition | {:not, negatable_condition}
    | {:comparison, String.t, comparator, any}

  @type from_clause :: table | parsed_subquery | unparsed_subquery | join

  @type table :: String.t

  @type join ::
    {:join, %{
      type: :cross_join | :inner_join | :full_outer_join | :left_outer_join | :right_outer_join,
      lhs: from_clause,
      rhs: from_clause,
      conditions: [where_clause]
    }}

  @type parsed_subquery :: {:subquery, %{type: :parsed, ast: parsed_query, alias: String.t}}
  @type unparsed_subquery :: {:subquery, %{type: :unparsed, unparsed_string: String.t}}

  @type parsed_query :: %{
    command: :select | :show,
    columns: [column | {column, :as, String.t}] | :*,
    group_by: [String.t],
    from: from_clause,
    where: [where_clause],
    order_by: [{String.t, :asc | :desc}],
    show: :tables | :columns
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses a SQL query in text form. Raises on error."
  @spec parse!(DataSource.t, String.t) :: parsed_query
  def parse!(data_source, string) do
    {:ok, query} = parse(data_source, string)
    query
  end

  @doc "Parses a SQL query in text form."
  @spec parse(DataSource.t, String.t) :: {:ok, parsed_query} | {:error, any}
  def parse(data_source, string) do
    with {:ok, tokens} <- Cloak.Aql.Lexer.tokenize(string) do
      case parse_tokens(tokens, parser(data_source)) do
        {:error, _} = error -> error
        [statement] ->
          {:ok, map_unparsed_subquery(statement, string)}
      end
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp map_unparsed_subquery(%{from: {:subquery, {:unparsed, from, from}}} = statement, _statement_string) do
    %{statement | from: {:subquery, %{type: :unparsed, unparsed_string: ""}}}
  end
  defp map_unparsed_subquery(%{from: {:subquery, {:unparsed, from, to}}} = statement, statement_string) do
    %{statement | from: {
      :subquery,
      %{type: :unparsed, unparsed_string: String.slice(statement_string, from..(to - 1))}
    }}
  end
  defp map_unparsed_subquery(statement, _statement_string), do: statement

  defp parser(data_source) do
    statement(data_source)
    |> statement_termination()
    |> end_of_input()
  end

  defp statement(data_source) do
    switch([
      {keyword(:select), select_statement(data_source)},
      {keyword(:show), show_statement()},
      {:else, error_message(fail(""), "Expected `select or show`")}
    ])
    |> map(fn({[command], [statement_data]}) -> statement_map(command, statement_data) end)
  end

  defp statement_map(command, statement_data) do
    statement_data
    |> Enum.reject(fn(value) -> value == nil end)
    |> Enum.into(%{})
    |> Map.put(:command, command)
  end

  defp statement_termination(parser) do
    parser
    |> skip(keyword(:";"))
  end

  defp show_statement() do
    switch([
      {keyword(:tables), noop()},
      {keyword(:columns), pair_both(keyword(:from), table_name())},
      {:else, error_message(fail(""), "Expected `tables or columns`")}
    ])
    |> map(fn({[show], data}) -> [{:show, show} | data] end)
  end

  defp select_statement(data_source) do
    sequence([
      select_columns(),
      from(data_source),
      optional_where(),
      optional_group_by(),
      optional_order_by()
    ])
  end

  defp select_columns() do
    either_deepest_error(
      keyword(:*) |> label("column definition"),
      comma_delimited(select_column())
    ) |> map(&{:columns, &1})
  end

  defp column() do
    additive_expression()
  end

  defp additive_expression() do
    infix_expression([keyword(:+), keyword(:-)], multiplicative_expression())
  end

  defp multiplicative_expression() do
    infix_expression([keyword(:*), keyword(:/), keyword(:%)], exponentiation_expression())
  end

  def exponentiation_expression() do
    infix_expression([keyword(:^)], simple_expression())
  end

  defp simple_expression() do
    choice_deepest_error([
      parenthesised_expression(),
      cast_expression(),
      function_expression(),
      extract_expression(),
      trim_expression(),
      substring_expression(),
      concat_expression(),
      qualified_identifier(),
      constant_column() |> label("column definition")
    ])
  end

  defp parenthesised_expression() do
    pipe(
      [keyword(:"("), lazy(fn -> column() end), keyword(:")")],
      fn([:"(", result, :")"]) -> result end
    )
  end

  defp constant_column() do
    either_deepest_error(interval(), any_constant())
  end

  defp select_column() do
    pipe(
      [
        column(),
        option(keyword(:as) |> identifier())
      ],
      fn
        ([column, nil]) -> column
        ([column, :as, name]) -> {column, :as, name}
      end
    )
  end

  defp cast_expression() do
    pipe(
      [
        keyword(:cast),
        keyword(:"("),
        lazy(fn -> column() end),
        either(keyword(:","), keyword(:as)),
        data_type(),
        keyword(:")"),
      ],
      fn [:cast, :"(", expr, _, type, :")"] -> {:function, {:cast, type}, [expr]} end
    )
  end

  @data_types ~w(integer real text boolean timestamp date time)
  defp data_type() do
    non_keyword_type =
      identifier()
      |> satisfy(&Enum.member?(@data_types, &1))
      |> map(&String.to_atom/1)

    either(non_keyword_type, keyword(:interval))
    |> label("type name")
  end

  defp function_expression() do
    switch([
      {function_name() |> keyword(:"("), lazy(fn -> function_arguments() end) |> keyword(:")")},
      {:else, error_message(fail(""), "Expected an argument list")}
    ])
    |> map(fn
      {[function, :"("], [arguments, :")"]} -> {:function, String.downcase(function), arguments}
    end)
  end

  defp function_name() do
    choice([
      identifier(),
      keyword(:left),
      keyword(:right),
    ])
    |> map(&to_string/1)
  end

  defp function_arguments() do
    choice_deepest_error([
      comma_delimited(column()),
      distinct_identifier(),
      keyword(:*)
    ])
    |> map(fn
      [_|_] = arguments -> arguments
      single_argument -> [single_argument]
    end)
  end

  defp extract_expression() do
    pipe(
      [
        keyword(:extract),
        keyword(:"("),
        identifier(),
        keyword(:from),
        lazy(fn -> map(column(), &[&1]) end),
        keyword(:")"),
      ],
      fn([:extract, :"(", part, :from, column, :")"]) -> {:function, String.downcase(part), column} end
    )
  end

  defp trim_expression() do
    pipe(
      [
        keyword(:trim),
        keyword(:"("),
        choice([keyword(:both), keyword(:leading), keyword(:trailing)]),
        option(constant(:string)),
        keyword(:from),
        lazy(fn -> column() end),
        keyword(:")"),
     ],
     fn
       [:trim, :"(", trim_type, nil, :from, column, :")"] ->
         {:function, trim_function(trim_type), [column]}
       [:trim, :"(", trim_type, chars, :from, column, :")"] ->
         {:function, trim_function(trim_type), [column, chars]}
     end
   )
  end

  defp trim_function(:both), do: "btrim"
  defp trim_function(:leading), do: "ltrim"
  defp trim_function(:trailing), do: "rtrim"

  defp substring_expression() do
    pipe(
      [
        keyword(:substring),
        keyword(:"("),
        lazy(fn -> column() end),
        option(sequence([keyword(:from), constant(:integer)])),
        option(sequence([keyword(:for), constant(:integer)])),
        keyword(:")"),
     ],
     fn
       [:substring, :"(", column, [:from, from], nil, :")"] ->
         {:function, "substring", [column, from]}
       [:substring, :"(", column, [:from, from], [:for, for_count], :")"] ->
         {:function, "substring", [column, from, for_count]}
       [:substring, :"(", column, nil, [:for, for_count], :")"] ->
         {:function, "substring_for", [column, for_count]}
       [:substring, :"(", column, nil, nil, :")"] ->
         {:function, "substring", [column]}
     end
   )
  end

  defp concat_expression() do
    infix_expression([keyword(:||)], either_deepest_error(qualified_identifier(), constant_column()))
  end

  defp infix_expression(operators, inner_expression) do
    pipe(
      [
        inner_expression,
        many(sequence([choice(operators), inner_expression])),
      ],
      fn[first, rest] -> Enum.reduce(rest, first,
        fn([operator, right], left) -> {:function, to_string(operator), [left, right]} end)
      end
    )
  end

  defp qualified_identifier() do
    map(
      either_deepest_error(
        sequence(
          [
            identifier(),
            keyword(:"."),
            identifier()
          ]
        ),
        identifier()
      ),
      fn
        ([table, :".", column]) -> {:identifier, table, column}
        (column) -> {:identifier, :unknown, column}
      end
    )
  end

  defp distinct_identifier() do
    pair_both(keyword(:distinct), qualified_identifier())
  end

  defp from(data_source) do
    pair_both(
      keyword(:from),
      from_expression(data_source)
    )
  end

  defp from_expression(data_source) do
    map(
      comma_delimited(join_expression(data_source) |> map(&join_ast/1)),
      &cross_joins/1
    )
  end

  defp join_ast(join_clauses),
    do: pair_joins(flatten_join_clauses(join_clauses))

  defp flatten_join_clauses([table]), do: [table]
  defp flatten_join_clauses([table, {join_type, next_join}]) do
    [table, join_type] ++ flatten_join_clauses(next_join)
  end

  defp pair_joins([table_or_join]), do: table_or_join
  defp pair_joins([table_or_join, join_type, table | rest]) do
    pair_joins([to_join(join_type, table_or_join, table) | rest])
  end

  defp to_join({join_type, :on, conditions}, left_expr, right_expr),
    do: {:join, %{type: join_type, lhs: left_expr, rhs: right_expr, conditions: conditions}}
  defp to_join(:cross_join, left_expr, right_expr),
    do: to_join({:cross_join, :on, []}, left_expr, right_expr)

  defp cross_joins([table]), do: table
  defp cross_joins([clause | rest]), do: to_join(:cross_join, clause, cross_joins(rest))

  defp join_expression(data_source) do
    lazy(fn ->
      sequence([
        table_or_subquery(data_source),
        next_join(data_source)
      ])
    end)
  end

  defp join_expression_with_on_clause(data_source) do
    lazy(fn ->
      sequence([
        table_or_subquery(data_source),
        keyword(:on),
        where_expressions(),
        next_join(data_source)
      ])
    end)
  end

  defp next_join(data_source) do
    switch([
      {cross_join(),join_expression(data_source)},
      {either_deepest_error(inner_join(), outer_join()), join_expression_with_on_clause(data_source)},
      {:else, noop()}
    ])
    |> map(&join_clause/1)
  end

  defp join_clause({[join_type], [[rhs, :on, comparison | next_join]]}),
    do: {{join_type, :on, comparison}, [rhs | next_join]}
  defp join_clause({[join_type], [rhs]}),
    do: {join_type, rhs}

  defp cross_join(),
    do: replace(pair_both(keyword(:cross), keyword(:join)), :cross_join)

  defp inner_join(),
    do: replace(pair_both(option(keyword(:inner)), keyword(:join)), :inner_join)

  defp outer_join(),
    do: choice([outer_join(:left), outer_join(:right), outer_join(:full)])

  defp outer_join(type) do
    sequence([keyword(type), option(keyword(:outer)), keyword(:join)])
    |> replace(:"#{type}_outer_join")
  end

  defp table_or_subquery(data_source) do
    switch([
      {keyword(:"(") |> map(fn(_) -> :subquery end), subquery(data_source)},
      {:else, table_name()}
    ])
    |> map(fn
          {[:subquery], [subquery_data]} -> {:subquery, subquery_data}
          other -> other
        end)
  end

  defp table_name() do
    either_deepest_error(
      table_with_schema(),
      identifier() |> label("table name")
    )
  end

  defp parsed_subquery(data_source) do
    sequence([
      ignore(keyword(:select)),
      lazy(fn -> select_statement(data_source) end),
      ignore(keyword(:")")),
      option(keyword(:as)),
      label(identifier(), "subquery alias")
    ])
    |> map(
          fn([select_statement, _as_keyword, alias]) ->
            %{type: :parsed, ast: statement_map(:select, select_statement), alias: alias}
          end
        )
  end

  defp subquery(%{driver: Cloak.DataSource.DsProxy}), do: unparsed_subquery()
  defp subquery(other_data_source), do: parsed_subquery(other_data_source)

  defp unparsed_subquery() do
    sequence([
      token_offset(),
      ignore(many1(unparsed_subquery_token()) |> label("subquery expression")),
      token_offset(),
      ignore(keyword(:")")),
      ignore(
        sequence([option(keyword(:as)), identifier()])
        |> label("subquery alias")
      )
    ])
    |> map(fn([from, to]) -> {:unparsed, from, to} end)
  end

  defp unparsed_subquery_token() do
    either_deepest_error(
      sequence([keyword(:"("), lazy(fn -> many(unparsed_subquery_token()) end), keyword(:")")]),
      any_token() |> satisfy(&(&1.category != :")" && &1.category != :eof))
    )
  end

  defp table_with_schema() do
    pipe(
      [identifier(), keyword(:.), identifier()],
      &Enum.join/1
    )
  end

  defp optional_where() do
    switch([
      {keyword(:where), where_expressions()},
      {:else, noop()}
    ])
    |> map(fn
          {[:where], [where_expressions]} -> {:where, List.flatten(where_expressions)}
          other -> other
        end)
  end

  defp where_expressions() do
    and_delimited(where_expression())
  end

  defp where_expression() do
    switch([
      {qualified_identifier() |> option(keyword(:not)) |> choice([keyword(:like), keyword(:ilike)]), constant(:string)},
      {qualified_identifier() |> option(keyword(:not)) |> keyword(:in), in_values()},
      {qualified_identifier() |> keyword(:is) |> option(keyword(:not)), keyword(:null)},
      {qualified_identifier() |> keyword(:between), allowed_where_range()},
      {qualified_identifier(), pair_both(comparator(), allowed_where_value())},
      {:else, error_message(fail(""), "Invalid where expression")}
    ])
    |> map(fn
          {[identifier, nil, :like], [string_constant]} -> {:like, identifier, string_constant}
          {[identifier, :not, :like], [string_constant]} -> {:not, {:like, identifier, string_constant}}
          {[identifier, nil, :ilike], [string_constant]} -> {:ilike, identifier, string_constant}
          {[identifier, :not, :ilike], [string_constant]} -> {:not, {:ilike, identifier, string_constant}}
          {[identifier, nil, :in], [in_values]} -> {:in, identifier, in_values}
          {[identifier, :not, :in], [in_values]} -> {:not, {:in, identifier, in_values}}
          {[identifier, :is, nil], [:null]} -> {:is, identifier, :null}
          {[identifier, :is, :not], [:null]} -> {:not, {:is, identifier, :null}}
          {[identifier], [{:<>, value}]} -> {:not, {:comparison, identifier, :=, value}}
          {[identifier], [{comparator, value}]} -> {:comparison, identifier, comparator, value}
          {[identifier, :between], [{min, max}]} ->
            [{:comparison, identifier, :>=, min}, {:comparison, identifier, :<=, max}]
        end)
  end

  defp allowed_where_value() do
    either(qualified_identifier(), any_constant())
    |> label("comparison value")
  end

  defp allowed_where_range() do
    pipe(
      [allowed_where_value(), keyword(:and), allowed_where_value()],
      fn([min, :and, max]) -> {min, max} end
    )
  end

  defp in_values() do
    pipe(
      [keyword(:"("), comma_delimited(any_constant()), keyword(:")")],
      fn([_, values, _]) -> values end
    )
  end

  defp interval() do
    pipe(
      [
        keyword(:interval),
        constant_of([:string]),
      ],
      fn([:interval, {:constant, :string, value}]) -> Timex.Duration.parse(value) end
    )
    |> satisfy(&match?({:ok, _}, &1))
    |> map(fn({:ok, result}) -> {:constant, :interval, result} end)
  end

  defp any_constant() do
    constant_of([:string, :integer, :float, :boolean])
  end

  defp constant_of(expected_types) do
    choice(Enum.map(expected_types, &constant/1))
    |> label(expected_types |> Enum.map(&"#{&1} constant") |> Enum.join(" or "))
  end

  defp constant(expected_type) do
    token(:constant)
    |> satisfy(fn(token) -> token.value.type == expected_type end)
    |> map(&{:constant, &1.value.type, &1.value.value})
    |> label("#{expected_type} constant")
  end

  defp optional_order_by() do
    switch([
      {keyword(:order), pair_both(keyword(:by), comma_delimited(order_by_field()))},
      {:else, noop()}
    ])
    |> map(fn({[:order], [{:by, fields}]}) -> {:order_by, fields} end)
  end

  defp optional_group_by() do
    switch([
      {keyword(:group), pair_both(keyword(:by), comma_delimited(column()))},
      {:else, noop()}
    ])
    |> map(fn {_, [{:by, columns}]} -> {:group_by, columns} end)
  end

  defp order_by_field() do
    pair_both(
      column(),
      order_by_direction()
    )
  end

  defp order_by_direction() do
    option(
      either(
        keyword(:asc),
        keyword(:desc)
      )
    )
  end

  defp identifier(parser \\ noop()) do
    parser
    |> token(:identifier)
    |> map(&(&1.value))
    |> label("identifier")
  end

  defp comparator(parser \\ noop()) do
    parser
    |> keyword_of([:=, :<, :<=, :>=, :>, :<>])
    |> label("comparator")
  end

  defp keyword_of(parser, types) do
    parser
    |> choice(Enum.map(types, &keyword(&1)))
    |> label(types |> Enum.join(" or "))
  end

  defp keyword(parser \\ noop(), type) do
    parser
    |> token(type)
    |> map(&(&1.category))
    |> label(to_string(type))
  end

  defp comma_delimited(term_parser) do
    sep_by1_failing(term_parser, keyword(:","))
  end

  defp and_delimited(term_parser) do
    sep_by1_failing(term_parser, keyword(:and))
  end

  defp end_of_input(parser) do
    parser
    |> error_message(token(:eof), "Expected end of input.")
    |> ignore()
  end

  defp replace(parser, value), do: map(parser, fn(_) -> value end)

  # -------------------------------------------------------------------
  # Work around invalid combine spec (see below)
  # -------------------------------------------------------------------

  # Temporary hack, since per spec Combine.parse accepts only string, which
  # leads to many dialyzer errors. A couple of functions are copy-pasted here
  # from combine. Once our changes are merged upstream, we should replace this
  # with a regular combine

  defp parse_tokens([first_token | _] = tokens, parser) do
    alias Combine.ParserState

    case parser.(%ParserState{input: tokens, line: first_token.line, column: first_token.column}) do
      %ParserState{status: :ok, results: res} ->
        res |> Enum.reverse |> Enum.filter_map(&ignore_filter/1, &filter_ignores/1)
      %ParserState{error: res} ->
        {:error, res}
      x ->
        {:error, {:fatal, x}}
    end
  end

  defp ignore_filter(:__ignore), do: false
  defp ignore_filter(_), do: true

  defp filter_ignores(element) when is_list(element) do
    Enum.filter_map(element, &ignore_filter/1, &filter_ignores/1)
  end
  defp filter_ignores(element), do: element
end
