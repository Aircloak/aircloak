defmodule Cloak.Aql.Parser do
  @moduledoc "Parser for SQL queries."
  use Combine
  import Cloak.Aql.Parsers
  alias Cloak.DataSource

  @type comparator ::
      :=
    | :<
    | :<=
    | :>=
    | :>

  @type qualified_identifier :: {:identifier, :unknown | String.t, String.t}

  @type column ::
      qualified_identifier
    | {:distinct, qualified_identifier}
    | {:function, String.t, [column]}
    | {:constant, Parsers.Token.t}

  @type like :: {:like | :ilike, String.t, String.t}
  @type is :: {:is, String.t, :null}

  @type where_clause ::
        {:comparison, String.t, comparator, any}
      | like
      | {:not, like}
      | {:not, {:comparison, String.t, :=, any}}
      | {:in, String.t, [any]}
      | is | {:not, is}

  @type from_clause ::
      String.t
    | {:join, :cross_join, from_clause, from_clause}
    | {
        :join, :full_outer_join | :left_outer_join | :right_outer_join, :inner_join,
        from_clause, from_clause, :on, [where_clause]
      }

  @type parsed_query :: %{
    command: :select | :show,
    columns: [column | {column, :as, String.t}] | :*,
    group_by: [String.t],
    from: from_clause | {:subquery, String.t},
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
          {:ok, map_from(statement, string)}
      end
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp map_from(%{from: {:subquery, from, from}} = statement, _statement_string) do
    %{statement | from: {:subquery, ""}}
  end
  defp map_from(%{from: {:subquery, from, to}} = statement, statement_string) do
    %{statement | from: {:subquery, String.slice(statement_string, from..(to - 1))}}
  end
  defp map_from(statement, _statement_string), do: statement

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
    |> map(&create_reportable_map/1)
  end

  defp create_reportable_map({[command], [statement_data]}) do
    statement_data
    |> Enum.reject(fn(value) -> value == nil end)
    |> Map.new
    |> Map.merge(%{command: command})
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
    either(
      keyword(:*),
      comma_delimited(select_column())
    ) |> map(&{:columns, &1})
    |> label("column definition")
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
    choice([
      parenthesised_expression(),
      function_expression(),
      extract_expression(),
      trim_expression(),
      substring_expression(),
      concat_expression(),
      qualified_identifier(),
      constant_column()
    ])
    |> label("column name or function")
  end

  defp parenthesised_expression() do
    pipe(
      [keyword(:"("), lazy(fn -> column() end), keyword(:")")],
      fn([:"(", result, :")"]) -> result end
    )
  end

  defp constant_column() do
    any_constant() |> map(&{:constant, &1})
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

  defp function_expression() do
    switch([
      {identifier() |> keyword(:"("), lazy(fn -> function_arguments() end) |> keyword(:")")},
      {:else, error_message(fail(""), "Expected an argument list")}
    ])
    |> map(fn
      {[function, :"("], [arguments, :")"]} -> {:function, String.downcase(function), arguments}
    end)
  end

  defp function_arguments() do
    choice([
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
        lazy(fn -> column() end),
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
         {:function, trim_function(trim_type), [column, {:constant, chars}]}
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
         {:function, "substring", [column, {:constant, from}]}
       [:substring, :"(", column, [:from, from], [:for, for_count], :")"] ->
         {:function, "substring", [column, {:constant, from}, {:constant, for_count}]}
       [:substring, :"(", column, nil, [:for, for_count], :")"] ->
         {:function, "substring_for", [column, {:constant, for_count}]}
       [:substring, :"(", column, nil, nil, :")"] ->
         {:function, "substring", [column]}
     end
   )
  end

  defp concat_expression() do
    infix_expression([keyword(:||)], either(qualified_identifier(), constant_column()))
  end

  defp infix_expression(operators, inner_expression) do
    either(
      pipe(
        [
          inner_expression,
          choice(operators),
          lazy(fn -> infix_expression(operators, inner_expression) end)
        ],
        fn[left, operator, right] -> {:function, to_string(operator), [left, right]} end
      ),
      inner_expression
    )
  end

  defp qualified_identifier() do
    map(
      either(
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
    switch([
      {keyword(:"(") |> map(fn(_) -> :subquery end), subquery(data_source)},
      {:else, table_selection()}
    ])
    |> map(fn
          {[:subquery], [[from, to]]} -> {:subquery, from, to}
          other -> other
        end)
  end

  defp table_selection() do
    map(
      comma_delimited(table_construct()),
      &turn_tables_into_join/1
    ) |> label("table name")
  end

  defp turn_tables_into_join([table]), do: handle_clause(table, nil)
  defp turn_tables_into_join([clause | rest]) do
    {:join, :cross_join, handle_clause(clause, nil), turn_tables_into_join(rest)}
  end

  defp handle_clause({:join_clause, table, sub_clause}, _acc) do
    handle_clause(sub_clause, table)
  end
  defp handle_clause({:join, :cross_join, table, sub_join}, acc) do
    acc = {:join, :cross_join, acc, table}
    handle_clause(sub_join, acc)
  end
  defp handle_clause({:join, join_type, table, :on, conditions, nil}, acc) do
    {:join, join_type, acc, table, :on, conditions}
  end
  defp handle_clause({:join, join_type, table, :on, conditions, sub_join}, acc) do
    acc = {:join, join_type, acc, table, :on, conditions}
    handle_clause(sub_join, acc)
  end
  defp handle_clause(nil, acc), do: acc
  defp handle_clause(table, _acc), do: table

  defp table_construct() do
    pipe([
        table_name(),
        option(join_appendix()),
      ],
      fn
        [table, nil] -> table
        [table, join] -> {:join_clause, table, join}
      end
    )
  end

  defp join_appendix() do
    pipe([
        option(join_type()),
        keyword(:join),
        table_name(),
        option(
          sequence([
            keyword(:on),
            where_expressions()
          ])
        ),
        option(lazy(fn -> join_appendix() end))
      ],
      fn
        ([:cross, :join, table, nil, next]) -> {:join, :cross_join, table, next}
        ([:cross, :join, _table, _on, _next]) -> {:join, :error, "`CROSS JOIN`s do not support `ON`-clauses."}
        ([_join, _join_type, _table, nil, _next]) ->
          {:join, :error, "Expected an `ON`-clause when JOINing tables."}
        ([join_type, :join, table, [:on, conditions], next]) ->
          {:join, expand_join_type(join_type), table, :on, conditions, next}
      end
    )
  end

  defp expand_join_type(nil), do: :inner_join
  defp expand_join_type(:inner), do: :inner_join
  defp expand_join_type(:outer), do: :full_outer_join
  defp expand_join_type(:full), do: :full_outer_join
  defp expand_join_type([:full, :outer]), do: :full_outer_join
  defp expand_join_type(:left), do: :left_outer_join
  defp expand_join_type([:left, :outer]), do: :left_outer_join
  defp expand_join_type(:right), do: :right_outer_join
  defp expand_join_type([:right, :outer]), do: :right_outer_join

  defp join_type() do
    choice([
      keyword(:cross),
      sequence([keyword(:full), keyword(:outer)]),
      keyword(:full),
      keyword(:outer),
      sequence([keyword(:left), keyword(:outer)]),
      keyword(:left),
      sequence([keyword(:right), keyword(:outer)]),
      keyword(:right),
      keyword(:inner),
    ])
  end

  defp table_name() do
    either(table_with_schema(), identifier())
    |> label("table name")
  end

  defp subquery(%{driver: Cloak.DataSource.DsProxy}) do
    unparsed_subquery()
  end
  defp subquery(_not_ds_proxy_data_source) do
    error_message(fail(""), "Subqueries are not supported for this data source")
  end

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
  end

  defp unparsed_subquery_token() do
    either(
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
          {[:where], [where_expressions]} -> {:where, where_expressions}
          other -> other
        end)
  end

  defp where_expressions() do
    and_delimited(where_expression())
  end

  defp where_expression() do
    switch([
      {qualified_identifier() |> option(keyword(:not)) |> choice([keyword(:like), keyword(:ilike)]), constant(:string)},
      {qualified_identifier() |> keyword(:in), in_values()},
      {qualified_identifier() |> keyword(:is) |> option(keyword(:not)), keyword(:null)},
      {qualified_identifier(), pair_both(comparator(), allowed_where_value())},
      {:else, error_message(fail(""), "Invalid where expression")}
    ])
    |> map(fn
          {[identifier, nil, :like], [string_constant]} -> {:like, identifier, string_constant}
          {[identifier, :not, :like], [string_constant]} -> {:not, {:like, identifier, string_constant}}
          {[identifier, nil, :ilike], [string_constant]} -> {:ilike, identifier, string_constant}
          {[identifier, :not, :ilike], [string_constant]} -> {:not, {:ilike, identifier, string_constant}}
          {[identifier, :in], [in_values]} -> {:in, identifier, in_values}
          {[identifier, :is, nil], [:null]} -> {:is, identifier, :null}
          {[identifier, :is, :not], [:null]} -> {:not, {:is, identifier, :null}}
          {[identifier], [{:<>, value}]} -> {:not, {:comparison, identifier, :=, value}}
          {[identifier], [{comparator, value}]} -> {:comparison, identifier, comparator, value}
        end)
  end

  defp allowed_where_value() do
    choice([qualified_identifier(), any_constant()])
    |> label("comparison value")
  end

  defp in_values() do
    pipe(
      [keyword(:"("), comma_delimited(any_constant()), keyword(:")")],
      fn([_, values, _]) -> values end
    )
  end

  defp any_constant() do
    constant_of([:string, :integer, :float, :boolean])
    |> label("comparison value")
  end

  defp constant_of(expected_types) do
    choice(Enum.map(expected_types, &constant/1))
    |> label(expected_types |> Enum.map(&"#{&1} constant") |> Enum.join(" or "))
  end

  defp constant(expected_type) do
    token(:constant)
    |> satisfy(fn(token) -> token.value.type == expected_type end)
    |> label("#{expected_type} constant")
  end

  defp optional_order_by() do
    switch([
      {keyword(:order), keyword(:by) |> comma_delimited(order_by_field())},
      {:else, noop()}
    ])
    |> map(fn({[:order], [:by, fields]}) -> {:order_by, fields} end)
  end

  defp optional_group_by() do
    switch([
      {keyword(:group), keyword(:by) |> comma_delimited(column())},
      {:else, noop()}
    ])
    |> map(fn {_, [:by, columns]} -> {:group_by, columns} end)
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

  defp comma_delimited(previous \\ noop(), term_parser) do
    previous
    |> sep_by1(term_parser, keyword(:","))
  end

  defp and_delimited(term_parser) do
    sep_by1(term_parser, keyword(:and))
  end

  defp end_of_input(parser) do
    parser
    |> error_message(token(:eof), "Expected end of input.")
    |> ignore()
  end

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
