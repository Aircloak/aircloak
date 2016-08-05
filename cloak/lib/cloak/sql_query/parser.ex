defmodule Cloak.SqlQuery.Parser do
  @moduledoc "Parser for SQL queries."
  use Combine
  import Cloak.SqlQuery.Parsers

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

  @type parsed_query :: %{
    command: :select | :show,
    columns: [column | {column, :as, String.t}] | :*,
    group_by: [String.t],
    from: String.t | {:subquery, String.t},
    where: [where_clause],
    order_by: [{String.t, :asc | :desc}],
    show: :tables | :columns
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses a SQL query in text form. Raises on error."
  @spec parse!(String.t) :: parsed_query
  def parse!(string) do
    {:ok, query} = parse(string)
    query
  end

  @doc "Parses a SQL query in text form."
  @spec parse(String.t) :: {:ok, parsed_query} | {:error, any}
  def parse(string) do
    with {:ok, tokens} <- Cloak.SqlQuery.Lexer.tokenize(string) do
      case parse_tokens(tokens, parser()) do
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

  defp parser do
    statement()
    |> statement_termination()
    |> end_of_input()
  end

  defp statement() do
    switch([
      {keyword(:select), select_statement()},
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
      {keyword(:columns), from()},
      {:else, error_message(fail(""), "Expected `tables or columns`")}
    ])
    |> map(fn({[show], data}) -> [{:show, show} | data] end)
  end

  defp select_statement() do
    sequence([
      select_columns(),
      from(),
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
    choice([function_expression(), extract_expression(), qualified_identifier(), constant_column()])
    |> label("column name or function")
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

  defp from() do
    pair_both(
      keyword(:from),
      from_expression()
    )
  end

  defp from_expression() do
    switch([
      {keyword(:"(") |> map(fn(_) -> :subquery end), subquery()},
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
        ([nil, :join, table, [:on, conditions], next]) -> {:join, :inner_join, table, :on, conditions, next}
        ([:inner, :join, table, [:on, conditions], next]) -> {:join, :inner_join, table, :on, conditions, next}
        ([:outer, :join, table, [:on, conditions], next]) -> {:join, :full_outer_join, table, :on, conditions, next}
        ([:full, :join, table, [:on, conditions], next]) -> {:join, :full_outer_join, table, :on, conditions, next}
        ([[:full, :outer], :join, table, [:on, conditions], next]) ->
          {:join, :full_outer_join, table, :on, conditions, next}
        ([:left, :join, table, [:on, conditions], next]) -> {:join, :left_outer_join, table, :on, conditions, next}
        ([[:left, :outer], :join, table, [:on, conditions], next]) ->
          {:join, :left_outer_join, table, :on, conditions, next}
        ([:right, :join, table, [:on, conditions], next]) -> {:join, :right_outer_join, table, :on, conditions, next}
        ([[:right, :outer], :join, table, [:on, conditions], next]) ->
          {:join, :right_outer_join, table, :on, conditions, next}
      end
    )
  end

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
  end

  defp subquery() do
    sequence([
      token_offset(),
      ignore(many1(subquery_token()) |> label("subquery expression")),
      token_offset(),
      ignore(keyword(:")")),
      ignore(
        sequence([option(keyword(:as)), identifier()])
        |> label("subquery alias")
      )
    ])
  end

  defp subquery_token() do
    either(
      sequence([keyword(:"("), lazy(fn -> many(subquery_token()) end), keyword(:")")]),
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
