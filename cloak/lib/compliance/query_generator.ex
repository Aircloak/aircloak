defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generates a random AST representing a query into the given tables."
  @spec generate_ast([Table.t()]) :: ast
  def generate_ast(tables) do
    {ast, _} = generate_ast_with_info(tables)
    ast
  end

  @doc "Generates the SQL query string from the given AST."
  @spec ast_to_sql(ast) :: iolist
  def ast_to_sql(ast), do: __MODULE__.Format.ast_to_sql(ast)

  # -------------------------------------------------------------------
  # Generators
  # -------------------------------------------------------------------

  defp generate_ast_with_info(tables) do
    {from_ast, tables} = generate_from(tables)
    {select_ast, info} = generate_select(tables)

    ast =
      {:query, nil,
       [
         select_ast,
         from_ast,
         optional(fn -> generate_where(tables) end),
         optional(fn -> generate_group_by(tables) end),
         optional(fn -> generate_having(tables) end),
         optional(fn -> generate_sample_users() end)
       ]}

    {ast, info}
  end

  defp generate_sample_users(), do: {:sample_users, :rand.uniform(100), []}

  defp generate_from(tables) do
    {from_ast, tables} = generate_from_expression(tables)
    {{:from, nil, [from_ast]}, tables}
  end

  defp generate_from_expression(tables),
    do:
      [
        fn -> generate_from_table(tables) end,
        fn -> generate_from_subquery(tables) end,
        fn -> generate_from_join(tables) end
      ]
      |> random_option()

  defp generate_from_subquery(tables) do
    name = random_name()
    {ast, info} = generate_ast_with_info(tables)

    {generate_as({:subquery, nil, [ast]}, name), [table_from_ast_info(name, info)]}
  end

  defp generate_from_join(tables) do
    {lhs, lhs_tables} = generate_join_element(tables)
    {rhs, rhs_tables} = generate_join_element(tables)
    tables = lhs_tables ++ rhs_tables
    {{:join, nil, [lhs, rhs, generate_on(tables)]}, tables}
  end

  defp generate_join_element(tables),
    do:
      [
        fn -> generate_aliased_table(tables) end,
        fn -> generate_from_subquery(tables) end
      ]
      |> random_option()

  defp generate_aliased_table(tables) do
    {table_ast, [table_info]} = generate_from_table(tables)
    alias = random_name()

    {generate_as(table_ast, alias), [%{table_info | name: alias}]}
  end

  defp generate_from_table(tables) do
    table = Enum.random(tables)
    {{:table, table.name, []}, [table]}
  end

  defp generate_as(object, name), do: {:as, name, [object]}

  defp generate_on(tables), do: {:on, nil, [generate_condition(tables)]}

  defp table_from_ast_info(name, ast_info),
    do: %{
      name: name,
      columns: Enum.map(ast_info, fn {type, name} -> %{name: name, type: type} end)
    }

  defp generate_where(tables), do: {:where, nil, [generate_condition(tables)]}

  defp generate_group_by(tables), do: {:group_by, nil, generate_group_list(tables)}

  defp generate_having(tables), do: {:having, nil, [generate_simple_condition(tables)]}

  defp generate_group_list(tables), do: many1(fn -> generate_column(tables) end)

  defp generate_simple_condition(tables),
    do:
      [
        fn -> generate_between(tables) end,
        fn -> generate_conjunction(fn -> generate_simple_condition(tables) end) end,
        fn -> generate_disjunction(fn -> generate_simple_condition(tables) end) end
        | Enum.map([:=, :<>, :<, :>], &generate_comparison(tables, &1))
      ]
      |> random_option()

  defp generate_condition(tables),
    do:
      [
        fn -> generate_between(tables) end,
        fn -> generate_conjunction(fn -> generate_condition(tables) end) end,
        fn -> generate_disjunction(fn -> generate_condition(tables) end) end,
        fn -> generate_like(tables) end,
        fn -> generate_in(tables) end
        | Enum.map([:=, :<>, :<, :>], &generate_comparison(tables, &1))
      ]
      |> random_option()

  defp generate_in(tables) do
    {column, table} = random_column(tables)
    type = Enum.random([:in, :not_in])
    {type, nil, [column_expression(column, table), generate_in_set(column.type)]}
  end

  defp generate_in_set(type), do: {:in_set, nil, many1(fn -> generate_value(type) end)}

  defp generate_like(tables) do
    type = Enum.random([:like, :ilike, :not_like, :not_ilike])
    {type, nil, [generate_column(tables), generate_value(:like_pattern)]}
  end

  defp generate_disjunction(generator), do: {:or, nil, [generator.(), generator.()]}

  defp generate_conjunction(generator), do: {:and, nil, [generator.(), generator.()]}

  defp generate_comparison(tables, type) do
    fn ->
      {column, table} = random_column(tables)
      value = generate_value(column.type)
      {type, nil, [column_expression(column, table), value]}
    end
  end

  defp generate_between(tables) do
    {column, table} = random_column(tables)

    {:between, nil, [column_expression(column, table), generate_value(column.type), generate_value(column.type)]}
  end

  defp generate_value(:any), do: [:boolean, :integer, :real, :text, :datetime] |> Enum.random() |> generate_value()

  defp generate_value(:boolean), do: {:boolean, [true, false] |> Enum.random(), []}
  defp generate_value(:integer), do: {:integer, :rand.uniform(1000), []}
  defp generate_value(:real), do: {:real, random_float(), []}
  defp generate_value(:text), do: {:text, random_text(), []}

  defp generate_value(:datetime),
    do:
      {:datetime,
       %NaiveDateTime{
         year: :rand.uniform(100) + 1950,
         month: :rand.uniform(12),
         day: :rand.uniform(28),
         hour: :rand.uniform(24) - 1,
         minute: :rand.uniform(60) - 1,
         second: :rand.uniform(60) - 1
       }, []}

  @like_characters [?% | Enum.to_list(?A..?z)]
  defp generate_value(:like_pattern), do: {:like_pattern, random_text(@like_characters), [optional(&like_escape/0)]}

  defp like_escape(), do: {:like_escape, [Enum.random(@like_characters)], []}

  defp generate_select(tables) do
    {select_list, info} = tables |> generate_select_list() |> Enum.unzip()
    {{:select, nil, select_list}, info}
  end

  defp generate_select_list(tables), do: many1(fn -> generate_expression_with_info(tables) end)

  defp generate_expression_with_info(tables),
    do:
      [
        fn -> generate_unaliased_expression_with_info(tables) end,
        fn -> generate_aliased_expression_with_info(tables) end
      ]
      |> random_option()

  defp generate_aliased_expression_with_info(tables) do
    {column, {table, _}} = generate_unaliased_expression_with_info(tables)
    alias = random_name()
    {generate_as(column, alias), {table, alias}}
  end

  defp generate_unaliased_expression(tables) do
    {expression, _info} = generate_unaliased_expression_with_info(tables)
    expression
  end

  defp generate_unaliased_expression_with_info(tables),
    do:
      [
        fn -> generate_aggregate_with_info(tables) end,
        fn -> generate_function_with_info(tables) end,
        fn -> generate_column_with_info(tables) end
      ]
      |> random_option()

  @functions ~w(
    abs btrim ceil concat date_trunc day extract_words floor hash hex hour left length lower ltrim minute month quarter
    right round rtrim second sqrt trunc upper weekday year
  )
  defp generate_function_with_info(tables) do
    function = Enum.random(@functions)

    arity =
      {:function, function, [], nil}
      |> Cloak.Sql.Function.argument_types()
      |> Enum.random()
      |> length()

    {{:function, function, Enum.map(1..arity, fn _ -> generate_unaliased_expression(tables) end)}, {:any, function}}
  end

  @aggregates ~w(count avg min max stddev count_noise avg_noise stddev_noise)
  defp generate_aggregate_with_info(tables) do
    {aggregated, {type, _}} = generate_unaliased_expression_with_info(tables)

    @aggregates
    |> Enum.map(fn aggregate ->
      {{:function, aggregate, [aggregated]}, {aggregate_type(aggregate, type), aggregate}}
    end)
    |> Enum.random()
  end

  defp aggregate_type(aggregate, type) when aggregate in ~w(min max), do: type
  defp aggregate_type(aggregate, _) when aggregate in ~w(count count_noise), do: :integer
  defp aggregate_type(_, _), do: :real

  defp generate_column(tables) do
    {column, _} = generate_column_with_info(tables)
    column
  end

  defp generate_column_with_info(tables) do
    {column, table} = random_column(tables)
    {column_expression(column, table), {column.type, column.name}}
  end

  defp column_expression(column, table), do: {:column, {column.name, table.name}, []}

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp many1(generator),
    do:
      [
        fn -> [generator.()] end,
        fn -> [generator.() | many1(generator)] end
      ]
      |> random_option()

  defp optional(generator),
    do:
      [
        fn -> {:empty, nil, []} end,
        generator
      ]
      |> random_option()

  defp random_option(options), do: Enum.random(options).()

  defp random_float(), do: (1 - 2 * :rand.uniform()) * :math.pow(10, :rand.uniform(3))

  @keywords ~w(in is as on or by from select)
  defp random_name() do
    name = random_text(?a..?z) |> to_string()

    if name in @keywords do
      random_name()
    else
      name
    end
  end

  defp random_text(allowed_chars \\ ?A..?z) do
    len = :rand.uniform(10)
    1..len |> Enum.map(fn _ -> Enum.random(allowed_chars) end)
  end

  defp random_column(tables) do
    table = Enum.random(tables)
    column = Enum.random(table.columns)
    {column, table}
  end
end
