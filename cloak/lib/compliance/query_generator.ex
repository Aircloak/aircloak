defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generates a random AST representing a query into the given tables."
  @spec generate_ast([Table.t]) :: ast
  def generate_ast(tables) do
    {ast, _} = generate_ast_with_info(tables)
    ast
  end

  @doc "Generates the SQL query string fro the given AST."
  @spec ast_to_sql(ast) :: iolist
  def ast_to_sql({:query, _, items}), do: Enum.map(items, &ast_to_sql/1)
  def ast_to_sql({:select, nil, select_list}), do:
    [" SELECT ", Enum.map(select_list, &ast_to_sql/1) |> Enum.intersperse(", ")]
  def ast_to_sql({:from, nil, [from_expression]}), do: [" FROM ", ast_to_sql(from_expression)]
  def ast_to_sql({:table, name, []}), do: name
  def ast_to_sql({:subquery, nil, [definition]}), do: ["( ", ast_to_sql(definition), " )"]
  def ast_to_sql({:join, nil, [lhs, rhs, on]}), do: [ast_to_sql(lhs), " JOIN ", ast_to_sql(rhs), ast_to_sql(on)]
  def ast_to_sql({:on, nil, [condition]}), do: [" ON ", ast_to_sql(condition)]
  def ast_to_sql({:as, name, [object]}), do: [ast_to_sql(object), " AS ", name]
  def ast_to_sql({:where, nil, [condition]}), do: [" WHERE ", ast_to_sql(condition)]
  def ast_to_sql({:group_by, nil, group_list}), do:
    [" GROUP BY ", Enum.map(group_list, &ast_to_sql/1) |> Enum.intersperse(", ")]
  def ast_to_sql({:having, nil, [condition]}), do: [" HAVING ", ast_to_sql(condition)]
  def ast_to_sql({:=, nil, [lhs, rhs]}), do: [ast_to_sql(lhs), " = ", ast_to_sql(rhs)]
  def ast_to_sql({:<>, nil, [lhs, rhs]}), do: [ast_to_sql(lhs), " <> ", ast_to_sql(rhs)]
  def ast_to_sql({:<, nil, [lhs, rhs]}), do: [ast_to_sql(lhs), " < ", ast_to_sql(rhs)]
  def ast_to_sql({:>, nil, [lhs, rhs]}), do: [ast_to_sql(lhs), " > ", ast_to_sql(rhs)]
  def ast_to_sql({:between, nil, [lhs, low, high]}), do:
    [ast_to_sql(lhs), " BETWEEN ", ast_to_sql(low), " AND ", ast_to_sql(high)]
  def ast_to_sql({:and, nil, [lhs, rhs]}), do: ["(", ast_to_sql(lhs), " AND ", ast_to_sql(rhs), ")"]
  def ast_to_sql({:or, nil, [lhs, rhs]}), do: ["(", ast_to_sql(lhs), " OR ", ast_to_sql(rhs), ")"]
  def ast_to_sql({:function, name, args}), do: [name, "(", Enum.map(args, &ast_to_sql/1), ")"]
  def ast_to_sql({:column, {column, table}, []}), do: [?", table, ?", ?., ?", column, ?"]
  def ast_to_sql({:integer, value, []}), do: to_string(value)
  def ast_to_sql({:text, value, []}), do: [?', value, ?']
  def ast_to_sql({:boolean, value, []}), do: to_string(value)
  def ast_to_sql({:datetime, value, []}), do: [?', value, ?']
  def ast_to_sql({:real, value, []}), do: to_string(value)
  def ast_to_sql({:star, _, _}), do: "*"
  def ast_to_sql({:empty, _, _}), do: ""
  def ast_to_sql({:sample_users, size, []}), do: [" SAMPLE_USERS ", to_string(size), "%"]


  # -------------------------------------------------------------------
  # Generators
  # -------------------------------------------------------------------

  defp generate_ast_with_info(tables) do
    {from_ast, tables} = generate_from(tables)
    {select_ast, info} = generate_select(tables)
    ast = {:query, nil, [
      select_ast,
      from_ast,
      optional(fn -> generate_where(tables) end),
      optional(fn -> generate_group_by(tables) end),
      optional(fn -> generate_having(tables) end),
      optional(fn -> generate_sample_users() end),
    ]}

    {ast, info}
  end

  defp generate_sample_users(), do:
    {:sample_users, :rand.uniform(100), []}

  defp generate_from(tables) do
    {from_ast, tables} = generate_from_expression(tables)
    {{:from, nil, [from_ast]}, tables}
  end

  defp generate_from_expression(tables), do:
    [
      fn -> generate_from_table(tables) end,
      fn -> generate_from_subquery(tables) end,
      fn -> generate_from_join(tables) end,
    ] |> random_option()

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

  defp generate_join_element(tables), do:
    [
      fn -> generate_aliased_table(tables) end,
      fn -> generate_from_subquery(tables) end,
    ] |> random_option()

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

  defp generate_on(tables), do:
    {:on, nil, [generate_condition(tables)]}

  defp table_from_ast_info(name, ast_info), do:
    %{name: name, columns: Enum.map(ast_info, fn({type, name}) -> %{name: name, type: type} end)}

  defp generate_where(tables), do:
    {:where, nil, [generate_condition(tables)]}

  defp generate_group_by(tables), do:
    {:group_by, nil, generate_group_list(tables)}

  defp generate_having(tables), do:
    {:having, nil, [generate_condition(tables)]}

  defp generate_group_list(tables), do:
    many1(fn -> generate_column(tables) end)

  defp generate_condition(tables), do:
    [
      fn -> generate_between(tables) end,
      fn -> generate_conjunction(tables) end,
      fn -> generate_disjunction(tables) end
      | Enum.map([:=, :<>, :<, :>], &generate_comparison(tables, &1))
    ] |> random_option()

  defp generate_disjunction(tables), do:
    {:or, nil, [generate_condition(tables), generate_condition(tables)]}

  defp generate_conjunction(tables), do:
    {:and, nil, [generate_condition(tables), generate_condition(tables)]}

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

  defp generate_value(:boolean), do: {:boolean, [true, false] |> Enum.random(), []}
  defp generate_value(:integer), do: {:integer, :rand.uniform(1000), []}
  defp generate_value(:real), do: {:real, random_float(), []}
  defp generate_value(:text), do: {:text, random_text(), []}
  defp generate_value(:datetime), do: {:datetime, "1970-01-01", []}

  defp generate_select(tables) do
    {select_list, info} = tables |> generate_select_list() |> Enum.unzip()
    {{:select, nil, select_list}, info}
  end

  defp generate_select_list(tables), do:
    many1(fn -> generate_expression_with_info(tables) end)

  defp generate_expression_with_info(tables), do:
    [
      fn -> {{:function, "count", [{:star, nil, []}]}, {:integer, "count"}} end,
      fn -> generate_column_with_info(tables) end,
      fn -> generate_aliased_column_with_info(tables) end,
    ] |> random_option()

  defp generate_aliased_column_with_info(tables) do
    {column, {table, _}} = generate_column_with_info(tables)
    alias = random_name()
    {generate_as(column, alias), {table, alias}}
  end

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

  defp many1(generator), do:
    [
      fn -> [generator.()] end,
      fn -> [generator.() | many1(generator)] end,
    ] |> random_option()

  defp optional(generator), do:
    [
      fn -> {:empty, nil, []} end,
      generator
    ] |> random_option()

  defp random_option(options), do: Enum.random(options).()

  defp random_float(), do: (1 - 2 * :rand.uniform()) * :math.pow(10, :rand.uniform(3))

  @keywords ~w(is as on or by from select)
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
    1..len |> Enum.map(fn(_) -> Enum.random(allowed_chars) end)
  end

  defp random_column(tables) do
    table = Enum.random(tables)
    column = Enum.random(table.columns)
    {column, table}
  end
end
