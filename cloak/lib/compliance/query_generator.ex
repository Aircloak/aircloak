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
  def ast_to_sql({:subquery, name, [definition]}), do: ["( ", ast_to_sql(definition), " ) AS ", name]
  def ast_to_sql({:where, nil, [condition]}), do: [" WHERE ", ast_to_sql(condition)]
  def ast_to_sql({:group_by, nil, group_list}), do:
    [" GROUP BY ", Enum.map(group_list, &ast_to_sql/1) |> Enum.intersperse(", ")]
  def ast_to_sql({:having, nil, [condition]}), do: [" HAVING ", ast_to_sql(condition)]
  def ast_to_sql({:=, nil, [lhs, rhs]}), do: [ast_to_sql(lhs), " = ", ast_to_sql(rhs)]
  def ast_to_sql({:between, nil, [lhs, low, high]}), do:
    [ast_to_sql(lhs), " BETWEEN ", ast_to_sql(low), " AND ", ast_to_sql(high)]
  def ast_to_sql({:and, nil, [lhs, rhs]}), do: [" (", ast_to_sql(lhs), " AND ", ast_to_sql(rhs), ") "]
  def ast_to_sql({:function, name, args}), do: [name, "(", Enum.map(args, &ast_to_sql/1), ")"]
  def ast_to_sql({:column, {column, table}, []}), do: [?", table, ?", ?., ?", column, ?"]
  def ast_to_sql({:integer, value, []}), do: to_string(value)
  def ast_to_sql({:text, value, []}), do: [?', value, ?']
  def ast_to_sql({:boolean, value, []}), do: to_string(value)
  def ast_to_sql({:datetime, value, []}), do: [?', value, ?']
  def ast_to_sql({:real, value, []}), do: to_string(value)
  def ast_to_sql({:star, _, _}), do: "*"
  def ast_to_sql({:empty, _, _}), do: ""


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
    ]}

    {ast, info}
  end

  defp generate_from(tables), do:
    [
      fn -> generate_from_table(tables) end,
      fn -> generate_from_subquery(tables) end,
    ] |> random_option()

  defp generate_from_table(tables) do
    table = Enum.random(tables)
    {{:from, nil, [{:table, table.name, []}]}, [table]}
  end

  defp generate_from_subquery(tables) do
    name = random_name()
    {ast, info} = generate_ast_with_info(tables)

    {{:from, nil, [{:subquery, name, [ast]}]}, [table_from_ast_info(name, info)]}
  end

  defp table_from_ast_info(name, ast_info), do:
    %{name: name, columns: Enum.map(ast_info, fn({type, name}) -> %{name: name, type: type} end)}

  defp generate_where(tables), do:
    {:where, nil, [generate_condition(tables)]}

  defp generate_group_by(tables), do:
    {:group_by, nil, generate_group_list(tables)}

  defp generate_having(tables), do:
    {:having, nil, [generate_condition(tables)]}

  defp generate_group_list(tables), do:
    [
      fn -> [generate_column(tables)] end,
      fn -> [generate_column(tables) | generate_group_list(tables)] end
    ] |> random_option()

  defp generate_condition(tables), do:
    [
      fn -> generate_equality(tables) end,
      fn -> generate_between(tables) end,
      fn -> generate_conjunction(tables) end,
    ] |> random_option()

  defp generate_conjunction(tables), do:
    {:and, nil, [generate_condition(tables), generate_condition(tables)]}

  defp generate_equality(tables) do
    {column, table} = random_column(tables)
    value = generate_value(column.type)
    {:=, nil, [column_expression(column, table), value]}
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
    [
      fn -> [generate_expression_with_info(tables)] end,
      fn -> [generate_expression_with_info(tables) | generate_select_list(tables)] end,
    ] |> random_option()

  defp generate_expression_with_info(tables), do:
    [
      fn -> {{:function, "count", [{:star, nil, []}]}, {:integer, "count"}} end,
      fn -> generate_column_with_info(tables) end,
    ] |> random_option()

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

  defp optional(generator), do:
    [
      fn -> {:empty, nil, []} end,
      generator
    ] |> random_option()

  defp random_option(options), do: Enum.random(options).()

  defp random_float(), do: (1 - 2 * :rand.uniform()) * :math.pow(10, :rand.uniform(3))

  defp random_name(), do: random_text(?a..?z)

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
