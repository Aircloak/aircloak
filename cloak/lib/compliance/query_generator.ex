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
  def ast_to_sql({:column, name, []}), do: name
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
    {from_ast, from_table} = generate_from(tables)
    {select_ast, info} = generate_select(from_table)
    ast = {:query, nil, [
      select_ast,
      from_ast,
      optional(fn -> generate_where(from_table) end),
      optional(fn -> generate_group_by(from_table) end),
      optional(fn -> generate_having(from_table) end),
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
    {{:from, nil, [{:table, table.name, []}]}, table}
  end

  defp generate_from_subquery(tables) do
    name = random_name()
    {ast, info} = generate_ast_with_info(tables)

    {{:from, nil, [{:subquery, name, [ast]}]}, table_from_ast_info(info)}
  end

  defp table_from_ast_info(ast_info), do:
    %{name: name, columns: Enum.map(ast_info, fn({type, name}) -> %{name: name, type: type} end)}

  defp generate_where(table), do:
    {:where, nil, [generate_condition(table)]}

  defp generate_group_by(table), do:
    {:group_by, nil, generate_group_list(table)}

  defp generate_having(table), do:
    {:having, nil, [generate_condition(table)]}

  defp generate_group_list(table), do:
    [
      fn -> [generate_column(table)] end,
      fn -> [generate_column(table) | generate_group_list(table)] end
    ] |> random_option()

  defp generate_condition(table), do:
    [
      fn -> generate_equality(table) end,
      fn -> generate_between(table) end,
      fn -> generate_conjunction(table) end,
    ] |> random_option()

  defp generate_conjunction(table), do:
    {:and, nil, [generate_condition(table), generate_condition(table)]}

  defp generate_equality(table) do
    column = Enum.random(table.columns)
    value = generate_value(column.type)
    {:=, nil, [column_expression(column), value]}
  end

  defp generate_between(table) do
    column = Enum.random(table.columns)
    {:between, nil, [column_expression(column), generate_value(column.type), generate_value(column.type)]}
  end

  defp generate_value(:boolean), do: {:boolean, [true, false] |> Enum.random(), []}
  defp generate_value(:integer), do: {:integer, :rand.uniform(1000), []}
  defp generate_value(:real), do: {:real, random_float(), []}
  defp generate_value(:text), do: {:text, random_text(), []}
  defp generate_value(:datetime), do: {:datetime, "1970-01-01", []}

  defp generate_select(table) do
    {select_list, info} = table |> generate_select_list() |> Enum.unzip()
    {{:select, nil, select_list}, info}
  end

  defp generate_select_list(table), do:
    [
      fn -> [generate_expression_with_info(table)] end,
      fn -> [generate_expression_with_info(table) | generate_select_list(table)] end,
    ] |> random_option()

  defp generate_expression_with_info(table), do:
    [
      fn -> {{:function, "COUNT", [{:star, nil, []}]}, {:integer, "COUNT"}} end,
      fn -> generate_column_with_info(table) end,
    ] |> random_option()

  defp generate_column(table) do
    {column, _} = generate_column_with_info(table)
    column
  end

  defp generate_column_with_info(table) do
    column = Enum.random(table.columns)
    {column_expression(column), {column.type, column.name}}
  end

  defp column_expression(%{name: name}), do: {:column, name, []}


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
end
