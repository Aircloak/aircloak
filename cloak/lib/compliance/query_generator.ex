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
    {from_table, from_ast} = generate_from(tables)
    {:query, nil, [generate_select(from_table), from_ast, generate_conditions(from_table)]}
  end

  @doc "Generates the SQL query string fro the given AST."
  @spec ast_to_sql(ast) :: iolist
  def ast_to_sql({:query, _, items}), do: Enum.map(items, &ast_to_sql/1)
  def ast_to_sql({:select, nil, select_list}), do: [" SELECT ", Enum.map(select_list, &ast_to_sql/1)]
  def ast_to_sql({:from, nil, [{:table, name, []}]}), do: [" FROM ", name]
  def ast_to_sql({:where, nil, conditions}), do: [" WHERE ", Enum.map(conditions, &ast_to_sql/1)]
  def ast_to_sql({:=, nil, [lhs, rhs]}), do: [ast_to_sql(lhs), " = ", ast_to_sql(rhs)]
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

  defp generate_from(tables) do
    table = Enum.random(tables)
    {table, {:from, nil, [{:table, table.name, []}]}}
  end

  defp generate_conditions(table) do
    [
      fn -> {:empty, nil, []} end,
      fn -> generate_where(table) end,
    ] |> random_option()
  end

  defp generate_where(table) do
    column = Enum.random(table.columns)
    value = generate_value(column.type)
    {:where, nil, [{:=, nil, [column_expression(column), value]}]}
  end

  defp generate_value(:boolean), do: {:boolean, [true, false] |> Enum.random(), []}
  defp generate_value(:integer), do: {:integer, :rand.uniform(1000), []}
  defp generate_value(:real), do: {:real, random_float(), []}
  defp generate_value(:text), do: {:text, random_text(), []}
  defp generate_value(:datetime), do: {:datetime, "1970-01-01", []}

  defp random_float(), do: (1 - 2 * :rand.uniform()) * :math.pow(10, :rand.uniform(3))

  defp random_text() do
    len = :rand.uniform(10)
    1..len |> Enum.map(fn(_) -> Enum.random(?A..?z) end)
  end

  defp column_expression(%{name: name}), do: {:column, name, []}

  defp generate_select(_table), do: {:select, nil, [{:function, "COUNT", [{:star, nil, []}]}]}

  defp random_option(options), do: Enum.random(options).()
end
