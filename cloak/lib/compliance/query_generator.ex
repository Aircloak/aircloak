defmodule Cloak.Compliance.QueryGenerator do
  @type ast :: {atom, any, [ast]}
  @type table :: String.t


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generates a random AST representing a query into the given tables."
  @spec generate_ast([table]) :: ast
  def generate_ast(tables) do
    {from_table, from_ast} = generate_from(tables)
    {:query, nil, [generate_select(from_table), from_ast, generate_conditions(from_table)]}
  end

  @doc "Generates the SQL query string fro the given AST."
  @spec ast_to_sql(ast) :: iolist
  def ast_to_sql({:select, nil, select_list}), do: [" SELECT ", Enum.map(select_list, &ast_to_sql/1)]
  def ast_to_sql({:from, nil, [{:table, name, []}]}), do: [" FROM ", name]
  def ast_to_sql({:function, name, args}), do: [name, "(", Enum.map(args, &ast_to_sql/1), ")"]
  def ast_to_sql({:star, _, _}), do: "*"
  def ast_to_sql({_, _, items}), do: Enum.map(items, &ast_to_sql/1)


  # -------------------------------------------------------------------
  # Generators
  # -------------------------------------------------------------------

  defp generate_from(tables) do
    table = tables |> Map.values() |> Enum.random()
    {table, {:from, nil, [{:table, table.name, []}]}}
  end

  defp generate_conditions(_table), do: {:empty, nil, []}

  defp generate_select(_table), do: {:select, nil, [{:function, "COUNT", [{:star, nil, []}]}]}
end
