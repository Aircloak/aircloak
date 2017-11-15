defmodule Cloak.Sql.Compiler.Optimizer do
  @moduledoc "Module for optimizing query execution."

  alias Cloak.Sql.{Expression, Query}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Rewrites the query in order to improve performance."
  @spec optimize(Query.t) :: Query.t
  def optimize(%Query{command: :show} = query), do: query
  def optimize(%Query{command: :select} = query), do:
    Helpers.apply_top_down(query, &optimize_query/1)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp optimize_query(query) do
    optimize_columns_from_subqueries(query)
  end

  defp optimize_columns_from_subqueries(query), do:
    Lens.map(
      Query.Lenses.direct_subqueries(),
      query,
      &%{&1 | ast: optimized_subquery_ast(&1.ast, used_columns_from_table(query, &1.alias))}
    )

  defp used_columns_from_table(query, table_name) do
    # all db columns of the outer query which are from this table, except the user id
    all_terminals = Lens.both(Lenses.terminals(), Lenses.join_conditions_terminals()) |> Lens.to_list(query)
    Lenses.leaf_expressions()
    |> Lens.to_list(all_terminals)
    |> Enum.filter(& &1.table != :unknown and &1.table.name == table_name)
    |> Enum.uniq_by(&Expression.id/1)
    |> Enum.map(& &1.name)
  end

  defp optimized_subquery_ast(ast, required_column_names) do
    {columns, column_titles} =
      Enum.zip(ast.columns, ast.column_titles)
      |> Enum.filter(fn ({column, column_name}) -> column.user_id? or column_name in required_column_names end)
      |> Enum.unzip()
    %Query{ast | columns: columns, column_titles: column_titles}
  end
end
