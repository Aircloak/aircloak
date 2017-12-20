defmodule Cloak.Sql.Compiler.Optimizer do
  @moduledoc "Module for optimizing query execution."

  alias Cloak.Sql.{Expression, Query, Condition}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Rewrites the query in order to get faster execution, i.e.: unused columns are dropped."
  @spec optimize(Query.t) :: Query.t
  def optimize(%Query{command: :show} = query), do: query
  def optimize(%Query{command: :select} = query), do:
    Helpers.apply_top_down(query, &optimize_query/1)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp optimize_query(query), do:
    query
    |> optimize_joins()
    |> optimize_columns_from_subqueries()

  defp optimize_columns_from_subqueries(query), do:
    Lens.map(
      Query.Lenses.direct_subqueries(),
      query,
      &%{&1 | ast: optimized_subquery_ast(&1.ast, used_columns_from_table(query, &1.alias))}
    )

  defp used_columns_from_table(query, table_name), do:
    # all db columns of the outer query which are from this table, except the user id
    Lens.both(Lenses.terminals(), Lenses.join_conditions_terminals())
    |> Lenses.leaf_expressions()
    |> Lens.to_list(query)
    |> Enum.filter(& &1.table != :unknown and &1.table.name == table_name)
    |> Enum.uniq_by(&Expression.id/1)
    |> Enum.map(& &1.name)

  defp optimized_subquery_ast(ast, required_column_names) do
    {columns, column_titles} =
      Enum.zip(ast.columns, ast.column_titles)
      |> Enum.filter(fn ({column, column_name}) -> column.user_id? or column_name in required_column_names end)
      |> Enum.unzip()
    %Query{ast | columns: columns, column_titles: column_titles}
  end

  defp optimize_joins(query), do:
    Lens.map(Lenses.joins(), query, &push_down_simple_conditions/1)

  defp push_down_simple_conditions(join) do
    {lhs, conditions} = move_simple_conditions_in_subquery(join.lhs, join.conditions)
    {rhs, conditions} = move_simple_conditions_in_subquery(join.rhs, conditions)
    %{join | lhs: lhs, rhs: rhs, conditions: conditions}
  end

  defp move_simple_conditions_in_subquery({:subquery, subquery}, conditions) do
    # we first separate moveable conditions from the others
    {simple_conditions, other_conditions} = Condition.partition(conditions, &condition_from_table?(&1, subquery.alias))
    # we move the simple conditions into the inner subquery, after expanding the referenced columns
    ast =
      Query.Lenses.conditions_terminals()
      |> Lens.satisfy(&not &1.constant?)
      |> Lens.map(simple_conditions, &lookup_column_in_query(&1.name, subquery.ast))
      |> add_conditions_to_query(subquery.ast)
    {{:subquery, %{subquery | ast: ast}}, other_conditions}
  end
  defp move_simple_conditions_in_subquery(branch, conditions), do: {branch, conditions}

  defp add_conditions_to_query(conditions, %Query{group_by: []} = query), do:
    %Query{query | where: Condition.combine(:and, query.where, conditions)}
  defp add_conditions_to_query(conditions, %Query{group_by: [_|_]} = query), do:
    %Query{query | having: Condition.combine(:and, query.having, conditions)}

  defp condition_from_table?(condition, table_name) do
    Query.Lenses.conditions_terminals()
    |> Lens.satisfy(&not &1.constant?)
    |> Lens.to_list(condition)
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> case do
      [%{name: ^table_name}] -> true
      _ -> false
    end
  end

  defp lookup_column_in_query(name, query) do
    Enum.fetch!(query.columns, Enum.find_index(query.column_titles, & &1 == name))
  end
end
