defmodule Cloak.Sql.Compiler.Optimizer do
  @moduledoc "Module for optimizing query execution."

  alias Cloak.Sql.{Expression, Query, Condition}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses

  use Lens.Macros

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
    Rewrites the query in order to get faster execution, i.e.:
    unused columns are dropped, filters are pushed down into the bottom subqueries.
  """
  @spec optimize(Query.t()) :: Query.t()
  def optimize(%Query{command: :show} = query), do: query

  def optimize(%Query{command: :select} = query), do: Helpers.apply_top_down(query, &optimize_query/1)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp optimize_query(query),
    do:
      query
      |> optimize_filters()
      |> optimize_joins()
      |> optimize_columns_from_subqueries()

  def optimize_columns_from_subqueries(query),
    do:
      Lens.map(
        Query.Lenses.direct_subqueries(),
        query,
        &%{&1 | ast: optimized_subquery_ast(&1.ast, used_columns_from_table(query, &1.alias))}
      )

  defp used_columns_from_table(query, table_name),
    # all db columns of the outer query which are from this table, except the user id
    do:
      Lens.both(Lenses.terminals(), Lenses.join_conditions_terminals())
      |> Lenses.leaf_expressions()
      |> Lens.to_list(query)
      |> Enum.filter(&(&1.table != :unknown and &1.table.name == table_name))
      |> Enum.uniq_by(&Expression.id/1)
      |> Enum.map(& &1.name)

  defp optimized_subquery_ast(ast, required_column_names) do
    {columns, column_titles} =
      Enum.zip(ast.columns, ast.column_titles)
      |> Enum.filter(fn {column, column_name} ->
        column.user_id? or column_name in required_column_names
      end)
      |> Enum.unzip()

    %Query{ast | columns: columns, column_titles: column_titles}
  end

  defp optimize_joins(query), do: Lens.map(Lenses.joins(), query, &push_down_simple_conditions/1)

  defp push_down_simple_conditions(join) do
    {lhs, conditions} = move_simple_conditions_into_subqueries(join.lhs, join.conditions)
    {rhs, conditions} = move_simple_conditions_into_subqueries(join.rhs, conditions)
    %{join | lhs: lhs, rhs: rhs, conditions: conditions}
  end

  defp move_simple_conditions_into_subqueries(branch, conditions) do
    nullable_columns? = has_outer_join?(branch)

    simple_condition? =
      if nullable_columns?,
        do: &(Condition.verb(&1) != :is and condition_from_table?(&1, &2)),
        else: &condition_from_table?(&1, &2)

    branch_with_moved_conditions =
      subqueries()
      |> Lens.reject(&(&1.ast.type == :anonymized))
      |> Lens.map(branch, fn subquery ->
        simple_conditions = Condition.reject(conditions, &(not simple_condition?.(&1, subquery.alias)))

        %{subquery | ast: move_conditions_into_subquery(subquery.ast, simple_conditions)}
      end)

    unmovable_conditions =
      filter_conditions_from_subqueries(branch, conditions, fn name, acc ->
        if nullable_columns? do
          Lenses.conditions()
          |> Lens.filter(&simple_condition?.(&1, name))
          |> Lens.map(acc, &{:not, {:is, Condition.subject(&1), :null}})
        else
          Condition.reject(acc, &simple_condition?.(&1, name))
        end
      end)

    {branch_with_moved_conditions, unmovable_conditions}
  end

  defp move_conditions_into_subquery(subquery, conditions),
    do:
      Query.Lenses.conditions_terminals()
      |> Lens.reject(& &1.constant?)
      |> Lens.map(conditions, &lookup_column_in_query(&1.name, subquery))
      |> add_conditions_to_query(subquery)

  defp filter_conditions_from_subqueries(branch, conditions, filter),
    do:
      subqueries()
      |> Lens.reject(&(&1.ast.type == :anonymized))
      |> Lens.to_list(branch)
      |> Enum.map(& &1.alias)
      |> Enum.reduce(conditions, filter)

  defp has_outer_join?({:join, join}),
    do: join.type in [:left_outer_join, :right_outer_join] or has_outer_join?(join.lhs) or has_outer_join?(join.rhs)

  defp has_outer_join?(_), do: false

  deflensp subqueries() do
    Lens.match(fn
      {:join, _} -> Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> subqueries()
      {:subquery, %{ast: _}} -> Lens.at(1)
      _other -> Lens.empty()
    end)
  end

  defp add_conditions_to_query(conditions, %Query{group_by: []} = query),
    do: %Query{query | where: Condition.combine(:and, query.where, conditions)}

  defp add_conditions_to_query(conditions, %Query{group_by: [_ | _]} = query),
    do: %Query{query | having: Condition.combine(:and, query.having, conditions)}

  defp condition_from_table?(condition, table_name) do
    Query.Lenses.conditions_terminals()
    |> Lens.reject(& &1.constant?)
    |> Lens.to_list(condition)
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> case do
      [%{name: ^table_name}] -> true
      _ -> false
    end
  end

  defp lookup_column_in_query(name, query) do
    column = Enum.fetch!(query.columns, Enum.find_index(query.column_titles, &(&1 == name)))
    %Expression{column | alias: name}
  end

  defp optimize_filters(query) do
    {from, conditions} = move_simple_conditions_into_subqueries(query.from, query.where)
    %Query{query | from: from, where: conditions}
  end
end
