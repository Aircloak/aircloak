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

  def optimize(%Query{command: :select} = query),
    do: Helpers.apply_top_down(query, &optimize_query/1, analyst_tables?: false)

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
        &%{&1 | ast: optimize_subquery_columns(&1.ast, used_columns_from_table(query, &1.alias))}
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

  defp optimize_subquery_columns(subquery, required_column_names) do
    {columns, column_titles} =
      Enum.zip(subquery.columns, subquery.column_titles)
      |> Enum.filter(fn {column, column_name} ->
        column.user_id? or column_name in required_column_names
      end)
      |> Enum.unzip()

    if subquery.group_by == [] and columns == [] and Enum.any?(subquery.columns, &Helpers.aggregated_column?/1) do
      %Query{subquery | columns: [hd(subquery.columns)], column_titles: [hd(subquery.column_titles)]}
    else
      %Query{subquery | columns: columns, column_titles: column_titles}
    end
  end

  defp optimize_joins(query), do: Lens.map(Lenses.joins(), query, &push_down_simple_conditions/1)

  defp push_down_simple_conditions(join) do
    {lhs, conditions} = move_simple_conditions_into_subqueries(join.lhs, join.condition)
    {rhs, conditions} = move_simple_conditions_into_subqueries(join.rhs, conditions)
    %{join | lhs: lhs, rhs: rhs, condition: conditions}
  end

  defp move_simple_conditions_into_subqueries(branch, conditions) do
    nullable_columns? = has_outer_join?(branch)

    simple_condition? =
      if nullable_columns?,
        do: &(Condition.verb(&1) != :is_null and condition_from_table?(&1, &2)),
        else: &condition_from_table?(&1, &2)

    branch_with_moved_conditions =
      subqueries()
      |> Lens.reject(&(&1.ast.type == :anonymized))
      |> Lens.map(branch, fn subquery ->
        simple_conditions = reject_and_conditions(conditions, &(not simple_condition?.(&1, subquery.alias)))

        %{subquery | ast: move_conditions_into_subquery(subquery.ast, simple_conditions)}
      end)

    unmovable_conditions =
      filter_conditions_from_subqueries(branch, conditions, fn name, acc ->
        if nullable_columns? do
          Lenses.and_conditions()
          |> Lens.filter(&simple_condition?.(&1, name))
          |> Lens.map(
            acc,
            &Expression.function(
              "not",
              [
                Expression.function("is_null", [Condition.subject(&1)], :boolean)
              ],
              :boolean
            )
          )
        else
          reject_and_conditions(acc, &simple_condition?.(&1, name))
        end
      end)

    {branch_with_moved_conditions, unmovable_conditions}
  end

  defp move_conditions_into_subquery(subquery, conditions),
    do:
      Query.Lenses.conditions_terminals()
      |> Lens.filter(&Expression.column?/1)
      |> Lens.map(conditions, &lookup_column_in_query(&1, subquery))
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
      {:subquery, %{ast: _}} -> Lens.at(1) |> Lens.filter(&is_nil(&1.ast.analyst_table))
      _other -> Lens.empty()
    end)
  end

  defp add_conditions_to_query(conditions, %Query{grouping_sets: []} = query),
    do: %Query{query | where: Condition.both(query.where, conditions)}

  defp add_conditions_to_query(conditions, %Query{grouping_sets: [_ | _]} = query),
    do: %Query{query | having: Condition.both(query.having, conditions)}

  defp condition_from_table?(condition, table_name) do
    Query.Lenses.conditions_terminals()
    |> Lens.filter(&Expression.column?/1)
    |> Lens.to_list(condition)
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> case do
      [%{name: ^table_name}] -> true
      _ -> false
    end
  end

  defp lookup_column_in_query(original_column, query) do
    column = Enum.fetch!(query.columns, Enum.find_index(query.column_titles, &(&1 == original_column.name)))
    %Expression{column | alias: original_column.name, source_location: original_column.source_location}
  end

  defp optimize_filters(query) do
    {from, conditions} = move_simple_conditions_into_subqueries(query.from, query.where)
    %Query{query | from: from, where: conditions}
  end

  defp reject_and_conditions(nil, _matcher), do: nil

  defp reject_and_conditions(%Expression{kind: :function, name: "and", args: [lhs, rhs]} = expression, matcher) do
    case {reject_and_conditions(lhs, matcher), reject_and_conditions(rhs, matcher)} do
      {nil, nil} -> nil
      {nil, rhs} -> rhs
      {lhs, nil} -> lhs
      {lhs, rhs} -> %Expression{expression | args: [lhs, rhs]}
    end
  end

  defp reject_and_conditions(%Expression{kind: :function, name: "or"} = expression, _matcher), do: expression

  defp reject_and_conditions(condition, matcher), do: if(matcher.(condition), do: nil, else: condition)
end
