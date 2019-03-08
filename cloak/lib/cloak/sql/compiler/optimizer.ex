defmodule Cloak.Sql.Compiler.Optimizer do
  @moduledoc "Module for optimizing query execution."

  alias Cloak.Sql.{Expression, Query, Condition, Function}
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

  @doc "Rewrites anonymizing queries in order to offload per-user grouping and aggregation to the backend."
  @spec optimize_per_user_aggregation(Query.t()) :: Query.t()
  def optimize_per_user_aggregation(%Query{command: :show} = query), do: query

  def optimize_per_user_aggregation(%Query{command: :select} = query),
    do: Helpers.apply_bottom_up(query, &offload_per_user_aggregation/1, analyst_tables?: false)

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
      |> Lens.reject(&Expression.constant?/1)
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

  defp add_conditions_to_query(conditions, %Query{group_by: []} = query),
    do: %Query{query | where: Condition.combine(:and, query.where, conditions)}

  defp add_conditions_to_query(conditions, %Query{group_by: [_ | _]} = query),
    do: %Query{query | having: Condition.combine(:and, query.having, conditions)}

  defp condition_from_table?(condition, table_name) do
    Query.Lenses.conditions_terminals()
    |> Lens.reject(&Expression.constant?/1)
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

  # -------------------------------------------------------------------
  # Offload per-user grouping
  # -------------------------------------------------------------------

  def offload_per_user_aggregation(query) do
    if query.type == :anonymized and needs_uid_grouping?(query),
      do: group_by_uid(query),
      else: query
  end

  defp needs_uid_grouping?(query), do: Enum.all?(query.aggregators, &can_be_uid_grouped?/1)

  @uid_offloaded_aggregators ~w(count sum min max count_noise sum_noise)
  defp can_be_uid_grouped?(aggregator),
    do: aggregator.function in @uid_offloaded_aggregators and not distinct_input?(aggregator.function_args)

  defp distinct_input?([{:distinct, %Expression{user_id?: false}}]), do: true
  defp distinct_input?([_]), do: false

  defp group_by_uid(query) do
    user_id = %Expression{Helpers.id_column(query) | synthetic?: true}

    base_columns =
      query
      |> required_groups()
      |> Enum.reject(& &1.user_id?)
      |> Enum.uniq_by(&Expression.semantic/1)
      |> Enum.with_index()
      |> Enum.map(fn {column, index} -> %Expression{column | alias: "__ac_group_#{index}"} end)

    aggregated_columns =
      query.aggregators
      |> Enum.reject(&match?(%Expression{function_args: [{:distinct, %Expression{user_id?: true}}]}, &1))
      |> Enum.map(&uid_aggregator/1)
      |> Enum.uniq_by(&Expression.semantic/1)
      |> Enum.with_index()
      |> Enum.map(fn {expression, index} ->
        %Expression{expression | alias: "__ac_agg_#{index}", synthetic?: true}
      end)

    grouped_columns = [user_id | base_columns]
    inner_columns = grouped_columns ++ aggregated_columns

    inner_query = %Query{
      query
      | subquery?: true,
        type: :restricted,
        aggregators: aggregated_columns,
        columns: inner_columns,
        column_titles: Enum.map(inner_columns, &Expression.title/1),
        group_by: Enum.map(grouped_columns, &Expression.unalias/1),
        order_by: [],
        having: nil,
        limit: nil,
        offset: 0,
        sample_rate: nil,
        distinct?: false,
        implicit_count?: false
    }

    inner_table = Helpers.create_table_from_columns(inner_columns, "__ac_uid_grouping")

    %Query{
      query
      | from: {:subquery, %{ast: inner_query, alias: inner_table.name}},
        selected_tables: [inner_table],
        where: nil
    }
    |> update_in(
      [Lenses.query_expressions() |> Lens.filter(& &1.aggregate?)],
      &update_aggregator(&1, inner_table, aggregated_columns)
    )
    |> update_in(
      [
        Lenses.query_expressions()
        |> Lens.filter(&Expression.member?(grouped_columns, &1))
        |> Lens.reject(&(&1.table == inner_table))
      ],
      &update_base_column(&1, inner_table, grouped_columns)
    )
  end

  defp uid_aggregator(%Expression{function: "count_noise"} = expression),
    do: uid_aggregator(%Expression{expression | function: "count", type: :integer})

  defp uid_aggregator(%Expression{function: "sum_noise", function_args: [arg]} = expression),
    do: uid_aggregator(%Expression{expression | function: "sum", type: Function.type(arg)})

  defp uid_aggregator(%Expression{function_args: [{:distinct, %Expression{user_id?: true} = user_id}]}),
    do: user_id

  defp uid_aggregator(aggregator), do: aggregator

  defp update_aggregator(
         %Expression{function_args: [{:distinct, %Expression{user_id?: true}}]} = aggregator,
         _inner_table,
         _aggregated_columns
       ),
       do: aggregator

  defp update_aggregator(old_aggregator, inner_table, aggregated_columns) do
    uid_aggregator = uid_aggregator(old_aggregator)
    index = Enum.find_index(aggregated_columns, &Expression.equals?(&1, uid_aggregator))
    true = index != nil
    column_name = "__ac_agg_#{index}"
    inner_column = Helpers.column_from_table(inner_table, column_name)
    function_name = global_aggregator(old_aggregator.function)
    new_aggregator = Expression.function(function_name, [inner_column], inner_column.type, true)
    %Expression{new_aggregator | alias: old_aggregator.function}
  end

  defp global_aggregator("count"), do: "sum"
  defp global_aggregator("count_noise"), do: "sum_noise"
  defp global_aggregator(function_name), do: function_name

  defp update_base_column(%Expression{user_id?: true}, inner_table, _base_columns),
    do: Helpers.column_from_table(inner_table, inner_table.user_id)

  defp update_base_column(column, inner_table, base_columns) do
    base_column = Enum.find(base_columns, &Expression.equals?(&1, column))
    Helpers.column_from_table(inner_table, base_column.alias)
  end

  defp required_groups(%Query{group_by: []} = query),
    do: query |> Helpers.aggregator_sources() |> Enum.flat_map(&extract_groups/1)

  defp required_groups(%Query{group_by: groups}), do: Enum.reject(groups, &Expression.constant?/1)

  defp extract_groups(%Expression{name: name} = column) when is_binary(name), do: [column]

  defp extract_groups(%Expression{function?: true, aggregate?: false} = expression) do
    if Helpers.aggregated_column?(expression) or uses_multiple_columns?(expression),
      do: Enum.flat_map(expression.function_args, &extract_groups/1),
      else: [expression]
  end

  defp extract_groups(_), do: []

  defp uses_multiple_columns?(expression) do
    Lenses.leaf_expressions()
    |> Lens.filter(&(&1.name != nil))
    |> Lens.to_list(expression)
    |> Enum.count() > 1
  end
end
