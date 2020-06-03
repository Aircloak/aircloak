defmodule Cloak.Sql.Compiler.Anonymization do
  @moduledoc """
    This module inspects the AST and detects the type for each individual subquery, which determines the required
    validations in later steps. It also prepares the anonymized subqueries for the anonymized aggregation pipeline.
  """

  alias Cloak.Sql.{Query, Expression, Query.Lenses, Condition}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Compiler.Anonymization.Transformer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Sets the correct type for each subquery in the AST, in order to ensure that user data is processed
  correctly and the query is properly validated.

  A subquery can have the following types:

  - `standard`: an arbitrary SQL query that processes data un-restricted.
    Either the data is not privacy sensitive or the query is the source of the data (virtual tables).

  - `restricted`: an SQL query that selects or aggregates data per-user, as input for an anonymized query, and
    which is subject to Aircloak specific restrictions (aligned ranges, restricted math, etc.).

  - `anonymized`: an SQL query that aggregates per-user, privacy sensitive data into anonymized data. The input
    expressions for the anonymized aggregators are subject to the same Aircloak specific restrictions as `restricted`
    queries are, meaning that, for example, where filters have aligned ranges and restricted math, while having
    filters are un-restricted.
  """
  @spec set_query_type(Query.t()) :: Query.t()
  def set_query_type(query), do: Helpers.apply_bottom_up(query, &%{&1 | type: get_query_type(&1)})

  @doc "Re-writes the anonymized subqueries to support the needs of the anonymized aggregator."
  @spec compile(Query.t()) :: Query.t()
  def compile(query), do: Helpers.apply_bottom_up(query, &compile_anonymization/1, analyst_tables?: false)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp get_query_type(query) do
    if Enum.all?(query.selected_tables, &(&1.content_type == :public)) do
      :standard
    else
      if query.subquery? and Helpers.uid_column_selected?(query) do
        :restricted
      else
        :anonymized
      end
    end
  end

  defp compile_anonymization(%Query{command: :select, type: :anonymized} = query) do
    cond do
      supports_statistics_anonymization?(query) -> convert_to_statistics_anonymization(query)
      needs_uid_grouping?(query) -> Transformer.group_by_uid(query)
      true -> query
    end
  end

  defp compile_anonymization(query), do: query

  defp needs_uid_grouping?(query), do: Enum.all?(query.aggregators, &can_be_uid_grouped?/1)

  @uid_offloaded_aggregators ~w(count sum min max count_noise sum_noise)
  defp can_be_uid_grouped?(aggregator),
    do: aggregator.name in @uid_offloaded_aggregators and not distinct_column_count?(aggregator)

  # -------------------------------------------------------------------
  # Statistics computation
  # -------------------------------------------------------------------

  defp supports_statistics_anonymization?(query) do
    Enum.all?(query.aggregators, &aggregator_supports_statistics?/1) and
      user_id_not_selected?(query) and
      statistics_anonymization_enabled?(query.data_source)
  end

  defp user_id_not_selected?(query) do
    Query.Lenses.leaf_expressions()
    |> Lens.filter(& &1.user_id?)
    |> Lens.to_list(query.group_by ++ Query.order_by_expressions(query))
    |> Enum.empty?()
  end

  defp aggregator_supports_statistics?(%Expression{kind: :function, name: name, type: type})
       when name in ["min", "max"] and type in [:date, :time, :datetime],
       do: false

  defp aggregator_supports_statistics?(aggregator),
    do: distinct_column_count?(aggregator) or can_be_uid_grouped?(aggregator)

  defp distinct_column_count?(%Expression{
         kind: :function,
         name: name,
         args: [{:distinct, %Expression{user_id?: false}}]
       })
       when name in ["count", "count_noise"],
       do: true

  defp distinct_column_count?(_), do: false

  defp statistics_anonymization_enabled?(data_source), do: data_source[:statistics_anonymization] != false

  defp columns_not_distinct_condition(
         %Expression{name: "__ac_grouping_id"} = column1,
         %Expression{name: "__ac_grouping_id"} = column2
       ),
       do: Expression.function("=", [column1, column2], :boolean)

  defp columns_not_distinct_condition(column1, column2),
    do: Expression.function("!<>", [column1, column2], :boolean) |> Map.put(:synthetic?, true)

  defp groups_equal_or_null_conditions(group1, group2) do
    Enum.zip(group1, group2)
    |> Enum.map(fn {column1, column2} -> columns_not_distinct_condition(column1, column2) end)
    |> Enum.reduce(fn condition, accumulator -> Condition.both(accumulator, condition) end)
  end

  defp convert_to_statistics_anonymization(query) do
    uid_grouped_query = query |> Transformer.group_by_uid() |> Transformer.offload_grouping_sets()

    regular_statistics_query = Transformer.compute_main_statistics(uid_grouped_query)
    regular_statistics_table = Helpers.create_table_from_columns(regular_statistics_query.columns, "__ac_regular_stats")
    regular_groups = Transformer.aggregation_groups(regular_statistics_query, regular_statistics_table)

    from = {:subquery, %{ast: regular_statistics_query, alias: regular_statistics_table.name}}

    {from, selected_tables} =
      query.aggregators
      |> target_columns_for_distinct_aggregators()
      |> Enum.with_index()
      |> Enum.reduce({from, [regular_statistics_table]}, fn {distinct_column, index}, {from, selected_tables} ->
        distinct_statistics_query = Transformer.compute_distinct_statistics(query, distinct_column)

        distinct_statistics_table =
          Helpers.create_table_from_columns(distinct_statistics_query.columns, "__ac_distinct_stats#{index}")

        conditions =
          distinct_statistics_query
          |> Transformer.aggregation_groups(distinct_statistics_table)
          |> groups_equal_or_null_conditions(regular_groups)

        from_rhs = {:subquery, %{ast: distinct_statistics_query, alias: distinct_statistics_table.name}}

        # We join each query for distinct statistics to the main query with regular statistics.
        from = {:join, %{type: :inner_join, lhs: from, rhs: from_rhs, condition: conditions}}

        {from, selected_tables ++ [distinct_statistics_table]}
      end)

    # Since only referenced columns are selected from the inner query, we need to add dummy
    # references to the min/max user ids and grouping id, in order to keep them in the aggregation input.
    # The user ids count column has the `user_id?` flag set, so it will be automatically selected,
    # as it will take place of user id column for the synthetic statistics query.
    min_uid_top_ref = Transformer.column_from_synthetic_table(regular_statistics_table, "__ac_min_uid")
    max_uid_top_ref = Transformer.column_from_synthetic_table(regular_statistics_table, "__ac_max_uid")
    grouping_id_top_ref = Transformer.column_from_synthetic_table(regular_statistics_table, "__ac_grouping_id")

    distinct_columns = target_columns_for_distinct_aggregators(uid_grouped_query.aggregators)

    %Query{
      uid_grouped_query
      | from: from,
        selected_tables: selected_tables,
        aggregators: [grouping_id_top_ref, min_uid_top_ref, max_uid_top_ref | uid_grouped_query.aggregators],
        where: nil
    }
    |> Helpers.apply_top_down(&%Query{&1 | anonymization_type: :statistics})
    |> update_in([Transformer.aggregators_lens()], &update_stats_aggregator(&1, selected_tables, distinct_columns))
    |> update_in(
      [Lenses.query_expressions() |> Lens.filter(&is_binary(&1.name)) |> Lens.reject(&(&1.table in selected_tables))],
      &%Expression{&1 | table: regular_statistics_table}
    )
    |> Query.add_debug_info("Using statistics-based anonymization.")
  end

  defp update_stats_aggregator(
         %Expression{args: [{:distinct, %Expression{user_id?: true}}]} = aggregator,
         [regular_statistics_table | _distinct_statistics_tables],
         _distinct_columns
       ) do
    arg = Transformer.column_from_synthetic_table(regular_statistics_table, "__ac_count_duid")
    %Expression{aggregator | args: [{:distinct, arg}]}
  end

  defp update_stats_aggregator(
         %Expression{args: [{:distinct, arg}]} = aggregator,
         [_regular_statistics_table | distinct_statistics_tables],
         distinct_columns
       ) do
    table_index = Enum.find_index(distinct_columns, &Expression.equals?(&1, arg))
    distinct_statistics_table = Enum.at(distinct_statistics_tables, table_index)
    real_count = Transformer.column_from_synthetic_table(distinct_statistics_table, "__ac_count_distinct")
    noise_factor = Transformer.column_from_synthetic_table(distinct_statistics_table, "__ac_noise_factor")
    %Expression{aggregator | args: [{:distinct, [real_count, noise_factor]}]}
  end

  defp update_stats_aggregator(aggregator, [regular_statistics_table | _distinct_statistics_tables], _distinct_columns) do
    [%Expression{name: "__ac_agg_" <> _ = name}] = aggregator.args

    args =
      for input <- ~w(count sum min max stddev),
          do: Transformer.column_from_synthetic_table(regular_statistics_table, "#{name}_#{input}")

    %Expression{aggregator | args: args}
  end

  defp target_columns_for_distinct_aggregators(aggregators) do
    aggregators
    |> Enum.filter(&distinct_column_count?/1)
    |> Enum.map(fn %Expression{args: [{:distinct, target_column}]} -> target_column end)
    |> Enum.uniq()
  end
end
