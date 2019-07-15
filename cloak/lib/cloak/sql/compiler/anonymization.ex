defmodule Cloak.Sql.Compiler.Anonymization do
  @moduledoc """
    This module inspects the AST and detects the type for each individual subquery, which determines the required
    validations in later steps. It also prepares the anonymized subqueries for the anonymized aggregation pipeline.
  """

  alias Cloak.Sql.{Query, Expression, Query.Lenses}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.DataSource.Table

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

  def compile_anonymization(%Query{type: :anonymized, from: {:subquery, %{alias: "__ac_uid_grouping"}}} = query) do
    if supports_statistics_anonymization?(query),
      do: convert_to_statistics_anonymization(query),
      else: query
  end

  def compile_anonymization(query), do: query

  defp supports_statistics_anonymization?(query) do
    Enum.all?(query.aggregators, &aggregator_supports_statistics?/1) and user_id_not_selected?(query) and
      statistics_anonymization_enabled?(query.data_source)
  end

  defp user_id_not_selected?(query) do
    Query.Lenses.leaf_expressions()
    |> Lens.filter(& &1.user_id?)
    |> Lens.to_list(query.group_by ++ Query.order_by_expressions(query))
    |> Enum.empty?()
  end

  defp aggregator_supports_statistics?(%Expression{function: function}) when function in ["min", "max"], do: false

  defp aggregator_supports_statistics?(_aggregator), do: true

  defp statistics_anonymization_enabled?(data_source), do: data_source[:statistics_anonymization] != false

  defp offload_grouping_sets(query) do
    {:subquery, uid_grouping_query} = query.from
    [uid_grouping_table] = query.selected_tables

    uid_grouping_query = %{uid_grouping_query | ast: push_grouping_sets(query, uid_grouping_query.ast)}

    grouping_id_column = Table.column("__ac_grouping_id", :integer)
    uid_grouping_table = %{uid_grouping_table | columns: [grouping_id_column | uid_grouping_table.columns]}

    %Query{query | selected_tables: [uid_grouping_table], from: {:subquery, uid_grouping_query}}
  end

  defp select_column_in_query(query, column) do
    %Query{
      query
      | columns: query.columns ++ [column],
        column_titles: query.column_titles ++ [Expression.title(column)]
    }
  end

  defp select_grouping_id_column(query) do
    grouping_id =
      Expression.function("grouping_id", query.group_by, :integer)
      |> set_fields(alias: "__ac_grouping_id", synthetic?: true)

    select_column_in_query(query, grouping_id)
  end

  defp add_user_id_to_grouping_sets(grouping_sets, group_by) do
    uid_index = Enum.find_index(group_by, & &1.user_id?)
    Enum.map(grouping_sets, &[uid_index | &1])
  end

  defp translate_grouping_sets(grouping_sets, group_by, target_query) do
    Enum.map(grouping_sets, fn grouping_set ->
      Enum.map(grouping_set, fn top_index ->
        top_column = Enum.at(group_by, top_index)
        select_index = Enum.find_index(target_query.column_titles, &(&1 == top_column.name))
        inner_column = Enum.at(target_query.columns, select_index)
        Enum.find_index(target_query.group_by, &Expression.equals?(&1, inner_column))
      end)
    end)
  end

  defp push_grouping_sets(%Query{grouping_sets: grouping_sets, group_by: group_by}, inner_query)
       when length(grouping_sets) > 1 do
    grouping_sets =
      grouping_sets
      |> translate_grouping_sets(group_by, inner_query)
      |> add_user_id_to_grouping_sets(inner_query.group_by)

    # Groups marked as synthetic will be ignored by the noise layers compiler.
    group_by = Enum.map(inner_query.group_by, &set_fields(&1, synthetic?: true))

    %Query{inner_query | grouping_sets: grouping_sets, group_by: group_by}
    |> select_grouping_id_column()
  end

  defp push_grouping_sets(_top_query, inner_query) do
    grouping_id = Expression.constant(:integer, 0) |> set_fields(alias: "__ac_grouping_id", synthetic?: true)
    select_column_in_query(inner_query, grouping_id)
  end

  defp convert_to_statistics_anonymization(query) do
    query = offload_grouping_sets(query)

    {:subquery, %{ast: uid_grouping_query}} = query.from
    [uid_grouping_table] = query.selected_tables

    grouping_id = column_from_synthetic_table(uid_grouping_table, "__ac_grouping_id")
    groups = [grouping_id | aggregation_groups(uid_grouping_query, uid_grouping_table)]

    {count_duid, min_uid, max_uid} = uid_statistics(uid_grouping_table)
    aggregators = [count_duid, min_uid, max_uid | aggregation_statistics(query.aggregators)]

    inner_columns = Enum.uniq(groups ++ aggregators)
    order_by = groups ++ [min_uid, max_uid]

    inner_query = %Query{
      query
      | subquery?: true,
        type: :restricted,
        aggregators: aggregators,
        columns: inner_columns,
        column_titles: Enum.map(inner_columns, &Expression.title(&1)),
        group_by: Enum.map(groups, &Expression.unalias/1),
        grouping_sets: Helpers.default_grouping_sets(groups),
        order_by: order_by |> Enum.map(&Expression.unalias/1) |> Enum.map(&{&1, :asc, :nulls_natural}),
        having: statistics_buckets_filter(query, count_duid),
        limit: nil,
        offset: 0,
        sample_rate: nil,
        distinct?: false,
        implicit_count?: false
    }

    inner_table = Helpers.create_table_from_columns(inner_columns, "__ac_statistics")

    # Since only referenced columns are selected from the inner query, we need to add dummy
    # references to the min/max user ids and grouping id, in order to keep them in the aggregation input.
    # The user ids count column has the `user_id?` flag set, so it will be automatically selected,
    # as it will take place of user id column for the synthetic statistics query.
    min_uid_top_ref = column_from_synthetic_table(inner_table, "__ac_min_uid")
    max_uid_top_ref = column_from_synthetic_table(inner_table, "__ac_max_uid")
    grouping_id_top_ref = column_from_synthetic_table(inner_table, "__ac_grouping_id")

    %Query{
      query
      | from: {:subquery, %{ast: inner_query, alias: inner_table.name}},
        selected_tables: [inner_table],
        aggregators: [grouping_id_top_ref, min_uid_top_ref, max_uid_top_ref | query.aggregators],
        where: nil
    }
    |> update_in(
      [Lenses.query_expressions() |> Lens.filter(&is_binary(&1.name))],
      &set_fields(&1, table: inner_table, synthetic?: true)
    )
    |> update_in(
      [Lenses.query_expressions() |> Lens.filter(& &1.aggregate?)],
      &update_aggregator/1
    )
    |> Query.add_debug_info("Using statistics-based anonymization.")
  end

  defp column_from_synthetic_table(table, name) do
    table
    |> Helpers.column_from_table(name)
    |> set_fields(synthetic?: true)
  end

  defp set_fields(expression, fields) do
    Enum.reduce(fields, expression, fn {name, value}, acc ->
      Map.put(acc, name, value)
    end)
  end

  defp update_aggregator(
         %Expression{function_args: [{:distinct, %Expression{user_id?: true, table: inner_table}}]} = aggregator
       ) do
    arg = column_from_synthetic_table(inner_table, "__ac_count_duid")
    %Expression{aggregator | function_args: [{:distinct, arg}]}
  end

  defp update_aggregator(aggregator) do
    [%Expression{name: "__ac_agg_" <> _ = name, table: inner_table}] = aggregator.function_args
    args = for input <- ~w(count sum min max stddev), do: column_from_synthetic_table(inner_table, "#{name}_#{input}")
    %Expression{aggregator | function_args: args}
  end

  defp uid_statistics(uid_grouping_table) do
    uid_column = column_from_synthetic_table(uid_grouping_table, uid_grouping_table.user_id)
    true = uid_column != nil

    count_duid =
      Expression.function("count", [Expression.column(uid_column, uid_grouping_table)], :integer, true)
      |> set_fields(alias: "__ac_count_duid", user_id?: true, synthetic?: true)

    min_uid =
      Expression.function("min", [Expression.column(uid_column, uid_grouping_table)], uid_column.type, true)
      |> set_fields(alias: "__ac_min_uid", synthetic?: true)

    max_uid =
      Expression.function("max", [Expression.column(uid_column, uid_grouping_table)], uid_column.type, true)
      |> set_fields(alias: "__ac_max_uid", synthetic?: true)

    {count_duid, min_uid, max_uid}
  end

  defp aggregation_statistics(aggregators) do
    Enum.flat_map(aggregators, fn
      %Expression{function_args: [{:distinct, %Expression{user_id?: true}}]} ->
        []

      %Expression{aggregate?: true, function_args: [column]} ->
        for {function, type} <- [
              {"count", :integer},
              {"sum", column.type},
              {"min", column.type},
              {"max", column.type},
              {"stddev", :real}
            ] do
          function
          |> Expression.function([column], type, true)
          |> set_fields(alias: "#{column.name}_#{function}", synthetic?: true)
        end
    end)
  end

  defp aggregation_groups(uid_grouping_query, uid_grouping_table) do
    # It would be more efficient (and simpler) to group the statistics query by the final grouping expressions.
    # But that is not possible to do because noise layers need to access the raw columns used in the various clauses.
    uid_grouping_query.columns
    |> Enum.take(Enum.count(uid_grouping_query.group_by))
    |> Enum.reject(& &1.user_id?)
    |> Enum.map(&column_from_synthetic_table(uid_grouping_table, Expression.title(&1)))
  end

  defp statistics_buckets_filter(query, count_duid) do
    if Query.lcf_buckets_aggregation_limit(query) == 0 do
      # This is an optimization in case we dont care about censored buckets at all.
      {:comparison, count_duid, :>, Expression.constant(:integer, 1)}
    else
      nil
    end
  end
end
