defmodule Cloak.Sql.Compiler.Anonymization do
  @moduledoc """
    This module inspects the AST and detects the type for each individual subquery, which determines the required
    validations in later steps. It also prepares the anonymized subqueries for the anonymized aggregation pipeline.
  """

  alias Cloak.Sql.{Query, Expression, Function, Query.Lenses}
  alias Cloak.DataSource.Table
  alias Cloak.Sql.Compiler.Helpers

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
  def compile(query), do: Helpers.apply_bottom_up(query, &compile_anonymization/1)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp get_query_type(query) do
    if Enum.all?(query.selected_tables, &(&1.user_id == nil)) do
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
    Enum.all?(query.aggregators, &aggregator_supports_statistics?/1) and user_id_not_selected?(query)
  end

  defp user_id_not_selected?(query) do
    Query.Lenses.leaf_expressions()
    |> Lens.filter(& &1.user_id?)
    |> Lens.to_list(query.group_by ++ Query.order_by_expressions(query))
    |> Enum.empty?()
  end

  defp aggregator_supports_statistics?(%Expression{function: function, type: type})
       when function in ["min", "max"] and type in [:date, :time, :datetime],
       do: false

  defp aggregator_supports_statistics?(_aggregator), do: true

  defp convert_to_statistics_anonymization(query) do
    {:subquery, %{ast: uid_grouping_query}} = query.from
    [uid_grouping_table] = query.selected_tables

    groups = aggregation_groups(uid_grouping_query, uid_grouping_table)

    {count_duid, min_uid, max_uid} = uid_statistics(uid_grouping_table)
    aggregators = [count_duid, min_uid, max_uid | aggregation_statistics(query.aggregators)]

    inner_columns = Enum.uniq(groups ++ aggregators)

    inner_query = %Query{
      query
      | subquery?: true,
        type: :restricted,
        aggregators: aggregators,
        columns: inner_columns,
        column_titles: Enum.map(inner_columns, &(&1.alias || &1.name)),
        group_by: Enum.map(groups, &Expression.unalias/1),
        order_by: [min_uid, max_uid] |> Enum.map(&Expression.unalias/1) |> Enum.map(&{&1, :asc, :nulls_first}),
        having: nil,
        limit: nil,
        offset: 0,
        sample_rate: nil,
        distinct?: false,
        implicit_count?: false
    }

    table_columns = Enum.map(inner_columns, &Table.column(&1.alias || &1.name, Function.type(&1)))
    inner_table = Table.new("__ac_statistics", count_duid.alias, columns: table_columns)

    # Since only referenced columns are selected from the inner query, we need to add dummy
    # references to the min and max user ids, in order to keep them in the aggregation input.
    # The user ids count column has the `user_id?` flag set, so it will be automatically selected,
    # as it will take place of user id column for the synthetic statistics query.
    min_uid_top_ref = column_from_table(inner_table, "__ac_min_uid")
    max_uid_top_ref = column_from_table(inner_table, "__ac_max_uid")

    %Query{
      query
      | from: {:subquery, %{ast: inner_query, alias: inner_table.name}},
        selected_tables: [inner_table],
        aggregators: [min_uid_top_ref, max_uid_top_ref | query.aggregators],
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

  defp column_from_table(table, name) do
    table.columns
    |> Enum.find(&(&1.name == name))
    |> Expression.column(table)
  end

  defp set_fields(expression, fields) do
    Enum.reduce(fields, expression, fn {name, value}, acc ->
      Map.put(acc, name, value)
    end)
  end

  defp update_aggregator(
         %Expression{function_args: [{:distinct, %Expression{user_id?: true, table: inner_table}}]} = aggregator
       ) do
    arg = inner_table |> column_from_table("__ac_count_duid") |> set_fields(user_id?: true, synthetic?: true)
    %Expression{aggregator | function_args: [{:distinct, arg}]}
  end

  defp update_aggregator(aggregator) do
    [%Expression{name: "__ac_agg_" <> _ = name, table: inner_table}] = aggregator.function_args

    args =
      for input <- ~w(sum min max stddev) do
        inner_table |> column_from_table("#{name}_#{input}") |> set_fields(synthetic?: true)
      end

    %Expression{aggregator | function_args: args}
  end

  defp uid_statistics(uid_grouping_table) do
    uid_column = column_from_table(uid_grouping_table, uid_grouping_table.user_id)
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
    |> Enum.map(&column_from_table(uid_grouping_table, &1.alias || &1.name))
  end
end
