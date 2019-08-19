defmodule Cloak.Sql.Compiler.Anonymization do
  @moduledoc """
    This module inspects the AST and detects the type for each individual subquery, which determines the required
    validations in later steps. It also prepares the anonymized subqueries for the anonymized aggregation pipeline.
  """

  alias Cloak.Sql.{Query, Expression, Query.Lenses, Function}
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

  defp compile_anonymization(%Query{command: :select, type: :anonymized} = query) do
    cond do
      supports_statistics_anonymization?(query) -> convert_to_statistics_anonymization(query)
      needs_uid_grouping?(query) -> group_by_uid(query)
      true -> query
    end
  end

  defp compile_anonymization(query), do: query

  defp set_fields(expression, fields) do
    Enum.reduce(fields, expression, fn {name, value}, acc ->
      Map.put(acc, name, value)
    end)
  end

  defp update_base_column(%Expression{user_id?: true, synthetic?: synthetic?}, inner_table, _base_columns),
    do: inner_table |> Helpers.column_from_table(inner_table.user_id) |> set_fields(synthetic?: synthetic?)

  defp update_base_column(column, inner_table, base_columns) do
    base_column = Enum.find(base_columns, &Expression.equals?(&1, column))
    inner_table |> Helpers.column_from_table(base_column.alias) |> set_fields(synthetic?: column.synthetic?)
  end

  defp update_base_columns(query, base_columns, inner_table) do
    query
    |> update_in(
      [
        Lenses.query_expressions()
        |> Lens.filter(&Expression.member?(base_columns, &1))
        |> Lens.reject(&(&1.table == inner_table))
      ],
      &update_base_column(&1, inner_table, base_columns)
    )
  end

  defp grouped_columns(%Query{group_by: []} = query),
    do: query |> Helpers.aggregator_sources() |> Enum.flat_map(&extract_groups/1)

  defp grouped_columns(%Query{group_by: group_by}), do: Enum.reject(group_by, &Expression.constant?/1)

  defp required_groups(query),
    do: query |> grouped_columns() |> Enum.reject(& &1.user_id?) |> Enum.uniq_by(&Expression.semantic/1)

  defp extract_groups(%Expression{name: name} = column) when is_binary(name), do: [column]

  defp extract_groups(%Expression{function?: true, aggregate?: false} = expression) do
    if Helpers.aggregated_column?(expression) or uses_multiple_columns?(expression),
      do: Enum.flat_map(expression.function_args, &extract_groups/1),
      else: [expression]
  end

  defp extract_groups(_), do: []

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

  defp aggregator_supports_statistics?(%Expression{function: function, type: type})
       when function in ["min", "max"] and type in [:date, :time, :datetime],
       do: false

  defp aggregator_supports_statistics?(aggregator),
    do: distinct_column_count?(aggregator) or can_be_uid_grouped?(aggregator)

  defp distinct_column_count?(%Expression{
         function: function,
         function_args: [{:distinct, %Expression{user_id?: false}}]
       })
       when function in ["count", "count_noise"],
       do: true

  defp distinct_column_count?(_), do: false

  defp statistics_anonymization_enabled?(data_source), do: data_source[:statistics_anonymization] != false

  defp offload_grouping_sets(query) do
    {:subquery, uid_grouping_subquery} = query.from
    [uid_grouping_table] = query.selected_tables

    uid_grouping_subquery_ast = uid_grouping_subquery.ast |> push_grouping_sets(query) |> select_grouping_id_column()
    uid_grouping_subquery = %{uid_grouping_subquery | ast: uid_grouping_subquery_ast}

    grouping_id_column = Table.column("__ac_grouping_id", :integer)
    uid_grouping_table = %{uid_grouping_table | columns: [grouping_id_column | uid_grouping_table.columns]}

    %Query{query | selected_tables: [uid_grouping_table], from: {:subquery, uid_grouping_subquery}}
  end

  defp select_column_in_query(query, column) do
    %Query{
      query
      | columns: query.columns ++ [column],
        column_titles: query.column_titles ++ [Expression.title(column)]
    }
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

  defp grouping_id_column(grouping_sets, group_by) do
    if length(grouping_sets) > 1 do
      Expression.function("grouping_id", Enum.reject(group_by, & &1.user_id?), :integer)
    else
      Expression.constant(:integer, 0)
    end
    |> set_fields(alias: "__ac_grouping_id", synthetic?: true)
  end

  defp select_grouping_id_column(query),
    do: select_column_in_query(query, grouping_id_column(query.grouping_sets, query.group_by))

  defp push_grouping_sets(inner_query, %Query{grouping_sets: grouping_sets, group_by: group_by})
       when length(grouping_sets) > 1 do
    grouping_sets =
      grouping_sets
      |> translate_grouping_sets(group_by, inner_query)
      |> add_user_id_to_grouping_sets(inner_query.group_by)

    %Query{inner_query | grouping_sets: grouping_sets}
  end

  defp push_grouping_sets(inner_query, _top_query), do: inner_query

  defp columns_equal_or_null_condition(
         %Expression{name: "__ac_grouping_id"} = column1,
         %Expression{name: "__ac_grouping_id"} = column2
       ),
       do: {:comparison, column1, :=, column2}

  defp columns_equal_or_null_condition(column1, column2) do
    columns_equal = {:comparison, column1, :=, column2}
    columns_null = {:and, {:is, column1, :null}, {:is, column2, :null}}
    {:or, columns_equal, columns_null}
  end

  defp groups_equal_or_null_conditions(group1, group2) do
    Enum.zip(group1, group2)
    |> Enum.map(fn {column1, column2} -> columns_equal_or_null_condition(column1, column2) end)
    |> Enum.reduce(fn condition, accumulator -> {:and, accumulator, condition} end)
  end

  defp convert_to_statistics_anonymization(query) do
    # Split the aggregators requiring distinct statistics from those requiring regular statistics.
    {distinct_aggregators, regular_aggregators} = Enum.split_with(query.aggregators, &distinct_column_count?/1)

    uid_grouped_query = %Query{query | aggregators: regular_aggregators} |> group_by_uid() |> offload_grouping_sets()

    regular_statistics_query = compute_main_statistics(uid_grouped_query)
    regular_statistics_table = Helpers.create_table_from_columns(regular_statistics_query.columns, "__ac_regular_stats")
    regular_groups = aggregation_groups(regular_statistics_query, regular_statistics_table)

    distinct_columns = target_columns_for_distinct_aggregators(distinct_aggregators)

    from = {:subquery, %{ast: regular_statistics_query, alias: regular_statistics_table.name}}

    {from, selected_tables} =
      distinct_columns
      |> Enum.with_index()
      |> Enum.reduce({from, [regular_statistics_table]}, fn {distinct_column, index}, {from, selected_tables} ->
        distinct_statistics_query = compute_distinct_statistics(query, distinct_column)

        distinct_statistics_table =
          Helpers.create_table_from_columns(distinct_statistics_query.columns, "__ac_distinct_stats#{index}")

        conditions =
          distinct_statistics_query
          |> aggregation_groups(distinct_statistics_table)
          |> groups_equal_or_null_conditions(regular_groups)

        from_rhs = {:subquery, %{ast: distinct_statistics_query, alias: distinct_statistics_table.name}}

        # We join each query for distinct statistics to the main query with regular statistics.
        from = {:join, %{type: :inner_join, lhs: from, rhs: from_rhs, conditions: conditions}}

        {from, selected_tables ++ [distinct_statistics_table]}
      end)

    # Re-assemble the top-level aggregators.
    aggregators = uid_grouped_query.aggregators ++ distinct_aggregators

    # Since only referenced columns are selected from the inner query, we need to add dummy
    # references to the min/max user ids and grouping id, in order to keep them in the aggregation input.
    # The user ids count column has the `user_id?` flag set, so it will be automatically selected,
    # as it will take place of user id column for the synthetic statistics query.
    min_uid_top_ref = column_from_synthetic_table(regular_statistics_table, "__ac_min_uid")
    max_uid_top_ref = column_from_synthetic_table(regular_statistics_table, "__ac_max_uid")
    grouping_id_top_ref = column_from_synthetic_table(regular_statistics_table, "__ac_grouping_id")

    %Query{
      uid_grouped_query
      | from: from,
        selected_tables: selected_tables,
        aggregators: [grouping_id_top_ref, min_uid_top_ref, max_uid_top_ref | aggregators],
        anonymization_type: :statistics,
        where: nil
    }
    |> update_in(
      [Lenses.query_expressions() |> Lens.filter(& &1.aggregate?)],
      &update_stats_aggregator(&1, selected_tables, distinct_columns)
    )
    |> update_in(
      [Lenses.query_expressions() |> Lens.filter(&is_binary(&1.name)) |> Lens.reject(&(&1.table in selected_tables))],
      &set_fields(&1, table: regular_statistics_table)
    )
    |> Query.add_debug_info("Using statistics-based anonymization.")
  end

  defp compute_main_statistics(query) do
    {:subquery, %{ast: uid_grouping_query}} = query.from
    [uid_grouping_table] = query.selected_tables

    grouping_id = column_from_synthetic_table(uid_grouping_table, "__ac_grouping_id")
    groups = [grouping_id | aggregation_groups(uid_grouping_query, uid_grouping_table)]

    {count_duid, min_uid, max_uid} = uid_statistics(uid_grouping_table)
    aggregators = [count_duid, min_uid, max_uid | aggregation_statistics(query.aggregators)]

    inner_columns = Enum.uniq(groups ++ aggregators)

    %Query{
      query
      | subquery?: true,
        type: :restricted,
        aggregators: aggregators,
        columns: inner_columns,
        column_titles: Enum.map(inner_columns, &Expression.title(&1)),
        group_by: Enum.map(groups, &Expression.unalias/1),
        grouping_sets: Helpers.default_grouping_sets(groups),
        order_by: [],
        having: statistics_buckets_filter(query, count_duid),
        limit: nil,
        offset: 0,
        sample_rate: nil,
        distinct?: false,
        implicit_count?: false
    }
  end

  defp column_from_synthetic_table(table, name) do
    table
    |> Helpers.column_from_table(name)
    |> set_fields(synthetic?: true)
  end

  defp update_stats_aggregator(
         %Expression{function_args: [{:distinct, %Expression{user_id?: true}}]} = aggregator,
         [regular_statistics_table | _distinct_statistics_tables],
         _distinct_columns
       ) do
    arg = column_from_synthetic_table(regular_statistics_table, "__ac_count_duid")
    %Expression{aggregator | function_args: [{:distinct, arg}]}
  end

  defp update_stats_aggregator(
         %Expression{function_args: [{:distinct, arg}]} = aggregator,
         [_regular_statistics_table | distinct_statistics_tables],
         distinct_columns
       ) do
    table_index = Enum.find_index(distinct_columns, &Expression.equals?(&1, arg))
    distinct_statistics_table = Enum.at(distinct_statistics_tables, table_index)
    real_count = column_from_synthetic_table(distinct_statistics_table, "__ac_count_distinct")
    noise_factor = column_from_synthetic_table(distinct_statistics_table, "__ac_noise_factor")
    %Expression{aggregator | function_args: [{:distinct, [real_count, noise_factor]}]}
  end

  defp update_stats_aggregator(aggregator, [regular_statistics_table | _distinct_statistics_tables], _distinct_columns) do
    [%Expression{name: "__ac_agg_" <> _ = name}] = aggregator.function_args

    args =
      for input <- ~w(count sum min max stddev),
          do: column_from_synthetic_table(regular_statistics_table, "#{name}_#{input}")

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

  # -------------------------------------------------------------------
  # UID grouping
  # -------------------------------------------------------------------

  defp needs_uid_grouping?(query), do: Enum.all?(query.aggregators, &can_be_uid_grouped?/1)

  @uid_offloaded_aggregators ~w(count sum min max count_noise sum_noise)
  defp can_be_uid_grouped?(aggregator),
    do: aggregator.function in @uid_offloaded_aggregators and not distinct_input?(aggregator.function_args)

  defp distinct_input?([{:distinct, %Expression{user_id?: false}}]), do: true
  defp distinct_input?([_]), do: false

  defp group_by_uid(query) do
    user_id = query |> Helpers.id_column() |> set_fields(synthetic?: true)

    base_columns =
      query
      |> required_groups()
      |> Enum.with_index()
      |> Enum.map(fn {column, index} ->
        set_fields(column, alias: "__ac_group_#{index}", synthetic?: true)
      end)

    aggregated_columns =
      query.aggregators
      |> Enum.reject(&match?(%Expression{function_args: [{:distinct, %Expression{user_id?: true}}]}, &1))
      |> Enum.map(&uid_aggregator/1)
      |> Enum.uniq_by(&Expression.semantic/1)
      |> Enum.with_index()
      |> Enum.map(fn {expression, index} ->
        set_fields(expression, alias: "__ac_agg_#{index}", synthetic?: true)
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
        grouping_sets: Helpers.default_grouping_sets(grouped_columns),
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
      &update_uid_aggregator(&1, inner_table, aggregated_columns)
    )
    |> update_base_columns(grouped_columns, inner_table)
  end

  defp uid_aggregator(%Expression{function: "count_noise"} = expression),
    do: uid_aggregator(%Expression{expression | function: "count", type: :integer})

  defp uid_aggregator(%Expression{function: "sum_noise", function_args: [arg]} = expression),
    do: uid_aggregator(%Expression{expression | function: "sum", type: Function.type(arg)})

  defp uid_aggregator(%Expression{function_args: [{:distinct, %Expression{user_id?: true} = user_id}]}),
    do: user_id

  defp uid_aggregator(aggregator), do: aggregator

  defp update_uid_aggregator(
         %Expression{function_args: [{:distinct, %Expression{user_id?: true}}]} = aggregator,
         _inner_table,
         _aggregated_columns
       ),
       do: aggregator

  defp update_uid_aggregator(old_aggregator, inner_table, aggregated_columns) do
    uid_aggregator = uid_aggregator(old_aggregator)

    aggregated_columns
    |> Enum.find_index(&Expression.equals?(&1, uid_aggregator))
    |> case do
      nil ->
        old_aggregator

      index ->
        column_name = "__ac_agg_#{index}"
        inner_column = Helpers.column_from_table(inner_table, column_name)
        function_name = global_aggregator(old_aggregator.function)

        function_name
        |> Expression.function([inner_column], old_aggregator.type, true)
        |> set_fields(alias: old_aggregator.function)
    end
  end

  defp global_aggregator("count"), do: "sum"
  defp global_aggregator("count_noise"), do: "sum_noise"
  defp global_aggregator(function_name), do: function_name

  defp uses_multiple_columns?(expression) do
    Lenses.leaf_expressions()
    |> Lens.filter(&(&1.name != nil))
    |> Lens.to_list(expression)
    |> Enum.count() > 1
  end

  # -------------------------------------------------------------------
  # Distinct statistics
  # -------------------------------------------------------------------

  defp add_column_index_to_grouping_sets(grouping_sets, column_index),
    do: Enum.map(grouping_sets, &[column_index | &1])

  defp target_columns_for_distinct_aggregators(distinct_aggregators) do
    distinct_aggregators
    |> Enum.map(fn %Expression{function_args: [{:distinct, target_column}]} -> target_column end)
    |> Enum.uniq()
  end

  defp compute_distinct_statistics(query, target_column) do
    base_columns =
      query
      |> required_groups()
      |> Enum.with_index()
      |> Enum.map(fn {column, index} ->
        set_fields(column, alias: "__ac_group_#{index}", synthetic?: true)
      end)

    target_column = set_fields(target_column, alias: "__ac_target", synthetic?: true)

    user_id = Helpers.id_column(query)
    min_user_id = Expression.function("min", [user_id], user_id.type, true)
    count_distinct_user_id = Expression.function("count", [{:distinct, user_id}], :integer, true)

    constant_lower = Expression.constant(:text, "<")
    constant_3 = Expression.constant(:integer, 3)
    low_count_user_id? = Expression.function("bool_op", [constant_lower, count_distinct_user_id, constant_3], :boolean)

    # The user id is valid only for at-risk values in the target column.
    user_id_aggregator =
      Expression.function("case", [low_count_user_id?, min_user_id, Expression.null()], user_id.type)
      |> set_fields(alias: "__ac_user_id", synthetic?: true)

    grouped_columns = base_columns ++ [target_column]
    grouping_id = grouping_id_column(query.grouping_sets, base_columns)

    # If we have multiple grouping sets, add the target column to each grouping set, otherwise, use the default ones.
    grouping_sets =
      if length(query.grouping_sets) > 1,
        do: add_column_index_to_grouping_sets(query.grouping_sets, Enum.count(base_columns)),
        else: Helpers.default_grouping_sets(grouped_columns)

    aggregated_columns = [user_id_aggregator]
    inner_columns = [grouping_id | grouped_columns ++ aggregated_columns]

    distinct_values_query = %Query{
      query
      | subquery?: true,
        type: :restricted,
        aggregators: aggregated_columns,
        columns: inner_columns,
        column_titles: Enum.map(inner_columns, &Expression.title/1),
        group_by: Enum.map(grouped_columns, &Expression.unalias/1),
        grouping_sets: grouping_sets,
        order_by: [],
        having: nil,
        limit: nil,
        offset: 0,
        sample_rate: nil,
        distinct?: false,
        implicit_count?: false
    }

    distinct_values_table = Helpers.create_table_from_columns(distinct_values_query.columns, "__ac_distinct_values")

    user_id = column_from_synthetic_table(distinct_values_table, "__ac_user_id")
    grouping_id = column_from_synthetic_table(distinct_values_table, "__ac_grouping_id")
    target_column = column_from_synthetic_table(distinct_values_table, "__ac_target")

    count_distinct_values =
      Expression.function("count", [target_column], :integer, true)
      |> set_fields(alias: "__ac_count_distinct", synthetic?: true)

    grouped_columns = [grouping_id, user_id | base_columns]
    aggregated_columns = [count_distinct_values]
    inner_columns = grouped_columns ++ aggregated_columns

    uid_grouping_query =
      %Query{
        distinct_values_query
        | from: {:subquery, %{ast: distinct_values_query, alias: distinct_values_table.name}},
          selected_tables: [distinct_values_table],
          aggregators: aggregated_columns,
          columns: inner_columns,
          column_titles: Enum.map(inner_columns, &Expression.title/1),
          group_by: Enum.map(grouped_columns, &Expression.unalias/1),
          grouping_sets: Helpers.default_grouping_sets(grouped_columns),
          where: nil
      }
      |> update_base_columns(grouped_columns, distinct_values_table)

    uid_grouping_table = Helpers.create_table_from_columns(uid_grouping_query.columns, "__ac_uid_grouping")

    user_id = column_from_synthetic_table(uid_grouping_table, "__ac_user_id")
    grouping_id = column_from_synthetic_table(uid_grouping_table, "__ac_grouping_id")
    count_distinct = column_from_synthetic_table(uid_grouping_table, "__ac_count_distinct")

    constant_different = Expression.constant(:text, "<>")
    low_count_user_id? = Expression.function("bool_op", [constant_different, user_id, Expression.null()], :boolean)
    low_count_user_id_as_integer = Expression.function({:cast, :integer}, [low_count_user_id?], :integer)
    noise_factor = Expression.function("unsafe_mul", [low_count_user_id_as_integer, count_distinct], :integer)

    max_noise_factor =
      Expression.function("max", [noise_factor], :integer, true)
      |> set_fields(alias: "__ac_noise_factor", synthetic?: true)

    global_count_distinct =
      Expression.function("sum", [count_distinct], :integer, true)
      |> set_fields(alias: "__ac_count_distinct", synthetic?: true)

    grouped_columns = [grouping_id | base_columns]
    aggregated_columns = [max_noise_factor, global_count_distinct]
    inner_columns = grouped_columns ++ aggregated_columns

    %Query{
      uid_grouping_query
      | from: {:subquery, %{ast: uid_grouping_query, alias: uid_grouping_table.name}},
        selected_tables: [uid_grouping_table],
        aggregators: aggregated_columns,
        columns: inner_columns,
        column_titles: Enum.map(inner_columns, &Expression.title/1),
        group_by: Enum.map(grouped_columns, &Expression.unalias/1),
        grouping_sets: Helpers.default_grouping_sets(grouped_columns),
        where: nil
    }
    |> update_base_columns(grouped_columns, uid_grouping_table)
  end
end
