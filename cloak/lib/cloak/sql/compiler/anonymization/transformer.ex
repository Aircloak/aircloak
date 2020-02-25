defmodule Cloak.Sql.Compiler.Anonymization.Transformer do
  @moduledoc "Helper module for rewriting queries during the Compiler.Anonymization stage."

  alias Cloak.Sql.{Query, Expression, Query.Lenses, Function}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.DataSource.Table

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Groups data in an anonymizing query by user id."
  @spec group_by_uid(Query.t()) :: Query.t()
  def group_by_uid(query) do
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
      |> Enum.reject(&match?(%Expression{args: [{:distinct, _}]}, &1))
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
    |> update_in([aggregators_lens()], &update_uid_aggregator(&1, inner_table, aggregated_columns))
    |> update_base_columns(grouped_columns, inner_table)
  end

  @doc "Creates a query that computes the main statistics for anonymization."
  @spec compute_main_statistics(Query.t()) :: Query.t()
  def compute_main_statistics(query) do
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
        distinct?: false,
        implicit_count?: false
    }
  end

  @doc "Creats a query that computes the statistics required for anonymizing distinct counts."
  @spec compute_distinct_statistics(Query.t(), Expression.t()) :: Query.t()
  def compute_distinct_statistics(query, target_column) do
    base_columns =
      query
      |> required_groups()
      |> Enum.with_index()
      |> Enum.map(fn {column, index} ->
        set_fields(column, alias: "__ac_group_#{index}", synthetic?: true)
      end)

    target_column = set_fields(target_column, alias: "__ac_target", synthetic?: true)

    user_id = Helpers.id_column(query)
    min_user_id = Expression.function("min", [user_id], user_id.type)
    count_distinct_user_id = Expression.function("count", [{:distinct, user_id}], :integer)

    low_count_user_id? = Expression.function("<", [count_distinct_user_id, Expression.constant(:integer, 3)], :boolean)

    # The user id is valid only for at-risk values in the target column.
    user_id_aggregator =
      Expression.function("case", [low_count_user_id?, min_user_id, Expression.null()], user_id.type)
      |> set_fields(alias: "__ac_user_id", synthetic?: true, user_id?: true)

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
        distinct?: false,
        implicit_count?: false
    }

    distinct_values_table = Helpers.create_table_from_columns(distinct_values_query.columns, "__ac_distinct_values")

    user_id = column_from_synthetic_table(distinct_values_table, "__ac_user_id")
    grouping_id = column_from_synthetic_table(distinct_values_table, "__ac_grouping_id")
    target_column = column_from_synthetic_table(distinct_values_table, "__ac_target")

    count_distinct_values =
      Expression.function("count", [target_column], :integer)
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

    low_count_user_id? = Expression.function("not", [Expression.function("is_null", [user_id], :boolean)], :boolean)
    low_count_user_id_as_integer = Expression.function({:cast, :integer}, [low_count_user_id?], :integer)
    noise_factor = Expression.function("unsafe_mul", [low_count_user_id_as_integer, count_distinct], :integer)

    max_noise_factor =
      Expression.function("max", [noise_factor], :integer)
      |> set_fields(alias: "__ac_noise_factor", synthetic?: true)

    global_count_distinct =
      Expression.function("sum", [count_distinct], :integer)
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

  @doc "Offloads grouping sets from an anonymizing query into the subquery that groups by user id."
  @spec offload_grouping_sets(Query.t()) :: Query.t()
  def offload_grouping_sets(query) do
    {:subquery, uid_grouping_subquery} = query.from
    [uid_grouping_table] = query.selected_tables

    uid_grouping_subquery_ast = uid_grouping_subquery.ast |> push_grouping_sets(query) |> select_grouping_id_column()
    uid_grouping_subquery = %{uid_grouping_subquery | ast: uid_grouping_subquery_ast}

    grouping_id_column = Table.column("__ac_grouping_id", :integer)
    uid_grouping_table = %{uid_grouping_table | columns: [grouping_id_column | uid_grouping_table.columns]}

    %Query{query | selected_tables: [uid_grouping_table], from: {:subquery, uid_grouping_subquery}}
  end

  @doc "Returns a lens for focusing on the aggregators in a query."
  @spec aggregators_lens() :: Lens.t()
  def aggregators_lens(), do: Lenses.query_expressions() |> Lens.filter(&Function.aggregator?/1)

  @doc "Returns a column expression from a synthetic table."
  @spec column_from_synthetic_table(Table.t(), String.t()) :: Expression.t()
  def column_from_synthetic_table(table, name) do
    table
    |> Helpers.column_from_table(name)
    |> set_fields(synthetic?: true)
  end

  @doc "Returns the aggregation groups for a query."
  @spec aggregation_groups(Query.t(), Table.t()) :: [Expression.t()]
  def aggregation_groups(uid_grouping_query, uid_grouping_table) do
    # It would be more efficient (and simpler) to group the statistics query by the final grouping expressions.
    # But that is not possible to do because noise layers need to access the raw columns used in the various clauses.
    uid_grouping_query.columns
    |> Enum.take(Enum.count(uid_grouping_query.group_by))
    |> Enum.reject(& &1.user_id?)
    |> Enum.map(&column_from_synthetic_table(uid_grouping_table, Expression.title(&1)))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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
        Lenses.top_down_query_expressions()
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

  defp extract_groups(%Expression{kind: :column} = column), do: [column]

  defp extract_groups(%Expression{kind: :function} = expression) do
    cond do
      Function.aggregator?(expression) ->
        []

      Helpers.aggregated_column?(expression) ->
        Enum.flat_map(expression.args, &extract_groups/1)

      true ->
        [expression]
    end
  end

  defp extract_groups(_), do: []

  # -------------------------------------------------------------------
  # Statistics
  # -------------------------------------------------------------------

  defp add_column_index_to_grouping_sets(grouping_sets, column_index),
    do: Enum.map(grouping_sets, &[column_index | &1])

  defp uid_statistics(uid_grouping_table) do
    uid_column = column_from_synthetic_table(uid_grouping_table, uid_grouping_table.user_id)
    true = uid_column != nil

    count_duid =
      Expression.function("count", [Expression.column(uid_column, uid_grouping_table)], :integer)
      |> set_fields(alias: "__ac_count_duid", user_id?: true, synthetic?: true)

    min_uid =
      Expression.function("min", [Expression.column(uid_column, uid_grouping_table)], uid_column.type)
      |> set_fields(alias: "__ac_min_uid", synthetic?: true)

    max_uid =
      Expression.function("max", [Expression.column(uid_column, uid_grouping_table)], uid_column.type)
      |> set_fields(alias: "__ac_max_uid", synthetic?: true)

    {count_duid, min_uid, max_uid}
  end

  defp aggregation_statistics(aggregators) do
    aggregators
    |> Enum.map(fn %Expression{args: [arg]} -> arg end)
    |> Enum.flat_map(fn
      {:distinct, _} ->
        []

      column ->
        for {function, type} <- [
              {"count", :integer},
              {"sum", column.type},
              {"min", column.type},
              {"max", column.type},
              {"stddev", :real}
            ] do
          function
          |> Expression.function([column], type)
          |> set_fields(alias: "#{column.name}_#{function}", synthetic?: true)
        end
    end)
  end

  defp statistics_buckets_filter(query, count_duid) do
    if Query.lcf_buckets_aggregation_limit(query) == 0 do
      # This is an optimization in case we dont care about censored buckets at all.
      Expression.function(">", [count_duid, Expression.constant(:integer, 1)], :boolean)
    else
      nil
    end
  end

  # -------------------------------------------------------------------
  # UID grouping
  # -------------------------------------------------------------------

  defp uid_aggregator(%Expression{kind: :function, name: "count_noise"} = expression),
    do: %Expression{expression | name: "count", type: :integer}

  defp uid_aggregator(%Expression{kind: :function, name: "sum_noise", args: [arg]} = expression),
    do: %Expression{expression | name: "sum", type: Function.type(arg)}

  defp uid_aggregator(%Expression{kind: :function, name: "count", args: [%Expression{kind: kind} = arg]} = expression)
       when kind != :constant do
    arg_is_null = Expression.function("is_null", [arg], :boolean)
    constant_one = Expression.constant(:integer, 1)
    constant_null = Expression.constant(nil, nil)

    one_if_arg_is_not_null =
      Expression.function("case", [arg_is_null, constant_null, constant_one], :integer) |> set_fields(synthetic?: true)

    %Expression{expression | name: "sum", args: [one_if_arg_is_not_null]}
  end

  defp uid_aggregator(aggregator), do: aggregator

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
        function_name = global_aggregator(old_aggregator.name)

        function_name
        |> Expression.function([inner_column], old_aggregator.type)
        |> set_fields(alias: old_aggregator.name)
    end
  end

  defp global_aggregator("count"), do: "sum"
  defp global_aggregator("count_noise"), do: "sum_noise"
  defp global_aggregator(function_name), do: function_name

  # -------------------------------------------------------------------
  # Offload grouping sets
  # -------------------------------------------------------------------

  defp push_grouping_sets(inner_query, %Query{grouping_sets: grouping_sets, group_by: group_by})
       when length(grouping_sets) > 1 do
    grouping_sets =
      grouping_sets
      |> translate_grouping_sets(group_by, inner_query)
      |> add_user_id_to_grouping_sets(inner_query.group_by)

    %Query{inner_query | grouping_sets: grouping_sets}
  end

  defp push_grouping_sets(inner_query, _top_query), do: inner_query

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

  defp select_column_in_query(query, column) do
    %Query{
      query
      | columns: query.columns ++ [column],
        column_titles: query.column_titles ++ [Expression.title(column)]
    }
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
end
