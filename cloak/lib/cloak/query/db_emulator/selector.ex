defmodule Cloak.Query.DbEmulator.Selector do
  @moduledoc """
  Data retrieval for emulated queries.
  """

  alias Cloak.Sql.{Query, Condition, Expression}
  alias Cloak.Query.{Rows, Sorter}
  alias Cloak.{Data, Stats}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes a `SELECT` query over the input stream of rows."
  @spec select(Enumerable.t(), Query.t()) :: Enumerable.t()
  def select(stream, query) do
    columns = Query.bucket_columns(query)

    stream
    |> Rows.filter(query |> Query.emulated_where() |> Condition.to_function())
    |> select_columns(columns, query)
    |> Sorter.order_rows(columns, query.order_by)
    |> Stream.map(&take_fields(&1, length(query.columns)))
    |> offset_rows(query)
    |> limit_rows(query)
  end

  @doc "Joins two streams into one using the specified join type and conditions."
  @spec join(Enumerable.t(), Enumerable.t(), map) :: Enumerable.t()
  def join(lhs, rhs, %{type: :cross_join}), do: cross_join(lhs, rhs)
  def join(lhs, rhs, %{type: :inner_join} = join), do: inner_join(lhs, rhs, join)
  def join(lhs, rhs, %{type: :left_outer_join} = join), do: left_join(lhs, rhs, join)
  def join(lhs, rhs, %{type: :right_outer_join} = join), do: right_join(lhs, rhs, join)

  @doc "Keeps only the columns needed by the query from the selection target."
  @spec pick_db_columns(Enumerable.t(), Query.t()) :: Enumerable.t()
  def pick_db_columns(stream, query = %Query{from: {:subquery, _}}), do: do_pick_db_columns(stream, query)

  def pick_db_columns(stream, query = %Query{from: {:join, _}}), do: do_pick_db_columns(stream, query)

  def pick_db_columns(stream, _query), do: stream

  # -------------------------------------------------------------------
  # Implementation of pick_db_columns
  # -------------------------------------------------------------------

  defp do_pick_db_columns(stream, %Query{db_columns: db_columns, from: from}) do
    # The column titles in a subquery are not guaranteed to be unique, but that is fine
    # since, if they are not, they can't be referenced exactly either.
    columns =
      db_columns
      |> Enum.sort_by(& &1.row_index)
      |> Enum.map(&update_row_index(&1, from))

    Stream.map(stream, &select_fields(&1, columns))
  end

  defp update_row_index(column = %Expression{kind: :function, args: args}, source),
    do: %Expression{
      column
      | row_index: nil,
        args: Enum.map(args, &update_row_index(&1, source))
    }

  defp update_row_index(column = %Expression{kind: :constant}, _source), do: column

  defp update_row_index(column, from), do: %Expression{column | row_index: index_in_from(column, from)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp select_fields(row, columns) when is_list(row), do: Enum.map(columns, &Expression.value(&1, row))
  defp select_fields(bucket, columns) when is_map(bucket), do: %{bucket | row: select_fields(bucket.row, columns)}

  defp take_fields(row, count) when is_list(row), do: Enum.take(row, count)
  defp take_fields(bucket, count) when is_map(bucket), do: %{bucket | row: take_fields(bucket.row, count)}

  defp bucket_data(row) when is_list(row), do: {row, nil}
  defp bucket_data(bucket) when is_map(bucket), do: {bucket.row, bucket.unreliable}

  defp merge_unreliable_flags(nil, flag), do: flag
  defp merge_unreliable_flags(flag, nil), do: flag
  defp merge_unreliable_flags(flag1, flag2) when is_boolean(flag1) and is_boolean(flag2), do: flag1 or flag2

  defp select_columns(stream, columns, %Query{implicit_count?: true}),
    do: Stream.map(stream, &select_fields(&1, columns))

  defp select_columns(stream, columns, query) do
    defaults = Enum.map(query.aggregators, &aggregator_to_default/1)
    accumulators = Enum.map(query.aggregators, &aggregator_to_accumulator/1)
    finalizers = Enum.map(query.aggregators, &aggregator_to_finalizer/1)

    results =
      stream
      |> Rows.group(
        query,
        fn _grouping_set_index -> [nil | defaults] end,
        fn _grouping_set_index, [aggregated_unreliability | aggregated_values], row_or_bucket ->
          {row, unreliable} = bucket_data(row_or_bucket)
          aggregated_unreliability = merge_unreliable_flags(aggregated_unreliability, unreliable)

          aggregated_values =
            aggregated_values
            |> Enum.zip(accumulators)
            |> Enum.map(fn {value, accumulator} -> accumulator.(row, value) end)

          [aggregated_unreliability | aggregated_values]
        end
      )
      |> Stream.map(fn {group_values, [aggregated_unreliability | aggregated_values]} ->
        aggregated_values =
          aggregated_values
          |> Enum.zip(finalizers)
          |> Enum.map(fn {value, finalizer} -> finalizer.(value) end)

        if aggregated_unreliability == nil,
          do: group_values ++ aggregated_values,
          else: %{row: group_values ++ aggregated_values, unreliable: aggregated_unreliability, occurrences: 1}
      end)
      |> Rows.extract_groups(columns, query)

    case {results, Rows.group_expressions(query)} do
      {[], []} -> [defaults]
      _ -> results
    end
  end

  defp offset_rows(stream, %Query{offset: 0}), do: stream
  defp offset_rows(stream, %Query{offset: offset}), do: Stream.drop(stream, offset)

  defp limit_rows(stream, %Query{limit: nil}), do: stream
  defp limit_rows(stream, %Query{limit: limit}), do: Stream.take(stream, limit)

  defp aggregator_to_default(%Expression{kind: :function, args: [{:distinct, _column}]}), do: MapSet.new()

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "count",
         args: [_column]
       }),
       do: 0

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "min",
         args: [_column]
       }),
       do: nil

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "max",
         args: [_column]
       }),
       do: nil

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "sum",
         args: [_column]
       }),
       do: nil

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "avg",
         args: [_column]
       }),
       do: {nil, 0}

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "stddev",
         args: [_column]
       }),
       do: []

  defp aggregator_to_default(%Expression{
         kind: :function,
         name: "variance",
         args: [_column]
       }),
       do: []

  defmacrop null_ignore_accumulator(do: expression) do
    quote do
      fn row, var!(accumulator) ->
        case Expression.value(var!(column), row) do
          nil -> var!(accumulator)
          :* -> var!(accumulator)
          var!(value) -> unquote(expression)
        end
      end
    end
  end

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         args: [{:distinct, column}]
       }),
       do: null_ignore_accumulator(do: MapSet.put(accumulator, value))

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "count",
         args: [:*]
       }),
       do: fn _row, count -> count + 1 end

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "count",
         args: [column]
       }) do
    null_ignore_accumulator do
      _ = value
      accumulator + 1
    end
  end

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "sum",
         args: [column]
       }),
       do: null_ignore_accumulator(do: (accumulator || 0) + value)

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "min",
         args: [column]
       }),
       do: null_ignore_accumulator(do: Data.min(accumulator, value))

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "max",
         args: [column]
       }),
       do: null_ignore_accumulator(do: Data.max(accumulator, value))

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "avg",
         args: [column]
       }) do
    null_ignore_accumulator do
      {sum, count} = accumulator
      {(sum || 0) + value, count + 1}
    end
  end

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "stddev",
         args: [column]
       }),
       do: null_ignore_accumulator(do: [value | accumulator])

  defp aggregator_to_accumulator(%Expression{
         kind: :function,
         name: "variance",
         args: [column]
       }),
       do: null_ignore_accumulator(do: [value | accumulator])

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "count",
         args: [{:distinct, _column}]
       }),
       do: &MapSet.size(&1)

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "count",
         args: [_column]
       }),
       do: & &1

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "sum",
         args: [_column]
       }),
       do: & &1

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "min",
         args: [_column]
       }),
       do: & &1

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "max",
         args: [_column]
       }),
       do: & &1

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "avg",
         args: [_column]
       }),
       do: fn
         {nil, 0} -> nil
         {sum, count} -> sum / count
       end

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "stddev",
         args: [_column]
       }),
       do: &Stats.stddev/1

  defp aggregator_to_finalizer(%Expression{
         kind: :function,
         name: "variance",
         args: [_column]
       }),
       do: &Stats.variance/1

  defp joined_row_size({:subquery, subquery}), do: Enum.count(subquery.ast.columns)
  defp joined_row_size({:join, join}), do: joined_row_size(join.lhs) + joined_row_size(join.rhs)

  defp join_rows_or_buckets(row1, row2) when is_list(row1) and is_list(row2), do: row1 ++ row2

  defp join_rows_or_buckets(row1, bucket2) when is_list(row1) and is_map(bucket2),
    do: %{bucket2 | row: row1 ++ bucket2.row}

  defp join_rows_or_buckets(bucket1, row2) when is_map(bucket1) and is_list(row2),
    do: %{bucket1 | row: bucket1.row ++ row2}

  defp join_rows_or_buckets(bucket1, bucket2) when is_map(bucket1) and is_map(bucket2) do
    unreliable = merge_unreliable_flags(bucket1.unreliable, bucket2.unreliable)
    %{row: bucket1.row ++ bucket2.row, unreliable: unreliable, occurrences: 1}
  end

  defp add_prefix_to_rows(stream, row), do: Stream.map(stream, &join_rows_or_buckets(row, &1))

  defp add_suffix_to_rows(stream, row), do: Stream.map(stream, &join_rows_or_buckets(&1, row))

  defp cross_join(lhs, rhs), do: Stream.flat_map(lhs, &add_prefix_to_rows(rhs, &1))

  defp inner_join(lhs, rhs, join) do
    rhs_pre_filter = create_join_pre_filter(rhs, join)
    filter = Condition.to_function(join.condition)

    Stream.flat_map(lhs, fn lhs_row ->
      lhs_row
      |> rhs_pre_filter.()
      |> add_prefix_to_rows(lhs_row)
      |> Rows.filter(filter)
    end)
  end

  defp left_join(lhs, rhs, join) do
    rhs_null_row = List.duplicate(nil, joined_row_size(join.rhs))
    outer_join(lhs, rhs, join, &add_prefix_to_rows/2, &[join_rows_or_buckets(&1, rhs_null_row)], & &1)
  end

  defp right_join(lhs, rhs, join) do
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    outer_join(rhs, lhs, join, &add_suffix_to_rows/2, &[join_rows_or_buckets(lhs_null_row, &1)], & &1)
  end

  defp outer_join(lhs, rhs, join, rows_combiner, unmatched_handler, matched_handler) do
    rhs_pre_filter = create_join_pre_filter(rhs, join)
    filter = Condition.to_function(join.condition)

    Stream.flat_map(lhs, fn lhs_row ->
      lhs_row
      |> rhs_pre_filter.()
      |> rows_combiner.(lhs_row)
      |> Rows.filter(filter)
      |> Enum.to_list()
      |> case do
        [] -> unmatched_handler.(lhs_row)
        joined_rows -> matched_handler.(joined_rows)
      end
    end)
  end

  # This function returns a functor that pre-filters right side rows in order to drastically improve join performance.
  # It does that by grouping rows by one of the matching columns in the join conditions, if one exists.
  defp create_join_pre_filter(rhs_rows, join) do
    case best_condition_for_matching(join) do
      nil ->
        fn _ -> rhs_rows end

      matched_fields ->
        {lhs, rhs} = extract_matching_columns(join, matched_fields)
        lhs_match_index = index_in_from(lhs, join.lhs)
        rhs_match_index = index_in_from(rhs, join.rhs)

        rhs_rows_map = Enum.group_by(rhs_rows, &(&1 |> Rows.fields() |> Enum.at(rhs_match_index)))

        fn lhs_row ->
          lhs_match_value = lhs_row |> Rows.fields() |> Enum.at(lhs_match_index)
          Map.get(rhs_rows_map, lhs_match_value, [])
        end
    end
  end

  # Make sure we return the columns in the correct order ({left_branch, right_branch}).
  defp extract_matching_columns(join, {subject, target}) do
    if table_is_in_join_branch?(subject.table.name, join.lhs) do
      true = table_is_in_join_branch?(target.table.name, join.rhs)
      {subject, target}
    else
      true = table_is_in_join_branch?(target.table.name, join.lhs)
      true = table_is_in_join_branch?(subject.table.name, join.rhs)
      {target, subject}
    end
  end

  defp best_condition_for_matching(join) do
    conditions = Lens.to_list(Query.Lenses.conditions(), join.condition)

    for %Expression{kind: :function, name: "=", args: [subject, target]} <- conditions,
        subject != target and subject.name != nil and target.name != nil do
      {subject, target}
    end
    |> case do
      [] -> nil
      match_conditions -> Enum.max_by(match_conditions, &prefer_user_id/1)
    end
  end

  defp prefer_user_id({subject, target}), do: user_id_score(subject) + user_id_score(target)

  defp user_id_score(%Expression{user_id?: true}), do: 1
  defp user_id_score(_), do: 0

  defp table_is_in_join_branch?(table_name, {:join, join}),
    do: table_is_in_join_branch?(table_name, join.lhs) or table_is_in_join_branch?(table_name, join.rhs)

  defp table_is_in_join_branch?(table_name, {:subquery, %{alias: subquery_alias}}), do: table_name == subquery_alias

  defp table_is_in_join_branch?(table_name, joined_table), do: table_name == joined_table

  defp insensitive_equal?(nil, _), do: false
  defp insensitive_equal?(_, nil), do: false
  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  # -------------------------------------------------------------------
  # Indexing helpers
  # -------------------------------------------------------------------

  defp index_in_from(column, {:subquery, %{ast: source_subquery}}),
    do:
      source_subquery.column_titles
      |> Enum.find_index(&insensitive_equal?(&1, column.name))
      |> check_index(column, source_subquery.column_titles)

  defp index_in_from(column, {:join, join}),
    do: join.columns |> join_column_index(column) |> check_index(column, join.columns)

  defp join_column_index(columns, column),
    do:
      Enum.find_index(columns, &(Expression.id(column) == Expression.id(&1))) ||
        Enum.find_index(columns, &insensitive_equal?(Expression.id(column), &1.name))

  defp check_index(nil, column, targets),
    do:
      raise(
        "Column index for column #{inspect(%{column | table: nil}, pretty: true)} could not be found in the " <>
          "list of available options: #{inspect(targets, pretty: true)}"
      )

  defp check_index(index, _column, _targets), do: index
end
