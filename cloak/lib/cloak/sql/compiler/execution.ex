defmodule Cloak.Sql.Compiler.Execution do
  @moduledoc """
  Makes the compiled query specification ready for execution.

  This is strictly speaking a post-compilation step where we do some transformation
  to the query, in order to be able to safely execute it.
  """

  alias Cloak.Sql.{CompilationError, Condition, Expression, FixAlign, Function, Query}
  alias Cloak.Sql.Compiler.{Helpers, Optimizer}
  alias Cloak.Sql.Query.Lenses

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the query for execution."
  @spec prepare(Query.t()) :: Query.t()
  def prepare(%Query{command: :show} = query), do: query

  def prepare(%Query{command: :select} = query),
    do:
      query
      |> prepare_subqueries()
      |> censor_selected_uids()
      |> align_buckets()
      |> align_where()
      |> align_join_ranges()
      |> compile_sample_rate()
      |> reject_null_user_ids()
      |> compute_aggregators()
      |> expand_virtual_tables()

  # -------------------------------------------------------------------
  # UID handling
  # -------------------------------------------------------------------

  defp reject_null_user_ids(%Query{type: :anonymized} = query) do
    user_id = %Expression{Helpers.id_column(query) | synthetic?: true}

    %{
      query
      | where: Condition.combine(:and, {:not, {:is, user_id, :null}}, query.where)
    }
  end

  defp reject_null_user_ids(query), do: query

  defp censor_selected_uids(%Query{type: :anonymized} = query) do
    # In an anonymized query, we're replacing all selected expressions which depend on uid columns with the `:*`
    # constant. This allows us to reduce the amount of anonymized values, without compromising the privacy.
    # For example, consider the query `select uid, name from users`. Normally, this would return only `(*, *)`
    # rows. However, with this replacement, we can return names which are frequent enough, without revealing
    # any sensitive information. For example, we could return: `(*, Alice), (*, Bob), (*, *)`, which is
    # not possible without this replacement.
    Lens.key(:columns)
    |> Lens.all()
    |> Lens.filter(&non_aggregated_uid_expression?/1)
    |> Lens.map(query, &Expression.constant(&1.type, :*))
  end

  defp censor_selected_uids(query), do: query

  # returns true if the column contains an expression with non-aggregated user ids
  defp non_aggregated_uid_expression?(column),
    do:
      [column] |> get_in([Lenses.all_expressions()]) |> Enum.all?(&(not &1.aggregate?)) and
        [column] |> extract_columns() |> Enum.any?(& &1.user_id?)

  # -------------------------------------------------------------------
  # Bucket alignment
  # -------------------------------------------------------------------

  defp align_buckets(%Query{type: :standard} = query), do: query

  defp align_buckets(query) do
    {messages, query} = Lens.get_and_map(Lenses.buckets(), query, &align_bucket/1)
    Query.add_info(query, Enum.reject(messages, &is_nil/1))
  end

  defp align_bucket(column) do
    if Function.bucket_size(column) <= 0 do
      raise CompilationError,
        source_location: column.source_location,
        message: "Bucket size #{Function.bucket_size(column)} must be > 0"
    end

    aligned = Function.update_bucket_size(column, &FixAlign.align/1)

    if aligned == column do
      {nil, aligned}
    else
      {"Bucket size adjusted from #{Function.bucket_size(column)} to #{Function.bucket_size(aligned)}", aligned}
    end
  end

  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp prepare_subqueries(query) do
    {info, prepared_subquery} =
      Lens.get_and_map(Query.Lenses.direct_subqueries(), query, fn subquery ->
        ast = prepare_subquery(subquery.ast)
        {ast.info, %{subquery | ast: ast}}
      end)

    Query.add_info(prepared_subquery, Enum.concat(info))
  end

  defp prepare_subquery(parsed_subquery),
    do:
      parsed_subquery
      |> prepare()
      |> align_limit()
      |> align_offset()
      |> align_having()

  @minimum_subquery_limit 10
  defp align_limit(query = %{limit: limit, type: :restricted}) when limit != nil do
    aligned = limit |> FixAlign.align() |> round() |> max(@minimum_subquery_limit)

    if aligned != limit do
      %{query | limit: aligned}
      |> Query.add_info("Limit adjusted from #{limit} to #{aligned}")
    else
      query
    end
  end

  defp align_limit(query), do: query

  defp align_offset(query = %{limit: limit, offset: offset, type: :restricted}) when offset != 0 do
    aligned = round(offset / limit) * limit

    if aligned != offset do
      %{query | offset: aligned}
      |> Query.add_info("Offset adjusted from #{offset} to #{aligned}")
    else
      query
    end
  end

  defp align_offset(query), do: query

  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp expand_arguments(column) do
    (column |> Expression.arguments() |> Enum.flat_map(&expand_arguments/1)) ++ [column]
  end

  defp compute_aggregators(%Query{group_by: [_ | _]} = query),
    do: %Query{
      query
      | aggregators: Expression.unique_except(aggregators(query), &Expression.row_splitter?/1)
    }

  defp compute_aggregators(query) do
    case aggregators(query) do
      [] ->
        %Query{query | aggregators: [Expression.count_star()], implicit_count?: true}

      aggregators ->
        %Query{
          query
          | aggregators: Expression.unique_except(aggregators, &Expression.row_splitter?/1)
        }
    end
  end

  defp aggregators(query),
    do:
      (query.columns ++ having_columns(query) ++ Query.order_by_expressions(query))
      |> Enum.flat_map(&expand_arguments/1)
      |> Enum.filter(&match?(%Expression{function?: true, aggregate?: true}, &1))

  defp having_columns(query),
    do:
      Lenses.conditions()
      |> Lenses.operands()
      |> Lens.to_list(query.having)

  defp align_join_ranges(%Query{type: :standard} = query), do: query

  defp align_join_ranges(query),
    do:
      query
      |> Query.Lenses.join_condition_lenses()
      |> Enum.reduce(query, fn lens, query -> align_ranges(query, lens) end)

  defp align_having(%Query{type: :restricted} = query), do: align_ranges(query, Lens.key(:having))
  defp align_having(query), do: query

  defp align_where(%Query{type: :standard} = query), do: query
  defp align_where(query), do: align_ranges(query, Lens.key(:where))

  defp align_ranges(query, lens) do
    clause = Lens.one!(lens, query)
    grouped_inequalities = inequalities_by_column(clause)

    verify_ranges(grouped_inequalities)

    non_range_conditions = Condition.reject(clause, &Condition.inequality?/1)

    query = put_in(query, [lens], non_range_conditions)
    Enum.reduce(grouped_inequalities, query, &add_aligned_range(&1, &2, lens))
  end

  defp add_aligned_range({column, conditions}, query, lens) do
    {left, right} =
      conditions
      |> Enum.map(&Condition.value/1)
      |> Enum.sort(&Cloak.Data.lt_eq/2)
      |> List.to_tuple()
      |> FixAlign.align_interval()

    if implement_range?({left, right}, conditions) do
      [lhs, rhs] = conditions
      range = {:and, lhs, rhs}
      update_in(query, [lens], &Condition.combine(:and, range, &1))
    else
      query
      |> add_clause(lens, {:comparison, column, :<, Expression.constant(column.type, right)})
      |> add_clause(lens, {:comparison, column, :>=, Expression.constant(column.type, left)})
      |> Query.add_info(
        "The range for column #{Expression.display_name(column)} has been adjusted to #{left} <= " <>
          "#{Expression.short_name(column)} < #{right}."
      )
    end
  end

  defp implement_range?({left, right}, conditions) do
    [{_, _, left_operator, left_column}, {_, _, right_operator, right_column}] =
      Enum.sort_by(conditions, &Condition.value/1, &Cloak.Data.lt_eq/2)

    left_operator == :>= && left_column.value == left && right_operator == :< && right_column.value == right
  end

  defp add_clause(query, lens, clause), do: Lens.map(lens, query, &Condition.combine(:and, clause, &1))

  defp verify_ranges(grouped_inequalities) do
    grouped_inequalities
    |> Enum.reject(fn {_, comparisons} -> valid_range?(comparisons) end)
    |> case do
      [{_, [inequality | _]} | _] ->
        column = Condition.subject(inequality)

        raise CompilationError,
          source_location: column.source_location,
          message: "Column #{Expression.display_name(column)} must be limited to a finite, nonempty range."

      _ ->
        :ok
    end
  end

  defp valid_range?(comparisons) do
    case Enum.sort_by(comparisons, &Condition.direction/1, &Kernel.>/2) do
      [cmp1, cmp2] ->
        Condition.direction(cmp1) != Condition.direction(cmp2) &&
          Cloak.Data.lt(Condition.value(cmp1), Condition.value(cmp2))

      _ ->
        false
    end
  end

  defp inequalities_by_column(where_clause) do
    Lenses.conditions()
    |> Lens.to_list(where_clause)
    |> Enum.filter(&Condition.inequality?/1)
    |> Enum.group_by(&(&1 |> Condition.subject() |> Expression.semantic()))
    |> Enum.map(&discard_redundant_inequalities/1)
    |> Enum.into(%{})
  end

  defp discard_redundant_inequalities({column, inequalities}) do
    case {bottom, top} = Enum.split_with(inequalities, &(Condition.direction(&1) == :>)) do
      {[], []} ->
        {column, []}

      {_, []} ->
        {column, [Enum.max_by(bottom, &Condition.value/1)]}

      {[], _} ->
        {column, [Enum.min_by(top, &Condition.value/1)]}

      {_, _} ->
        {column, [Enum.max_by(bottom, &Condition.value/1), Enum.min_by(top, &Condition.value/1)]}
    end
  end

  defp extract_columns(columns), do: Query.Lenses.leaf_expressions() |> Lens.to_list(columns)

  defp compile_sample_rate(%Query{sample_rate: amount} = query) when amount != nil do
    if query.type == :standard,
      do: raise(CompilationError, message: "The `SAMPLE_USERS` clause is not valid in standard queries.")

    user_id_hash =
      Expression.function("hash", [Helpers.id_column(query)], :text)
      |> put_in([Query.Lenses.all_expressions() |> Lens.key(:synthetic?)], true)

    {aligned_sample_rate, messages} = align_sample_rate(amount)
    hash_limit = round(aligned_sample_rate / 100 * 4_294_967_295)
    hex_hash_limit = <<hash_limit::32>> |> Base.encode16(case: :lower) |> String.pad_leading(8, "0")

    sample_condition = {:comparison, user_id_hash, :<=, Expression.constant(:text, hex_hash_limit)}

    %Query{query | where: Condition.combine(:and, sample_condition, query.where)}
    |> Query.add_info(messages)
  end

  defp compile_sample_rate(query), do: query

  defp align_sample_rate(sample_rate) do
    aligned_sample_rate = FixAlign.align(sample_rate)

    if sample_rate == aligned_sample_rate do
      {aligned_sample_rate, []}
    else
      {aligned_sample_rate, ["Sample rate adjusted from #{sample_rate}% to #{aligned_sample_rate}%"]}
    end
  end

  # -------------------------------------------------------------------
  # Virtual tables
  # -------------------------------------------------------------------

  defp expand_virtual_tables(query) do
    query.selected_tables
    |> Enum.reject(&(&1.query == nil))
    |> Enum.reduce(query, fn %{query: table_query, name: table_name}, query ->
      Lenses.leaf_tables()
      |> Lens.filter(&(&1 == table_name))
      |> Lens.map(query, &{:subquery, %{alias: &1, ast: table_query}})
    end)
    |> Optimizer.optimize_columns_from_subqueries()
  end
end
