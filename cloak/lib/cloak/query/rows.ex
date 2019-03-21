defmodule Cloak.Query.Rows do
  @moduledoc "Functions for row processing, such as filtering and grouping."
  alias Cloak.Sql.{Expression, Query, Condition, Compiler}

  @type groups :: %{Cloak.DataSource.row() => group_data}
  @type group_updater :: (group_data, Cloak.DataSource.row() | Cloak.Query.Result.bucket() -> group_data)
  @type group_data :: any

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a stream of rows or buckets passing all the given filters."
  @spec filter(Enumerable.t(), (any -> boolean) | nil) :: Enumerable.t()
  def filter(rows_or_buckets, nil), do: rows_or_buckets
  def filter(rows_or_buckets, filter), do: Stream.filter(rows_or_buckets, &(&1 |> fields() |> filter.()))

  @doc """
    Filters groups and extracts desired columns according to query specification.

    It is assumed that each row contains group expressions first, followed by query aggregators.
  """
  @spec extract_groups(Enumerable.t(), [Expression.t()], Query.t()) :: Enumerable.t()
  def extract_groups(rows, columns_to_select, query) do
    selected_columns = group_expressions(query) ++ query.aggregators
    columns_to_select = Enum.map(columns_to_select, &update_row_index(&1, selected_columns))

    filters =
      Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.map(query.having, &update_row_index(&1, selected_columns))
      |> Condition.to_function()

    rows
    |> filter(filters)
    |> Enum.map(&select_values(&1, columns_to_select))
  end

  @doc "Groups input rows according to the query specification."
  @spec group(Enumerable.t(), Query.t(), group_data, group_updater) :: groups
  def group(rows, query, default_group_data, group_updater) do
    {grouping_sets, group_expressions} = grouping_sets(query)

    Enum.reduce(rows, %{}, fn row_or_bucket, groups ->
      fields = fields(row_or_bucket)
      group_values = Enum.map(group_expressions, &Expression.value(&1, fields))

      Enum.reduce(grouping_sets, groups, fn grouping_set, groups ->
        group =
          group_values
          |> Enum.with_index(0)
          |> Enum.map(fn {value, index} ->
            if index in grouping_set, do: value, else: nil
          end)

        group_data = Map.get(groups, group, default_group_data)
        Map.put(groups, group, group_updater.(group_data, row_or_bucket))
      end)
    end)
  end

  @doc "Returns the list of expressions used to form the groups for aggregation and anonymization."
  @spec group_expressions(Query.t()) :: [Expression.t()]
  def group_expressions(%Query{group_by: [_ | _] = group_by}),
    # There are group by clauses -> we're grouping on these clauses
    do: group_by

  def group_expressions(%Query{group_by: [], implicit_count?: true} = query) do
    # Group by is not provided, and no selected expression is an aggregation function ->
    #   we're grouping on all selected columns + non selected order by expressions.
    Expression.unique(query.columns ++ non_selected_order_by_expressions(query))
  end

  def group_expressions(%Query{group_by: [], implicit_count?: false}),
    # Group by is not provided, and all expressions are aggregate functions
    #   -> all rows fall in the same group
    do: []

  @doc "Returns the fields from a row or bucket element."
  @spec fields(Cloak.DataSource.row() | Cloak.Query.Result.bucket()) :: Cloak.DataSource.row()
  def fields(row) when is_list(row), do: row
  def fields(bucket) when is_map(bucket), do: bucket.row

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_row_index(column, selected_columns) do
    case Enum.find_index(selected_columns, &Expression.equals?(&1, column)) do
      nil ->
        if column.function? do
          args = Enum.map(column.function_args, &update_row_index(&1, selected_columns))
          %Expression{column | function_args: args}
        else
          column
        end

      index ->
        %Expression{column | row_index: index}
    end
  end

  defp non_selected_order_by_expressions(query),
    do: query |> Query.order_by_expressions() |> Enum.reject(&(&1 in query.columns))

  defp select_values(row, expressions) when is_list(row), do: Enum.map(expressions, &Expression.value(&1, row))

  defp select_values(bucket, expressions) when is_map(bucket),
    do: %{bucket | row: select_values(bucket.row, expressions)}

  defp grouping_sets(%Query{grouping_sets: [_ | _]} = query),
    # There are group by clauses -> we're grouping on these clauses
    do: {query.grouping_sets, query.group_by}

  defp grouping_sets(%Query{grouping_sets: [], implicit_count?: true} = query) do
    # Group by is not provided, and no selected expression is an aggregation function ->
    #   we're grouping on all selected columns + non selected order by expressions.
    groups = Expression.unique(query.columns ++ non_selected_order_by_expressions(query))
    {Compiler.Helpers.default_grouping_sets(groups), groups}
  end

  defp grouping_sets(%Query{grouping_sets: [], implicit_count?: false}),
    # Group by is not provided, and all expressions are aggregate functions
    #   -> all rows fall in the same group
    do: {[[]], []}
end
