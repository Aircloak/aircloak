defmodule Cloak.Query.Rows do
  @moduledoc "Functions for row processing, such as filtering and grouping."
  alias Cloak.Sql.{Expression, Query, Condition, Compiler}

  @type groups :: %{Cloak.DataSource.row() => group_data}
  @type group_updater :: (integer(), group_data, Cloak.DataSource.row() | Cloak.Query.Result.bucket() -> group_data)
  @type group_data :: any

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a stream of rows or buckets passing all the given filters."
  @spec filter(Enumerable.t(), (any -> boolean) | nil) :: Enumerable.t()
  def filter(rows_or_buckets, nil), do: rows_or_buckets
  def filter(rows_or_buckets, filter), do: Stream.filter(rows_or_buckets, &(&1 |> fields() |> filter.() == true))

  @doc """
    Filters groups and extracts desired columns according to query specification.

    It is assumed that each row contains group expressions first, followed by query aggregators.
  """
  @spec extract_groups(Enumerable.t(), [Expression.t()], Query.t()) :: Enumerable.t()
  def extract_groups(rows, columns_to_select, query) do
    selected_columns = [:group_index | group_expressions(query) ++ query.aggregators]
    columns_to_select = Enum.map(columns_to_select, &update_row_index(query, &1, selected_columns))

    filters =
      Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.map(query.having, &update_row_index(query, &1, selected_columns))
      |> Condition.to_function()

    rows
    |> filter(filters)
    # sort by group index
    |> Enum.sort_by(&(&1 |> fields() |> Enum.at(0)))
    |> Enum.map(&select_values(&1, columns_to_select))
  end

  @doc "Groups input rows according to the query specification."
  @spec group(Enumerable.t(), Query.t(), (integer() -> group_data), group_updater) :: groups
  def group(rows, query, group_initializer, group_updater) do
    group_expressions = group_expressions(query)

    grouping_sets =
      if query.grouping_sets != [],
        do: query.grouping_sets,
        else: Compiler.Helpers.default_grouping_sets(group_expressions)

    Enum.reduce(rows, %{}, fn row_or_bucket, groups ->
      fields = fields(row_or_bucket)
      group_values = Enum.map(group_expressions, &Expression.value(&1, fields))

      grouping_sets
      |> Enum.with_index()
      |> Enum.reduce(groups, fn {grouping_set, grouping_set_index}, groups ->
        group =
          group_values
          |> Enum.with_index(0)
          |> Enum.map(fn {value, index} ->
            if index in grouping_set, do: value, else: nil
          end)

        group_data = Map.get(groups, [grouping_set_index | group], group_initializer.(grouping_set_index))
        Map.put(groups, [grouping_set_index | group], group_updater.(grouping_set_index, group_data, row_or_bucket))
      end)
    end)
  end

  @doc "Returns the list of expressions used to form the groups for aggregation and anonymization."
  @spec group_expressions(Query.t()) :: [Expression.t()]
  def group_expressions(query) do
    if query.group_by != [] or Compiler.Helpers.aggregates?(query),
      do: query.group_by,
      else: Query.bucket_columns(query)
  end

  @doc "Returns the fields from a row or bucket element."
  @spec fields(Cloak.DataSource.row() | Cloak.Query.Result.bucket()) :: Cloak.DataSource.row()
  def fields(row) when is_list(row), do: row
  def fields(bucket) when is_map(bucket), do: bucket.row

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_row_index(query, %Expression{kind: :function, name: "grouping_id"} = column, _selected_columns) do
    bits_indices =
      Enum.map(column.args, fn column ->
        Enum.find_index(query.group_by, &Expression.equals?(&1, column))
      end)

    %Expression{
      column
      | args: [
          %Expression{kind: :column, type: :integer, name: "group_index", row_index: 0},
          Expression.constant(:unknown, bits_indices),
          Expression.constant(:unknown, query.grouping_sets)
        ]
    }
  end

  defp update_row_index(query, column, selected_columns) do
    case Enum.find_index(selected_columns, &Expression.equals?(&1, column)) do
      nil ->
        if Expression.function?(column) do
          args = Enum.map(column.args, &update_row_index(query, &1, selected_columns))
          %Expression{column | args: args}
        else
          column
        end

      index ->
        %Expression{column | row_index: index}
    end
  end

  defp select_values(row, expressions) when is_list(row), do: Enum.map(expressions, &Expression.value(&1, row))

  defp select_values(bucket, expressions) when is_map(bucket),
    do: %{bucket | row: select_values(bucket.row, expressions)}
end
