defmodule Cloak.Query.Rows do
  @moduledoc "Functions for row processing, such as filtering and grouping."
  alias Cloak.Sql.{Expression, Query}

  @type groups :: %{Cloak.DataSource.row => group_data}
  @type group_updater :: ((group_data, Cloak.DataSource.row) -> group_data)
  @type group_data :: any


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a stream of rows passing all the given filters."
  @spec filter(Enumerable.t, (any -> boolean) | nil) :: Enumerable.t
  def filter(rows, nil), do: rows
  def filter(rows, filter), do: Stream.filter(rows, filter)

  @doc """
    Filters groups and extracts desired columns according to query specification.

    It is assumed that each row contains group expressions first, followed by query aggregators.
  """
  @spec extract_groups(Enumerable.t, [Expression.t], Query.t) :: Enumerable.t
  def extract_groups(rows, columns_to_select, query) do
    columns =
      (group_expressions(query) ++ query.aggregators)
      |> Enum.map(&Expression.unalias/1)
      |> Enum.with_index()
      |> Enum.into(%{})

    columns_to_select = Enum.map(columns_to_select, &Expression.unalias/1)

    rows
    |> Enum.filter(&filter_group(&1, columns, query))
    |> Enum.map(&selected_values(&1, columns, columns_to_select))
  end

  @doc "Groups input rows according to the query specification."
  @spec group(Enumerable.t, Query.t, group_data, group_updater) :: groups
  def group(rows, query, default_group_data, group_updater) do
    group_expressions = group_expressions(query)
    Enum.reduce(
      rows,
      %{},
      fn(row, groups) ->
        group_values = Enum.map(group_expressions, &Expression.value(&1, row))
        group_data = Map.get(groups, group_values, default_group_data)
        Map.put(groups, group_values, group_updater.(group_data, row))
      end
    )
  end

  @doc "Returns the list of expressions used to form the groups for aggregation and anonymization."
  @spec group_expressions(Query.t) :: [Expression.t]
  def group_expressions(%Query{group_by: [_|_] = group_by}), do:
    # There are group by clauses -> we're grouping on these clauses
    Expression.unique_except(group_by, &Expression.row_splitter?/1)
  def group_expressions(%Query{group_by: [], implicit_count?: true} = query) do
    # Group by is not provided, and no selected expression is an aggregation function ->
    #   we're grouping on all selected columns + non selected order by expressions.
    Expression.unique_except(
      query.columns ++ non_selected_order_by_expressions(query),
      &Expression.row_splitter?/1
    )
  end
  def group_expressions(%Query{group_by: [], implicit_count?: false}), do:
    # Group by is not provided, and all expressions are aggregate functions
    #   -> all rows fall in the same group
    []


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp non_selected_order_by_expressions(query), do:
    query |> Query.order_by_expressions() |> Enum.reject(& &1 in query.columns)

  defp selected_values(row, columns, columns_to_select), do:
    Enum.map(columns_to_select, &fetch_value!(row, &1, columns))

  defp fetch_value!(row, %Expression{function?: true, function_args: args} = function, columns) do
    case Map.fetch(columns, function) do
      {:ok, index} -> Enum.at(row, index)
      :error -> Expression.apply_function(function, Enum.map(args, &fetch_value!(row, &1, columns)))
    end
  end
  defp fetch_value!(row, column, columns) do
    case Map.fetch(columns, column) do
      {:ok, index} -> Enum.at(row, index)
      :error -> Expression.value(column, row)
    end
  end

  defp filter_group(row, columns, query), do: matches_having_condition?(row, query.having, columns)

  defp matches_having_condition?(_row, nil, _columns), do: true
  defp matches_having_condition?(row, {:and, lhs, rhs}, columns), do:
    matches_having_condition?(row, lhs, columns) and matches_having_condition?(row, rhs, columns)
  defp matches_having_condition?(row, {:or, lhs, rhs}, columns), do:
    matches_having_condition?(row, lhs, columns) or matches_having_condition?(row, rhs, columns)
  defp matches_having_condition?(row, {:comparison, column, operator, target}, columns) do
    value = fetch_value!(row, column, columns)
    target = fetch_value!(row, target, columns)
    compare(value, operator, target)
  end

  defp compare(nil, _op, _target), do: false
  defp compare(_value, _op, nil), do: false
  defp compare(value, :=, target), do: value == target
  defp compare(value, :<, target), do: value < target
  defp compare(value, :<=, target), do: value <= target
  defp compare(value, :>, target), do: value > target
  defp compare(value, :>=, target), do: value >= target
  defp compare(value, :<>, target), do: value != target
end
