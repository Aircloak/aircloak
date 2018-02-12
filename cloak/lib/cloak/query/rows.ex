defmodule Cloak.Query.Rows do
  @moduledoc "Functions for row processing, such as filtering and grouping."
  alias Cloak.Sql.{Expression, Query, Condition}

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
    source =
      (group_expressions(query) ++ query.aggregators)
      |> Enum.map(&clear/1)
      |> Enum.with_index()
      |> Enum.into(%{})

    columns_to_select = Enum.map(columns_to_select, &update_row_index(&1, source))
    filters =
      Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.map(query.having, &update_row_index(&1, source))
      |> Condition.to_function()

    rows
    |> filter(filters)
    |> Enum.map(&select_values(&1, columns_to_select))
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

  defp update_row_index(column, source) do
    case Map.fetch(source, clear(column)) do
      {:ok, index} -> %Expression{column| row_index: index}
      :error ->
        if column.function? do
          args = Enum.map(column.function_args, &update_row_index(&1, source))
          %Expression{column| function_args: args}
        else
          column
        end
    end
  end

  defp non_selected_order_by_expressions(query), do:
    query |> Query.order_by_expressions() |> Enum.reject(& &1 in query.columns)

  defp select_values(row, expressions), do: Enum.map(expressions, &Expression.value(&1, row))

  defp clear(expression), do: %Expression{expression | alias: nil, row_index: nil}
end
