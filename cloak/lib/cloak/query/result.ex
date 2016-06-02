defmodule Cloak.Query.Result do
  @moduledoc "Contains functions for converting buckets to a columns/rows representation."

  use Cloak.Type
  alias Cloak.SqlQuery


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Converts a list of buckets into rows, expanding them if the query does not aggregate."
  @spec expand([Bucket.t], SqlQuery.t) :: [number | String.t]
  def expand(results, query) do
    if aggregate?(query) do
      extract_rows(results, query)
    else
      expand_rows(results)
    end
  end

  @doc "Sorts the rows in the order defined in the query."
  @spec apply_order([number | String.t], SqlQuery.t) :: [number | String.t]
  def apply_order(rows, %{columns: columns, order_by: order_by_spec}) do
    order_list = for {column, direction} <- order_by_spec do
      index = columns |> Enum.find_index(&(&1 == column))
      {index, direction}
    end

    Enum.sort(rows, &compare_rows(&1, &2, order_list))
  end
  def apply_order(rows, _), do: rows

  @doc "Returns a list of column titles for the query."
  @spec column_titles(SqlQuery.t) :: [String.t]
  def column_titles(%{columns: columns}) do
    Enum.map(columns, &column_title/1)
  end

  @doc "Returns a string title for the given column specification."
  @spec column_title(SqlQuery.column) :: String.t
  def column_title({:count, :star}), do: "count(*)"
  def column_title(column), do: column


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp extract_rows(results, query) do
    Enum.map(results, &extract_row(&1, query))
  end

  defp extract_row(row_bucket, %{columns: columns}) do
    row_bucket
    |> bucket(:property)
    |> Enum.zip(columns)
    |> Enum.map(fn
       {_value, {:count, _}} -> bucket(row_bucket, :noisy_count)
       {value, _column} -> value
    end)
  end

  defp expand_rows(results) do
    Enum.flat_map(results, fn result ->
      List.duplicate(bucket(result, :property), bucket(result, :noisy_count))
    end)
  end

  defp aggregate?(%{columns: [count: :star]}), do: true
  defp aggregate?(%{group_by: [_ | _]}), do: true
  defp aggregate?(_), do: false

  defp compare_rows(row1, row2, []), do: row1 < row2
  defp compare_rows(row1, row2, [{index, direction} | remaining_order]) do
    field1 = row1 |> Enum.at(index)
    field2 = row2 |> Enum.at(index)
    case field1 === field2 do
      :true -> compare_rows(row1, row2, remaining_order)
      :false -> compare_fields(field1, field2, direction)
    end
  end

  defp compare_fields(field1, field2, nil), do: compare_fields(field1, field2, :asc)
  defp compare_fields(field1, field2, :asc), do: field1 < field2
  defp compare_fields(field1, field2, :desc), do: field1 > field2
end
