defmodule Cloak.Query.Result do
  @moduledoc "Contains functions for converting buckets to a columns/rows representation."

  use Cloak.Type
  alias Cloak.SqlQuery


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Converts a list of buckets into rows, expanding them if the query does not aggregate."
  @spec expand([Bucket.t]) :: [Property.t]
  def expand(results) do
    Enum.flat_map(results, fn(result) ->
      List.duplicate(bucket(result, :property), bucket(result, :noisy_count))
    end)
  end

  @doc """
  For aggregating queries (ones that group or use aggregate functions) this converts the buckets to contain
  the values of the aggregating functions and have count 1.
  """
  @spec apply_aggregation([Bucket.t], SqlQuery.t) :: [Bucket.t]
  def apply_aggregation(results, query) do
    if aggregate?(query) do
      do_apply_aggregation(results, query)
    else
      results
    end
  end

  @doc "Sorts the rows in the order defined in the query."
  @spec apply_order([Bucket.t], SqlQuery.t) :: [Bucket.t]
  def apply_order(buckets, %{order_by: order_list}) do
    Enum.sort(buckets, fn(bucket1, bucket2) ->
      compare_rows(bucket(bucket1, :property), bucket(bucket2, :property), order_list)
    end)
  end
  def apply_order(buckets, _), do: buckets


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_apply_aggregation(results, query) do
    Enum.map(results, fn(result) ->
      bucket(result, noisy_count: 1, property: assemble_row(result, query))
    end)
  end

  defp assemble_row(row_bucket, %{columns: columns}) do
    row_bucket
    |> bucket(:property)
    |> Enum.zip(columns)
    |> Enum.map(fn
       {_value, {:function, "count", _}} -> bucket(row_bucket, :noisy_count)
       {value, _column} -> value
    end)
  end

  defp aggregate?(%{columns: [{:function, _, _}]}), do: true
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
  defp compare_fields(:*, _, _), do: false
  defp compare_fields(_, :*, _), do: true
  defp compare_fields(nil, _, :asc), do: false
  defp compare_fields(_, nil, :asc), do: true
  defp compare_fields(nil, _, :desc), do: true
  defp compare_fields(_, nil, :desc), do: false
  defp compare_fields(field1, field2, :asc), do: field1 < field2
  defp compare_fields(field1, field2, :desc), do: field1 > field2
end
