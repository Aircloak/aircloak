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

  @doc "Returns a list of column titles for the query."
  @spec column_titles(SqlQuery.t) :: [String.t]
  def column_titles(%{columns: columns}) do
    Enum.map(columns, &column_title/1)
  end

  #
  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp column_title({:count, :star}), do: "count(*)"
  defp column_title(column), do: column

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
end
