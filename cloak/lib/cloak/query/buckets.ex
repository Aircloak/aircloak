defmodule Cloak.Query.Buckets do
  use Cloak.Type

  def expand(results, query) do
    if aggregate?(query) do
      extract_rows(results, query)
    else
      expand_rows(results)
    end
  end

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
      :lists.duplicate(bucket(result, :noisy_count), bucket(result, :property))
    end)
  end

  defp aggregate?(%{columns: [count: :star]}), do: true
  defp aggregate?(%{group_by: [_ | _]}), do: true
  defp aggregate?(_), do: false
end
