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

  defp extract_row(bucket, %{columns: columns}) do
    Enum.zip(bucket(bucket, :property), columns)
    |> Enum.map(&extract_value(&1, bucket(bucket, :noisy_count)))
  end

  defp extract_value({_value, {:count, _}}, count), do: count
  defp extract_value({value, _column}, _count), do: value

  defp expand_rows(results) do
    Enum.flat_map(results, fn result ->
      :lists.duplicate(bucket(result, :noisy_count), bucket(result, :property))
    end)
  end

  defp aggregate?(%{columns: [count: :star]}), do: true
  defp aggregate?(%{group_by: [_ | _]}), do: true
  defp aggregate?(_), do: false
end
