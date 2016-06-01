defmodule Cloak.Query.Buckets do
  use Cloak.Type

  def expand(results, query) do
    if aggregate?(query) do
      extract_rows(results)
    else
      expand_rows(results)
    end
  end

  defp extract_rows(results) do
    Enum.map(results, fn result -> [bucket(result, :noisy_count)] end)
  end
  defp expand_rows(results) do
    Enum.flat_map(results, fn result ->
      :lists.duplicate(bucket(result, :noisy_count), bucket(result, :property))
    end)
  end

  defp aggregate?(%{columns: [count: :star]}), do: true
  defp aggregate?(_), do: false
end
