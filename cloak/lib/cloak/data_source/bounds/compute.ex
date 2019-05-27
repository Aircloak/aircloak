defmodule Cloak.DataSource.Bounds.Compute do
  alias Cloak.Query.Anonymizer

  def max(data) do
    if length(data) < bound_size_cutoff() do
      :error
    else
      {:ok, 10}
    end
  end

  defp bound_size_cutoff(), do: Anonymizer.config(:bound_size_cutoff)
end
