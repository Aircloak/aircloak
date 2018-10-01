defmodule Cloak.TestShadowCache do
  @moduledoc """
  Mock shadow cache used in tests. Returns an empty list for all columns by default.  Forwards to
  `Cloak.DataSource.Shadows.Query` for columns for which `live` was called.
  """

  def build_shadow(data_source, table, column) do
    case Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column})) do
      :error -> []
      {:ok, :live} -> Cloak.DataSource.Shadows.Query.build_shadow(data_source, table, column)
    end
  end

  def live(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :live))
  end

  def child_spec(_), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end
