defmodule Cloak.TestBoundsCache do
  @moduledoc """
  Mock bounds cache used in tests. Returns :unknown for all columns by default. Returns the given bounds
  for columns for which `set` was called.
  """

  def lookup(data_source, table, column) do
    Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column}))
  end

  def set(data_source, table, column, value) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, value))
  end

  def child_spec(_), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end
