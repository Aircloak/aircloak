defmodule Cloak.TestIsolatorsCache do
  @moduledoc "Mock isolators cache used in tests."
  use Agent

  @doc false
  def start_link(_), do: Agent.start_link(fn -> MapSet.new() end, name: __MODULE__)

  def isolates_users?(data_source, table, column) do
    Agent.get(__MODULE__, &MapSet.member?(&1, {data_source[:name], table, column}))
  end

  def register_isolating_column(data_source, table, column) do
    Agent.update(__MODULE__, &MapSet.put(&1, {data_source[:name], table, column}))
  end
end
