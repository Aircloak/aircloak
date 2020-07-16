defmodule Cloak.TestIsolatorsCache do
  @moduledoc """
  Mock isolators cache used in tests. Returns false for all columns by default. Returns true for columns for which
  `register_isolator` was called.
  """

  def isolates_users?(data_source, table, column) do
    case Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column})) do
      :error -> false
      {:ok, :isolates} -> true
    end
  end

  def lookup(data_source, table, column) do
    case Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column})) do
      :error -> {:ok, false}
      {:ok, :isolates} -> {:ok, true}
      {:ok, :pending} -> {:error, :pending}
    end
  end

  def register_isolator(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :isolates))
  end

  def register_pending(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :pending))
  end

  def data_sources_changed(),
    do: send(Cloak.DataSource.Isolators.Cache, {:data_sources_changed, Cloak.DataSource.all()})

  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [
        mock_spec(),
        # avoiding auto refresh in tests, because of many frequent changes to datasources
        {Cloak.DataSource.Isolators.Cache, auto_refresh?: false}
      ],
      strategy: :one_for_one,
      name: __MODULE__.Supervisor
    )
  end

  def mock_spec(), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end
