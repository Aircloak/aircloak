defmodule Cloak.TestIsolatorsCache do
  @moduledoc """
  Mock isolators cache used in tests. Returns false for all columns by default. Returns true for columns for which
  `register_isolator` was called. Forwards to `Cloak.DataSource.Isolators.Cache` for columns for which
  `forward_isolator` was called.
  """

  def isolates_users?(data_source, table, column) do
    case Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column})) do
      :error -> false
      {:ok, :isolates} -> true
      {:ok, :forward} -> Cloak.DataSource.Isolators.Cache.isolates_users?(data_source, table, column)
    end
  end

  def register_isolator(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :isolates))
  end

  def forward_isolator(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :forward))
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
