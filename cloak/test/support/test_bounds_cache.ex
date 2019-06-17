defmodule Cloak.TestBoundsCache do
  @moduledoc """
  Mock bounds cache used in tests. Returns :unknown for all columns by default. Returns the given bounds
  for columns for which `set` was called.
  """

  def lookup(data_source, table, column) do
    Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column}))
    |> case do
      {:ok, :forward} -> {:ok, Cloak.DataSource.Bounds.Cache.value(data_source, table, column)}
      other -> other
    end
  end

  def set(data_source, table, column, value) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, value))
  end

  def forward(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :forward))
  end

  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [
        mock_spec(),
        # avoiding auto refresh in tests, because of many frequent changes to datasources
        {Cloak.DataSource.Bounds.Cache, auto_refresh?: false}
      ],
      strategy: :one_for_one,
      name: __MODULE__.Supervisor
    )
  end

  def mock_spec(), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end
