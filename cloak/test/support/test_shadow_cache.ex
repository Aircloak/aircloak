defmodule Cloak.TestShadowCache do
  @moduledoc """
  Mock shadow cache used in tests. Returns an empty list for all columns by default.  Forwards to
  `Cloak.DataSource.Shadows.Query` for columns for which `live` was called.
  """

  def strict(strict), do: Application.put_env(:cloak, {__MODULE__, :strict?}, strict)

  def shadow(data_source, table, column) do
    case Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column})) do
      :error -> if Application.get_env(:cloak, {__MODULE__, :strict?}, false), do: raise("not found"), else: []
      {:ok, {:safe, values}} -> values
      {:ok, :live} -> Cloak.DataSource.Shadows.Query.build_shadow(data_source, table, column)
      {:ok, :forward} -> Cloak.DataSource.Shadows.Cache.shadow(data_source, table, column)
    end
  end

  def lookup(data_source, table, column) do
    case Agent.get(__MODULE__, &Map.fetch(&1, {data_source.name, table, column})) do
      :error -> if Application.get_env(:cloak, {__MODULE__, :strict?}, false), do: raise("not found"), else: []
      {:ok, {:safe, values}} -> {:ok, values}
      {:ok, :live} -> {:ok, Cloak.DataSource.Shadows.Query.build_shadow(data_source, table, column)}
      {:ok, :forward} -> Cloak.DataSource.Shadows.Cache.lookup(data_source, table, column)
    end
  end

  def live(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :live))
  end

  def forward(data_source, table, column) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, :forward))
  end

  def safe(data_source, table, column, values) do
    Agent.update(__MODULE__, &Map.put(&1, {data_source.name, table, column}, {:safe, values}))
  end

  def data_sources_changed(),
    do: send(Cloak.DataSource.Shadows.Cache, {:data_sources_changed, Cloak.DataSource.all()})

  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [
        mock_spec(),
        # avoiding auto refresh in tests, because of many frequent changes to datasources
        {Cloak.DataSource.Shadows.Cache, auto_refresh?: false}
      ],
      strategy: :one_for_one,
      name: __MODULE__.Supervisor
    )
  end

  def mock_spec(), do: Aircloak.ChildSpec.agent(fn -> %{} end, name: __MODULE__)
end
