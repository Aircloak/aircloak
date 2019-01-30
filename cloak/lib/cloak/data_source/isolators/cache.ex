defmodule Cloak.DataSource.Isolators.Cache do
  @moduledoc """
  Implementation of the cache which holds the isolator property of all known columns of all data sources.
  """

  @day :timer.hours(24)
  @refresh_interval 60 * @day

  alias Cloak.DataSource.PerColumn.Cache

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(atom | pid, Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(cache_ref \\ __MODULE__, data_source, table_name, column_name),
    do: Cache.value(cache_ref, data_source, table_name, column_name)

  @doc "Performs a cache lookup."
  @spec lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, boolean} | {:error, :pending | :failed | :unknown_column}
  def lookup(cache_ref \\ __MODULE__, data_source, table_name, column_name),
    do: Cache.lookup(cache_ref, data_source, table_name, column_name)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compute_column_isolation({data_source_name, table_name, column_name}) do
    {:ok, data_source} = Cloak.DataSource.fetch(data_source_name)
    Cloak.DataSource.Isolators.Query.isolates_users?(data_source, table_name, column_name)
  end

  defp known_columns(data_sources), do: Enum.flat_map(data_sources, &data_source_columns/1)

  defp data_source_columns(data_source), do: Enum.flat_map(data_source.tables, &table_columns(data_source, &1))

  defp table_columns(_data_source, {_table_id, %{auto_isolating_column_classification: false}}), do: []

  defp table_columns(data_source, {_table_id, table}) do
    table.columns
    |> Enum.reject(&Map.has_key?(table.isolating_columns, &1.name))
    |> Enum.reject(&Map.has_key?(table.keys, &1.name))
    |> Enum.map(&{data_source.name, table.name, &1.name})
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(opts) do
    [
      columns_provider: &known_columns/1,
      property_fun: &compute_column_isolation/1,
      cache_owner: Cloak.DataSource.Isolators.PersistentKeyValue,
      refresh_interval: @refresh_interval,
      name: __MODULE__,
      registered?: true,
      auto_refresh?: true,
      default: true
    ]
    |> Keyword.merge(opts)
    |> Cache.child_spec()
  end
end
