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
    do: Cache.value(cache_ref, data_source, table_name, column_name) != false

  @doc "Performs a cache lookup."
  @spec lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, boolean} | {:error, :pending | :failed | :unknown_column}
  def lookup(cache_ref \\ __MODULE__, data_source, table_name, column_name),
    do: Cache.lookup(cache_ref, data_source, table_name, column_name)

  @doc "Updates the cache with a result computed in another Cloak."
  @spec update_with_remote_result(GenServer.server(), %{
          descriptor: Descriptor.t(),
          status: :ok,
          expires: NaiveDateTime.t(),
          result: any,
          type: Result.result_type()
        }) :: :ok
  def update_with_remote_result(cache_ref \\ __MODULE__, result), do: Cache.update_with_remote_result(cache_ref, result)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compute_column_isolation({data_source, table_name, column_name}) do
    Cloak.DataSource.Isolators.Query.isolates_users?(data_source, table_name, column_name)
  end

  defp known_columns(data_sources), do: Enum.flat_map(data_sources, &data_source_columns/1)

  defp data_source_columns(data_source) do
    data_source.tables
    |> Enum.map(fn {_table_id, table} -> table end)
    |> Enum.filter(& &1.auto_isolating_column_classification)
    |> Enum.sort_by(&length(List.wrap(&1.user_id_join_chain)))
    |> Enum.flat_map(&table_columns(data_source, &1))
  end

  defp table_columns(data_source, table) do
    table.columns
    |> Enum.reject(&Map.has_key?(table.isolating_columns, &1.name))
    |> Enum.map(&{data_source, table.name, &1.name})
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
