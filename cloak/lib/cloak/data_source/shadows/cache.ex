defmodule Cloak.DataSource.Shadows.Cache do
  @moduledoc "Implementation of the cache which holds the shadow table of all known columns of all data sources."

  @day :timer.hours(24)
  @refresh_interval 30 * @day

  alias Cloak.DataSource.PerColumn.Cache

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the shadow table for the given column."
  @spec shadow(atom | pid, Cloak.DataSource.t(), String.t(), String.t()) :: [any]
  def shadow(cache_ref \\ __MODULE__, data_source, table_name, column_name),
    do: Cache.value(cache_ref, data_source, table_name, column_name)

  @doc "Performs a cache lookup."
  @spec lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, [any]} | {:error, :pending | :failed | :unknown_column}
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

  defp compute_shadow_table({data_source, table_name, column_name}) do
    Cloak.DataSource.Shadows.Query.build_shadow(data_source, table_name, column_name)
  end

  defp known_columns(data_sources) do
    for data_source <- data_sources,
        {_id, table} <- data_source.tables,
        column <- table.columns do
      {data_source, table.name, column.name}
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(opts) do
    [
      columns_provider: &known_columns/1,
      property_fun: &compute_shadow_table/1,
      cache_owner: Cloak.DataSource.Shadows.PersistentKeyValue,
      refresh_interval: @refresh_interval,
      name: __MODULE__,
      registered?: true,
      auto_refresh?: true,
      default: []
    ]
    |> Keyword.merge(opts)
    |> Cache.child_spec()
  end
end
