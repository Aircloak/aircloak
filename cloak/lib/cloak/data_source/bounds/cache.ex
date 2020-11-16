defmodule Cloak.DataSource.Bounds.Cache do
  @moduledoc "Implementation of the cache which holds the bounds of all known columns of all data sources."

  @day :timer.hours(24)
  @refresh_interval 60 * @day

  alias Cloak.DataSource.PerColumn.Cache
  alias Cloak.Sql.Expression

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Performs a cache lookup."
  @spec lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, Expression.bounds()} | {:error, :pending | :failed | :unknown_column}
  def lookup(cache_ref \\ __MODULE__, data_source, table_name, column_name),
    do: Cache.lookup(cache_ref, data_source, table_name, column_name)

  @doc "Returns the cached bound value, waiting until it's computed if it's not yet in the cache."
  @spec value(Cloak.DataSource.t(), String.t(), String.t()) :: Expression.bounds()
  def value(cache_ref \\ __MODULE__, data_source, table_name, column_name),
    do: Cache.value(cache_ref, data_source, table_name, column_name)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compute_bounds({data_source, table_name, column_name}) do
    Cloak.DataSource.Bounds.Query.bounds(data_source, table_name, column_name)
  end

  defp known_columns(data_sources) do
    for data_source = %{bound_computation_enabled: true} <- data_sources,
        {_id, table} <- data_source.tables,
        column <- table.columns,
        column.type in [:integer, :real, :date, :datetime] do
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
      property_fun: &compute_bounds/1,
      cache_owner: Cloak.DataSource.Bounds.PersistentKeyValue,
      refresh_interval: @refresh_interval,
      name: __MODULE__,
      registered?: true,
      auto_refresh?: true,
      default: :unknown
    ]
    |> Keyword.merge(opts)
    |> Cache.child_spec()
  end
end
