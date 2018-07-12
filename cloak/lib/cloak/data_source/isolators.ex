defmodule Cloak.DataSource.Isolators do
  @moduledoc "Entry point for checking if a column is isolating."

  require Aircloak

  @cache_module Aircloak.in_env(test: Cloak.TestIsolatorsCache, else: Cloak.DataSource.Isolators.Cache)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table, column) do
    case preconfigured(data_source, table, column) do
      {:ok, result} -> result
      :error -> @cache_module.isolates_users?(data_source, table, column)
    end
  end

  @doc "Performs a cache lookup for the given column."
  @spec cache_lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, boolean} | {:error, :failed | :pending | :unknown_column}
  def cache_lookup(data_source, table_name, column_name) do
    case preconfigured(data_source, table_name, column_name) do
      {:ok, result} -> {:ok, result}
      :error -> @cache_module.lookup(data_source, table_name, column_name)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp preconfigured(data_source, table_name, column) do
    {_, table} = Enum.find(data_source.tables, fn {_key, table} -> table.name == table_name end)

    if table.auto_isolating_column_classification do
      Map.fetch(table.isolating_columns, column)
    else
      {:ok, Map.get(table.isolating_columns, column, true)}
    end
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(arg) do
    Aircloak.ChildSpec.supervisor(
      [
        # The cache table is owned by a separate process. This is mostly done for testing purposes, but it also improves
        # fault-tolerance. If the cache process crashes, the cache table will survive.
        __MODULE__.CacheOwner,
        {@cache_module, arg}
      ],
      strategy: :rest_for_one
    )
  end
end
