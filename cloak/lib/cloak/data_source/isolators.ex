defmodule Cloak.DataSource.Isolators do
  @moduledoc "Entry point for checking if a column is isolating."

  require Aircloak

  @cache_module Aircloak.in_env(test: Cloak.TestIsolatorsCache, else: Cloak.DataSource.Isolators.Cache)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  defdelegate isolates_users?(data_source, table, column), to: @cache_module

  @doc "Invoked when data sources have been changed."
  @spec data_sources_changed() :: :ok
  defdelegate data_sources_changed(), to: @cache_module

  @doc "Returns true if the isolated property for the given column is computed."
  @spec computed?(Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  def computed?(data_source, table, column),
    do: match?({:ok, _isolated?}, Cloak.DataSource.Isolators.CacheOwner.lookup({data_source.name, table, column}))

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
