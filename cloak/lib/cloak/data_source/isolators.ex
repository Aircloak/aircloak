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

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  defdelegate child_spec(arg), to: @cache_module
end
