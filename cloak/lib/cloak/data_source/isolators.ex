defmodule Cloak.DataSource.Isolators do
  @moduledoc "Entry point for checking if a column is isolating."

  alias Cloak.DataSource
  require Aircloak

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table, column), do: cache_module().isolates_users?(data_source, table, column)

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(arg), do: cache_module().child_spec(arg)

  defp cache_module(), do: Aircloak.in_env(test: Cloak.TestIsolatorsCache, else: Cloak.DataSource.Isolators.Cache)
end
