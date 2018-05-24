defmodule Cloak.DataSource.Isolators do
  @moduledoc "Entry point for checking if a column is isolating."

  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(DataSource.t(), atom, String.t()) :: boolean
  def isolates_users?(data_source, table, column) do
    __MODULE__.Query.isolates_users?(data_source, table, column)
  end
end
