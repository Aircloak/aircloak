defmodule Cloak.DataSource.Isolators.PersistentKeyValue do
  @moduledoc "Owner of the isolators cache ets table."

  alias Cloak.DataSource.PerColumn.PersistentKeyValue

  # Version of the persisted cache format. Bump this if you're changing the format.
  @persisted_cache_version 2

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg),
    do: PersistentKeyValue.child_spec(%{name: __MODULE__, persisted_cache_version: @persisted_cache_version})

  @doc false
  def cache_file(), do: PersistentKeyValue.cache_file(__MODULE__)
end
