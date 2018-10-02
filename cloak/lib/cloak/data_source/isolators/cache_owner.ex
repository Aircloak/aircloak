defmodule Cloak.DataSource.Isolators.CacheOwner do
  @moduledoc "Owner of the isolators cache ets table."

  alias Cloak.DataSource.PerColumn.CacheOwner

  # Version of the persisted cache format. Bump this if you're changing the format.
  @persisted_cache_version 1

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Performs a lookup into the isolator cache table."
  @spec lookup(Queue.column()) :: {:ok, boolean} | :error
  def lookup(column), do: CacheOwner.lookup(__MODULE__, column)

  @doc "Stores an item into the isolator cache table."
  @spec store(Queue.column(), boolean) :: :ok
  def store(column, isolated), do: CacheOwner.store(__MODULE__, column, isolated)

  @doc "Deletes unkown columns from the cache table."
  @spec remove_unknown_columns(Queue.columns()) :: :ok
  def remove_unknown_columns(known_columns), do: CacheOwner.remove_unknown_columns(__MODULE__, known_columns)

  @doc "Returns the collection of cached columns."
  @spec cached_columns() :: Queue.columns()
  def cached_columns(), do: CacheOwner.cached_columns(__MODULE__)

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg),
    do: CacheOwner.child_spec(%{name: __MODULE__, persisted_cache_version: @persisted_cache_version})

  @doc false
  def cache_file(), do: CacheOwner.cache_file(__MODULE__)
end
