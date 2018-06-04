defmodule Cloak.DataSource.Isolators.CacheOwner do
  @moduledoc "Owner of the isolators cache ets table."
  use GenServer
  alias Cloak.DataSource.Isolators.Queue

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Performs a lookup into the isolator cache table."
  @spec lookup(Queue.column()) :: {:ok, boolean} | :error
  def lookup(column) do
    case :ets.match(__MODULE__, {column, :"$1"}) do
      [[isolates?]] -> {:ok, isolates?}
      [] -> :error
    end
  end

  @doc "Stores an item into the isolator cache table."
  @spec store(Queue.column(), boolean) :: :ok
  def store(column, isolated) do
    :ets.insert(__MODULE__, {column, isolated})
    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_arg) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true])
    {:ok, nil}
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: GenServer.start_link(__MODULE__, nil)
end
