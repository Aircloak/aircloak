defmodule Air.DataSourceManager do
  @moduledoc """
  The DataSourceManager holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  use GenServer
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the DataSourceManager which allows the air application to, among other things,
  discover whether a datastore is available for querying, and if so where.
  """
  @spec start_link() :: {:ok, pid} | {:error, term}
  def start_link(), do: GenServer.start_link(__MODULE__, nil, name: {:global, __MODULE__})


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    Logger.info("Started the data source manager")
    state = %{}
    {:ok, state}
  end

  @doc false
  def handle_call(msg, _from, state) do
    raise "Unimplemented call: #{inspect msg}"
    {:reply, {:error, :not_implemented}, state}
  end

  @doc false
  def handle_cast(msg, state) do
    raise "Unimplemented cast: #{inspect msg}"
    {:noreply, state}
  end
end
