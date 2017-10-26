defmodule Cloak.DataSource.SerializingUpdater do
  @moduledoc """
  Serializes data source updates, guarding against accidental race conditions
  that can occur when multiple processes update a data source concurrently.
  """

  use GenServer, start: {__MODULE__, :start_link, []}
  alias Cloak.DataSource

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Asynchronously runs a check to figure out if we can still connect to the data sources"
  @spec run_liveness_check() :: :ok
  def run_liveness_check(), do:
    GenServer.cast(__MODULE__, :run_liveness_check)

  @doc "Processes a change event to a data source definition file"
  @spec process_update(String.t) :: :ok
  def process_update(path), do:
    GenServer.cast(__MODULE__, {:process_update, path})

  @doc "Processes a removal event to a data source definition file"
  @spec process_removal(String.t) :: :ok
  def process_removal(path), do:
    GenServer.cast(__MODULE__, {:process_removal, path})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do:
    {:ok, nil}

  @impl GenServer
  def handle_cast(:run_liveness_check, state) do
    DataSource.perform_data_source_availability_checks()
    {:noreply, state}
  end
  def handle_cast({:process_update, file_path}, state) do
    Logger.debug("Reloading data source configuration at #{file_path}.")
    DataSource.initialize_data_source_from_path(file_path)
    {:noreply, state}
  end
  def handle_cast({:process_removal, _file_path}, state) do
    Logger.debug("Data source removal detected. Reloading all data source configurations.")
    DataSource.reinitialize_all_data_sources()
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Supervison tree callback
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_options \\ []) do
    import Aircloak.ChildSpec
    supervisor([
        gen_server(__MODULE__, [], name: __MODULE__),
        Cloak.DataSource.FileSystemMonitor,
      ], strategy: :one_for_all, name: __MODULE__.Supervisor
    )
  end
end
