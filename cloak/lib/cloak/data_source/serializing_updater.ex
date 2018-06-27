defmodule Cloak.DataSource.SerializingUpdater do
  @moduledoc """
  Serializes data source updates, guarding against accidental race conditions
  that can occur when multiple processes update a data source concurrently.
  """

  use GenServer, start: {__MODULE__, :start_link, []}
  alias Aircloak.ChildSpec
  alias Cloak.DataSource
  require Logger
  require Aircloak

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Processes a change event to a data source definition file"
  @spec process_update(String.t()) :: :ok
  def process_update(path), do: GenServer.cast(__MODULE__, {:process_update, path})

  @doc "Processes a removal event to a data source definition file"
  @spec process_removal(String.t()) :: :ok
  def process_removal(path), do: GenServer.cast(__MODULE__, {:process_removal, path})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_cast(:run_liveness_check, state) do
    DataSource.perform_data_source_availability_checks()
    {:noreply, state}
  end

  def handle_cast({:process_update, file_path}, state) do
    Logger.debug(fn -> "Reloading data source configuration at #{file_path}." end)
    DataSource.initialize_data_source_from_path(file_path)
    {:noreply, state}
  end

  def handle_cast({:process_removal, _file_path}, state) do
    Logger.debug(fn -> "Data source removal detected. Reloading all data source configurations." end)
    DataSource.reinitialize_all_data_sources()
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Supervison tree callback
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_options \\ []) do
    Aircloak.unused(periodic_liveness_check(), in: [:test])

    ChildSpec.supervisor(
      [
        ChildSpec.gen_server(__MODULE__, [], name: __MODULE__),
        Aircloak.in_env(test: nil, else: periodic_liveness_check()),
        Cloak.DataSource.FileSystemMonitor
      ]
      |> Enum.reject(&is_nil/1),
      strategy: :one_for_all,
      name: __MODULE__.Supervisor
    )
  end

  defp periodic_liveness_check() do
    {Periodic,
     id: :liveness_check,
     run: fn -> GenServer.cast(__MODULE__, :run_liveness_check) end,
     every: Aircloak.in_env(dev: :timer.hours(1), else: :timer.minutes(1))}
  end
end
