defmodule Cloak.DataSource.SerializingUpdater do
  @moduledoc """
  Serializes data source updates, guarding against accidental race conditions
  that can occur when multiple processes update a data source concurrently.
  """

  use GenServer, start: {__MODULE__, :start_link, []}
  alias Cloak.DataSource

  require Logger
  require Aircloak.{DeployConfig, File}

  @file_system_monitor_name __MODULE__.FileSystemMonitor


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(options) do
    if Keyword.get(options, :subscribe_to_monitor) do
      Logger.info("Monitoring for data source definition changes")
      FileSystem.subscribe(@file_system_monitor_name)
    end
    activate_monitor_timer()
    {:ok, %{}}
  end

  @impl GenServer
  def handle_info(:monitor_availability, state) do
    DataSource.perform_data_source_availability_checks()
    activate_monitor_timer()
    {:noreply, state}
  end
  def handle_info({:file_event, _worker_pid, {file_path, events}}, state) do
    if :removed in events do
      Logger.debug("Data source removal detected. Reloading all data source configurations.")
      DataSource.reinitialize_all_data_sources()
    else
      Logger.debug("Reloading data source configuration at #{file_path}.")
      DataSource.initialize_data_source_from_path(file_path)
    end
    {:noreply, state}
  end
  def handle_info({:file_event, _worker_pid, :stop}, state) do
    Logger.warn("Data source definition change detector terminated unexpectedly.")
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp activate_monitor_timer() do
    interval = Application.get_env(:cloak, :data_source_monitor_interval)
    if interval != nil, do: Process.send_after(self(), :monitor_availability, interval)
  end

  defp child_specs() do
    import Aircloak.ChildSpec
    if configured_with_individual_configurations?() do
      [
        %{
          id: FileSystem,
          start: {FileSystem, :start_link, [[dirs: [config_path()], name: @file_system_monitor_name]]},
        },
        gen_server(__MODULE__, [subscribe_to_monitor: true], name: __MODULE__),
      ]
    else
      [
        gen_server(__MODULE__, [subscribe_to_monitor: false], name: __MODULE__),
      ]
    end
  end

  defp configured_with_individual_configurations?(), do:
    not is_nil(config_path())

  defp config_path() do
    case Aircloak.DeployConfig.fetch!("data_sources") do
      path when is_binary(path) -> Path.join([Aircloak.File.config_dir_path(), path])
      _other -> nil
    end
  end


  # -------------------------------------------------------------------
  # Supervison tree callback
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_options \\ []) do
    import Aircloak.ChildSpec
    supervisor(child_specs(), strategy: :one_for_all, name: __MODULE__.Supervisor)
  end
end
