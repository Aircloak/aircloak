defmodule Cloak.DataSource.FileSystemMonitor do
  @moduledoc """
  Monitors the file system for changes to data source definitions and spawns
  of the appropriate actions.
  """

  use GenServer
  alias Aircloak.ChildSpec
  require Logger
  require Aircloak.{DeployConfig, File}

  @file_system_monitor_name __MODULE__.FileSystemMonitor
  @pre_processing_delay_ms 200


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    FileSystem.subscribe(@file_system_monitor_name)
    {:ok, []}
  end

  @impl GenServer
  def handle_info(:timeout, messages) do
    messages
    |> group_by_data_source_config()
    |> consolidate_and_classify_events()
    |> handle_events()
    {:noreply, []}
  end
  def handle_info({:file_event, _pid, {file_path, events}}, messages) do
    {:noreply, [{file_path, events} | messages], @pre_processing_delay_ms}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp group_by_data_source_config(messages), do:
    Enum.group_by(messages, fn({file_path, _events}) -> file_path end)

  defp consolidate_and_classify_events(events_by_file), do:
    Enum.map(events_by_file, fn({file_path, file_events}) -> {file_path, processed_events(file_events)} end)

  defp processed_events(file_events), do:
    file_events
    |> unique_file_events()
    |> classify_events()

  defp unique_file_events(file_events), do:
    file_events
    |> Enum.flat_map(fn({_path, events}) -> events end)
    |> Enum.uniq()

  defp classify_events(events) do
    if removal_event?(events) do
      :removal
    else
      :update
    end
  end

  defp removal_event?(events), do:
    Enum.any?([:removed, :deleted, :renamed], & &1 in events)

  defp handle_events(events), do:
    Enum.each(events, fn
      {path, :update} -> Cloak.DataSource.SerializingUpdater.process_update(path)
      {path, :removal} -> Cloak.DataSource.SerializingUpdater.process_removal(path)
    end)


  # -------------------------------------------------------------------
  # Supervison tree callback
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_options \\ []) do
    ChildSpec.supervisor(child_specs(), strategy: :one_for_all, name: __MODULE__.Supervisor)
  end

  defp child_specs() do
    if configured_with_individual_configurations?() do
      [
        %{
          id: FileSystem,
          start: {FileSystem, :start_link, [[dirs: [config_path()], name: @file_system_monitor_name]]},
        },
        ChildSpec.gen_server(__MODULE__, [], name: __MODULE__),
      ]
    else
      []
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
end
