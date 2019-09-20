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
  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Processes a change event to a data source definition file"
  @spec handle_events([{String.t(), :update | :removal}]) :: :ok
  def handle_events(events) do
    if Enum.any?(events, &match?({_path, :removal}, &1)) do
      GenServer.cast(__MODULE__, :reinitialize)
    else
      Enum.each(events, fn {path, :update} -> GenServer.cast(__MODULE__, {:process_update, path}) end)
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_cast(:run_liveness_check, state) do
    DataSource.perform_data_source_availability_checks()
    {:noreply, state, :hibernate}
  end

  def handle_cast({:process_update, file_path}, state) do
    Logger.debug(fn -> "Reloading data source configuration at #{file_path}." end)
    DataSource.initialize_data_source_from_path(file_path)
    {:noreply, state, :hibernate}
  end

  def handle_cast(:reinitialize, state) do
    Logger.debug(fn -> "Data source removal detected. Reloading all data source configurations." end)
    DataSource.reinitialize_all_data_sources()
    {:noreply, state, :hibernate}
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
     every: liveness_check_interval()}
  end

  defp liveness_check_interval(),
    do:
      :timer.minutes(
        Aircloak.DeployConfig.get(
          "liveness_check_interval",
          Aircloak.in_env(dev: 60, else: 5)
        )
      )
end
