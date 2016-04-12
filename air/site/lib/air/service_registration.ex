defmodule Air.ServiceRegistration do
  @moduledoc """
  In charge of renewing the service registration in etcd.

  This module will start a dedicated process and periodically insert provided
  registration data.
  """

  use GenServer
  require Logger
  require Record

  Record.defrecordp :etcd_set, :set, Record.extract(:set, from_lib: "etcd/include/etcd_types.hrl")

  @renew_interval :timer.seconds(10)
  @registration_expiry_sec 30

  @type options :: [
    crash_on_error: boolean,
    renew_interval: pos_integer,
    registration_expiry_sec: pos_integer,
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the process which periodically registers the k-v pair to etcd.
  """
  @spec start_link(key :: String.t, data :: String.t, options) :: {:ok, pid} | {:error, term}
  def start_link(key, data, options \\ []),
    do: GenServer.start_link(__MODULE__, {key, data, options})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({key, data, options}) do
    Process.flag(:trap_exit, true)
    state = %{
      key: key,
      data: data,
      crash_on_error: Keyword.get(options, :crash_on_error, false),
      renew_interval: Keyword.get(options, :renew_interval, @renew_interval),
      registration_expiry_sec: Keyword.get(options, :registration_expiry_sec, @registration_expiry_sec),
      previously_registered: false
    }
    {:ok, renew_registration(state), @renew_interval}
  end

  @doc false
  def handle_info(:timeout, state),
    do: {:noreply, renew_registration(state), @renew_interval}

  @doc false
  def terminate(reason, state) do
    Logger.info("Deregistering #{state.key} due to #{inspect reason}")
    :air_etcd.delete(state.key)
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp renew_registration(state) do
    case :air_etcd.set(state.key, state.data, state.registration_expiry_sec) do
      {:ok, etcd_set()} ->
        unless state.previously_registered, do: Logger.info("Registered #{state.key}")
        %{state | previously_registered: true}
      error ->
        Logger.error("Error registering the service #{state.key}: #{inspect error}")
        if state.crash_on_error, do: exit(:registration_error)
        %{state | previously_registered: false}
    end
  end
end
