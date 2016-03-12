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
  @registration_expiry_sec 10


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the process which periodically registers the k-v pair to etcd.
  """
  @spec start_link(key :: String.t, data :: String.t) :: {:ok, pid} | {:error, term}
  def start_link(key, data),
    do: GenServer.start_link(__MODULE__, {key, data})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(registration_data) do
    Process.flag(:trap_exit, true)
    {:ok, renew_registration({registration_data, false}), @renew_interval}
  end

  @doc false
  def handle_info(:timeout, state),
    do: {:noreply, renew_registration(state), @renew_interval}

  @doc false
  def terminate(reason, {{key, _data}, _previously_registered}) do
    Logger.info(fn -> "Deregistering #{key} due to #{inspect reason}" end)
    :air_etcd.delete(key)
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp renew_registration({{key, data}, previously_registered}) do
    case :air_etcd.set(key, data, @registration_expiry_sec) do
      {:ok, etcd_set()} ->
        unless previously_registered, do: Logger.info(fn -> "Registered #{key}" end)
        {{key, data}, true}
      error ->
        Logger.error(fn -> "Error registering the service #{key}: #{inspect error}" end)
        {{key, data}, false}
    end
  end
end
