defmodule Cloak.AirSocket do
  @moduledoc """
  Client side of the socket connection to the Air system.

  This module will connect to the Air system. The connection parameters are
  hardcoded in the configuration file. If the connection can't be established,
  the process will attempt to reconnect in regular intervals specified in the
  configuration file.
  """
  require Logger
  alias Channels.Client.Socket
  @behaviour Socket


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the socket client."
  @spec start_link(GenServer.options) :: GenServer.on_start
  def start_link(gen_server_opts \\ []) do
    Socket.start_link(
          __MODULE__,
          nil,
          [serializer: Channels.Client.Socket.Serializer.GzipJson],
          gen_server_opts
        )
  end


  # -------------------------------------------------------------------
  # Channels.Client.Socket callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    params = %{
      cloak_token: "cloak_token"
    }
    url = "#{:cloak_conf.get_val(:air, :socket_url)}?#{URI.encode_query(params)}"
    send(self(), :connect)
    {:ok, url, %{}}
  end

  @doc false
  def handle_connected(_transport, state) do
    Logger.info(fn -> "connected" end)
    send(self(), {:join, "main"})
    {:ok, state}
  end

  @doc false
  def handle_disconnected(reason, state) do
    Logger.error(fn -> "disconnected: #{inspect reason}" end)
    Process.send_after(self(), :connect, :cloak_conf.get_val(:air, :reconnect_interval))
    {:ok, state}
  end

  @doc false
  def handle_joined(topic, _payload, _transport, state) do
    Logger.info(fn -> "joined the topic #{topic}" end)
    {:ok, state}
  end

  @doc false
  def handle_join_error(topic, payload, _transport, state) do
    Logger.error(fn -> "join error on the topic #{topic}: #{inspect payload}" end)
    {:ok, state}
  end

  @doc false
  def handle_channel_closed(topic, payload, _transport, state) do
    Logger.error(fn -> "disconnected from the topic #{topic}: #{inspect payload}" end)
    Process.send_after(self(), {:join, topic}, :cloak_conf.get_val(:air, :rejoin_interval))
    {:ok, state}
  end

  @doc false
  def handle_message(topic, event, payload, _transport, state) do
    Logger.warn(fn -> "unhandled message on topic #{topic}: #{event} #{inspect payload}" end)
    {:ok, state}
  end

  @doc false
  def handle_reply(topic, _ref, payload, _transport, state) do
    Logger.warn(fn -> "unhandled reply on topic #{topic}: #{inspect payload}" end)
    {:ok, state}
  end

  @doc false
  def handle_info(:connect, _transport, state) do
    Logger.info(fn -> "connecting" end)
    {:connect, state}
  end
  def handle_info({:join, topic}, transport, state) do
    case Socket.join(transport, topic) do
      {:error, reason} ->
        Logger.error(fn -> "error joining the topic #{topic}: #{inspect reason}" end)
        Process.send_after(self(), {:join, topic}, :cloak_conf.get_val(:air, :rejoin_interval))
      {:ok, _ref} -> :ok
    end

    {:ok, state}
  end
end
