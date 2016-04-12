defmodule Cloak.AirSocket do
  @moduledoc """
  Client side of the socket connection to the Air system.

  This module will connect to the Air system. The connection parameters are
  hardcoded in the configuration file. If the connection can't be established,
  the process will attempt to reconnect in regular intervals specified in the
  configuration file.
  """
  require Logger
  require Air.SyncRequester
  alias Phoenix.Channels.GenSocketClient
  @behaviour GenSocketClient


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the socket client."
  @spec start_link(GenServer.options) :: GenServer.on_start
  def start_link(gen_server_opts \\ []) do
    GenSocketClient.start_link(
          __MODULE__,
          GenSocketClient.Transport.WebSocketClient,
          nil,
          [serializer: GenSocketClient.Serializer.GzipJson],
          gen_server_opts
        )
  end


  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    params = %{
      cloak_name: cloak_name()
    }
    url = "#{:cloak_conf.get_val(:air, :socket_url)}?#{URI.encode_query(params)}"
    {:connect, url, %{}}
  end

  @doc false
  def handle_connected(_transport, state) do
    Logger.info("connected")
    send(self(), {:join, "main"})
    {:ok, state}
  end

  @doc false
  def handle_disconnected(reason, state) do
    Logger.error("disconnected: #{inspect reason}")
    Process.send_after(self(), :connect, :cloak_conf.get_val(:air, :reconnect_interval))
    {:ok, state}
  end

  @doc false
  def handle_joined(topic, _payload, _transport, state) do
    Logger.info("joined the topic #{topic}")
    {:ok, state}
  end

  @doc false
  def handle_join_error(topic, payload, _transport, state) do
    Logger.error("join error on the topic #{topic}: #{inspect payload}")
    {:ok, state}
  end

  @doc false
  def handle_channel_closed(topic, payload, _transport, state) do
    Logger.error("disconnected from the topic #{topic}: #{inspect payload}")
    Process.send_after(self(), {:join, topic}, :cloak_conf.get_val(:air, :rejoin_interval))
    {:ok, state}
  end

  @doc false
  def handle_message(topic, Air.SyncRequester.request_event(command), payload, transport, state) do
    {request_ref, request} = Air.SyncRequester.decode_request!(payload)
    handle_sync_request(topic, command, request, request_ref, transport)
    {:ok, state}
  end
  def handle_message(topic, event, payload, _transport, state) do
    Logger.warn("unhandled message on topic #{topic}: #{event} #{inspect payload}")
    {:ok, state}
  end

  @doc false
  def handle_reply(topic, _ref, payload, _transport, state) do
    Logger.warn("unhandled reply on topic #{topic}: #{inspect payload}")
    {:ok, state}
  end

  @doc false
  def handle_info(:connect, _transport, state) do
    Logger.info("connecting")
    {:connect, state}
  end
  def handle_info({:join, topic}, transport, state) do
    case GenSocketClient.join(transport, topic, get_join_info()) do
      {:error, reason} ->
        Logger.error("error joining the topic #{topic}: #{inspect reason}")
        Process.send_after(self(), {:join, topic}, :cloak_conf.get_val(:air, :rejoin_interval))
      {:ok, _ref} -> :ok
    end
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Handling sync requests from Air
  # -------------------------------------------------------------------

  defp handle_sync_request("main", "run_task", task, request_ref, transport) do
    Logger.info("starting task #{task.id}")
    response = :started
    payload = Air.SyncRequester.encode_response(request_ref, response)
    GenSocketClient.push(transport, "main", Air.SyncRequester.response_event("run_task"), payload)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp cloak_name(), do: Node.self() |> Atom.to_string()

  defp get_join_info() do
    data_sources = for data_source <- Cloak.DataSource.all do
      tables = for table <- Cloak.DataSource.tables(data_source) do
        columns = for {name, type} <- Cloak.DataSource.columns(data_source, table) do
          %{name: name, type: type}
        end
        %{id: table, columns: columns}
      end
      %{id: data_source, tables: tables}
    end
    %{name: cloak_name(), data_sources: data_sources}
  end
end
