defmodule Cloak.AirSocket do
  @moduledoc """
  Client side of the socket connection to the Air system.

  This module will connect to the Air system. The connection parameters are
  hardcoded in the configuration file. If the connection can't be established,
  the process will attempt to reconnect in regular intervals specified in the
  configuration file.
  """
  require Logger
  require Aircloak.SyncRequester
  alias Aircloak.SyncRequester
  alias Phoenix.Channels.GenSocketClient
  @behaviour GenSocketClient


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the socket client."
  @spec start_link() :: GenServer.on_start
  def start_link() do
    GenSocketClient.start_link(
          __MODULE__,
          GenSocketClient.Transport.WebSocketClient,
          nil,
          [
            serializer: GenSocketClient.Serializer.GzipJson,
            transport_opts: [
              keepalive: :timer.seconds(30)
            ]
          ],
          name: __MODULE__
        )
  end

  @doc "Sends task results to the Air."
  @spec send_task_results(any) :: :ok
  def send_task_results(task_results),
    do: call({:send_task_results, task_results})

  @doc "Sends task progress report to the Air."
  @spec send_task_progress_report(any) :: :ok
  def send_task_progress_report(progress_report), do: Logger.info("new task progress report: #{progress_report}")


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
  def handle_message(topic, SyncRequester.request_event(command), payload, transport, state) do
    {request_ref, request} = SyncRequester.decode_request!(payload)
    handle_sync_request(topic, command, request, request_ref, transport, state)
  end
  def handle_message(topic, SyncRequester.response_event(command), payload, transport, state) do
    case SyncRequester.decode_response!(SyncRequester.Backend.Ets, __MODULE__, payload) do
      {:matched, {request, request_meta, response}} ->
        handle_sync_response(topic, command, response, request, request_meta, transport, state)
      {:not_matched, response} ->
        Logger.warn("unmatched response #{inspect response}")
        {:ok, state}
    end
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
  def handle_info({{__MODULE__, :call}, from, message}, transport, state),
    do: handle_call(message, from, transport, state)
  def handle_info(message, _transport, state) do
    Logger.warn("unhandled message #{inspect message}")
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Handling internal requests
  # -------------------------------------------------------------------

  defp handle_call({:send_task_results, task_results}, from, transport, state) do
    Logger.info("sending task results to Air")
    payload = SyncRequester.encode_request(SyncRequester.Backend.Ets, __MODULE__, task_results)
    GenSocketClient.push(transport, "main", SyncRequester.request_event("task_results"), payload)
    respond_to_internal_request(from, :ok)
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Handling sync requests from Air
  # -------------------------------------------------------------------

  defp handle_sync_request("main", "run_task", task, request_ref, transport, state) do
    Logger.info("starting task #{task.id}")
    response =
      try do
          :ok = task |> :task_parser.parse |> :progress_handler.register_task |> :task_coordinator.run_task
          :started
        rescue
          error ->
            Logger.error("error starting task #{task.id}: #{inspect error}\n#{inspect System.stacktrace}")
            :error
      end
    payload = SyncRequester.encode_response(request_ref, response)
    GenSocketClient.push(transport, "main", SyncRequester.response_event("run_task"), payload)
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Handling sync responses from Air
  # -------------------------------------------------------------------

  def handle_sync_response("main", "task_results", status, _request, _request_meta, _transport, state) do
    Logger.info("task results sent to air: #{inspect status}")
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  defp call(message, timeout \\ :timer.seconds(5)) do
    mref = Process.monitor(__MODULE__)
    send(__MODULE__, {{__MODULE__, :call}, {self(), mref}, message})
    receive do
      {^mref, response} ->
        Process.demonitor(mref, [:flush])
        response
      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)
    after timeout ->
      exit(:timeout)
    end
  end

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
