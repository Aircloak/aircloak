defmodule Cloak.AirSocket do
  @moduledoc """
  Client side of the socket connection to the Air system.

  This module will connect to the Air system. The connection parameters are
  hardcoded in the configuration file. If the connection can't be established,
  the process will attempt to reconnect in regular intervals specified in the
  configuration file.
  """
  require Logger
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

  @doc """
  Sends task results to the Air.

  The function returns when the Air responds. If the timeout occurs, it is
  still possible that the Air has received the request.
  """
  @spec send_task_results(any) :: :ok | {:error, any}
  def send_task_results(task_results) do
    case call("task_results", task_results, :timer.seconds(5)) do
      "ok" -> :ok
      other -> {:error, other}
    end
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
    {:connect, url, %{pending_calls: %{}}}
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
  def handle_message("main", "air_call", request, transport, state) do
    handle_air_call(request["event"], request["payload"], {transport, request["request_id"]}, state)
  end
  def handle_message("main", "call_response", payload, _transport, state) do
    request_id = payload["request_id"]
    case Map.fetch(state.pending_calls, request_id) do
      {:ok, request_data} ->
        Process.cancel_timer(request_data.timeout_ref)
        respond_to_internal_request(request_data.from, payload["response"])
      :error ->
        Logger.warn("unknown sync call response: #{inspect payload}")
    end
    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
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
  def handle_info({{__MODULE__, :call}, timeout, from, event, payload}, transport, state) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    GenSocketClient.push(transport, "main", "cloak_call",
        %{request_id: request_id, event: event, payload: payload})
    timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)
    {:ok, put_in(state.pending_calls[request_id], %{from: from, timeout_ref: timeout_ref})}
  end
  def handle_info({:call_timeout, request_id}, _transport, state) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout")
    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
  end
  def handle_info(message, _transport, state) do
    Logger.warn("unhandled message #{inspect message}")
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Handling air sync calls
  # -------------------------------------------------------------------

  defp handle_air_call("run_task", task, from, state) do
    Logger.info("starting task #{task["id"]}")
    respond_to_air(from, %{status: "started"})
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp respond_to_air({transport, request_id}, response) do
    GenSocketClient.push(transport, "main", "call_response", %{request_id: request_id, response: response})
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  defp call(event, payload, timeout) do
    mref = Process.monitor(__MODULE__)
    send(__MODULE__, {{__MODULE__, :call}, timeout, {self(), mref}, event, payload})
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
