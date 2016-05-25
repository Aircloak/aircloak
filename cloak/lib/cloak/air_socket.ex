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
  @spec start_link(String.t, GenServer.options) :: GenServer.on_start
  def start_link(cloak_name \\ cloak_name(), gen_server_opts \\ [name: __MODULE__]) do
    GenSocketClient.start_link(
      __MODULE__,
      GenSocketClient.Transport.WebSocketClient,
      cloak_name,
      [
        serializer: :cloak_conf.get_val(:air, :serializer),
        transport_opts: [
          keepalive: :timer.seconds(30)
        ]
      ],
      gen_server_opts
    )
  end

  @doc """
  Sends a query result to the Air.

  The function returns when the Air responds. If the timeout occurs, it is
  still possible that the Air has received the request.
  """
  @spec send_query_result(GenServer.server, %{}) :: :ok | {:error, any}
  def send_query_result(socket \\ __MODULE__, result) do
    Logger.info("sending result for query #{result.query_id} to Air")
    case call(socket, "query_result", result, :timer.seconds(5)) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @doc "Sends query progress report to the Air."
  @spec send_query_progress_report(GenServer.server, %{}) :: :ok
  def send_query_progress_report(_socket \\ __MODULE__, progress_report),
    do: Logger.info("new query progress report: #{inspect progress_report}")


  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(cloak_name) do
    params = %{
      cloak_name: cloak_name
    }
    url = "#{:cloak_conf.get_val(:air, :socket_url)}?#{URI.encode_query(params)}"
    initial_interval = :cloak_conf.get_val(:air, :min_reconnect_interval)
    state = %{
      cloak_name: cloak_name,
      pending_calls: %{},
      reconnect_interval: initial_interval,
      rejoin_interval: initial_interval
    }
    {:connect, url, state}
  end

  @doc false
  def handle_connected(_transport, state) do
    Logger.info("connected")
    send(self(), {:join, "main"})
    initial_interval = :cloak_conf.get_val(:air, :min_reconnect_interval)
    {:ok, %{state | reconnect_interval: initial_interval}}
  end

  @doc false
  def handle_disconnected(reason, %{reconnect_interval: interval} = state) do
    Logger.error("disconnected: #{inspect reason}")
    Process.send_after(self(), :connect, interval)
    {:ok, %{state | reconnect_interval: next_interval(interval)}}
  end

  @doc false
  def handle_joined(topic, _payload, _transport, state) do
    Logger.info("joined the topic #{topic}")
    initial_interval = :cloak_conf.get_val(:air, :min_reconnect_interval)
    {:ok, %{state | rejoin_interval: initial_interval}}
  end

  @doc false
  def handle_join_error(topic, payload, _transport, state) do
    Logger.error("join error on the topic #{topic}: #{inspect payload}")
    {:ok, state}
  end

  @doc false
  def handle_channel_closed(topic, payload, _transport, %{rejoin_interval: interval} = state) do
    Logger.error("disconnected from the topic #{topic}: #{inspect payload}")
    Process.send_after(self(), {:join, topic}, interval)
    {:ok, %{state | rejoin_interval: next_interval(interval)}}
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
        response = case payload["status"] do
          "ok" -> {:ok, payload["result"]}
          "error" -> {:error, payload["result"]}
          _other -> {:error, {:invalid_status, payload}}
        end
        respond_to_internal_request(request_data.from, response)
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
    case GenSocketClient.join(transport, topic, get_join_info(state.cloak_name)) do
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

  defp handle_air_call("run_query", query, from, state) do
    query_id = Map.fetch!(query, "id")
    Logger.info("starting query #{query_id}")
    Cloak.Query.start(%Cloak.Query{id: query_id, query: Map.fetch!(query, "query")})
    respond_to_air(from, :ok)
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @spec respond_to_air({GenSocketClient.transport, request_id::String.t}, :ok | :error, any) ::
      :ok | {:error, any}
  defp respond_to_air({transport, request_id}, status, result \\ nil) do
    case GenSocketClient.push(transport, "main", "call_response", %{
          request_id: request_id,
          status: status,
          result: result
        }) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  @spec call(GenServer.server, String.t, %{}, pos_integer) :: {:ok, any} | {:error, any}
  defp call(socket, event, payload, timeout) do
    mref = Process.monitor(socket)
    send(socket, {{__MODULE__, :call}, timeout, {self(), mref}, event, payload})
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

  defp get_join_info(cloak_name) do
    data_sources = for data_source <- Cloak.DataSource.all do
      tables = for table <- Cloak.DataSource.tables(data_source) do
        columns = for {name, type} <- Cloak.DataSource.columns(data_source, table) do
          %{name: name, type: type}
        end
        %{id: table, columns: columns}
      end
      %{id: data_source, tables: tables}
    end
    %{name: cloak_name, data_sources: data_sources}
  end

  defp next_interval(current_interval) do
    max_interval = :cloak_conf.get_val(:air, :max_reconnect_interval)
    min(current_interval * 2, max_interval)
  end
end
