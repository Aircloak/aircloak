defmodule Cloak.AirSocket do
  @moduledoc """
  Client side of the socket connection to the Air system.

  This module will connect to the Air system. The connection parameters are
  hardcoded in the configuration file. If the connection can't be established,
  the process will attempt to reconnect in regular intervals specified in the
  configuration file.
  """

  require Logger
  require Aircloak.DeployConfig
  alias Phoenix.Channels.GenSocketClient

  @behaviour GenSocketClient


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the socket client."
  @spec start_link(map, GenServer.options) :: GenServer.on_start
  def start_link(cloak_params \\ cloak_params(), gen_server_opts \\ [name: __MODULE__]) do
    GenSocketClient.start_link(
      __MODULE__,
      GenSocketClient.Transport.WebSocketClient,
      air_socket_url(cloak_params),
      [
        serializer: config(:serializer),
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
  @spec send_query_result(GenServer.server, map) :: :ok | {:error, any}
  def send_query_result(socket \\ __MODULE__, result) do
    Logger.info("sending result for query #{result.query_id} to Air")
    case call(socket, "query_result", result, :timer.seconds(5)) do
      {:ok, _} -> :ok
      error -> error
    end
  end


  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(air_socket_url) do
    initial_interval = config(:min_reconnect_interval)
    state = %{
      pending_calls: %{},
      reconnect_interval: initial_interval,
      rejoin_interval: initial_interval
    }
    {:connect, air_socket_url, state}
  end

  @doc false
  def handle_connected(_transport, state) do
    Logger.info("connected")
    send(self(), {:join, "main"})
    initial_interval = config(:min_reconnect_interval)
    {:ok, %{state | reconnect_interval: initial_interval}}
  end

  @doc false
  def handle_disconnected(reason, %{reconnect_interval: interval} = state) do
    log_disconnected(reason)
    Process.send_after(self(), :connect, interval)
    {:ok, %{state | reconnect_interval: next_interval(interval)}}
  end

  @doc false
  def handle_joined(topic, _payload, _transport, state) do
    Logger.info("joined the topic #{topic}")
    initial_interval = config(:min_reconnect_interval)
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
    log_connect()
    {:connect, state}
  end
  def handle_info({:join, topic}, transport, state) do
    case GenSocketClient.join(transport, topic, get_join_info()) do
      {:error, reason} ->
        Logger.error("error joining the topic #{topic}: #{inspect reason}")
        Process.send_after(self(), {:join, topic}, config(:rejoin_interval))
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

  defp handle_air_call("run_query", serialized_query, from, state) do
    %{"id" => id, "statement" => statement, "data_source" => data_source} = serialized_query
    parameters = decode_params(Map.fetch!(serialized_query, "parameters"))
    views = Map.fetch!(serialized_query, "views")
    case Cloak.DataSource.fetch(data_source) do
      :error ->
        respond_to_air(from, :error, "Unknown data source.")

      {:ok, data_source} ->
        Logger.info("starting query #{id} ...")
        Cloak.Query.Runner.start(id, data_source, statement, parameters, views)
        respond_to_air(from, :ok)
    end
    {:ok, state}
  end
  defp handle_air_call("describe_query", serialized_query, from, state) do
    %{"statement" => statement, "data_source" => data_source} = serialized_query
    parameters = decode_params(Map.fetch!(serialized_query, "parameters"))
    views = Map.fetch!(serialized_query, "views")
    case Cloak.DataSource.fetch(data_source) do
      :error ->
        respond_to_air(from, :error, "Unknown data source.")

      {:ok, data_source} ->
        case Cloak.Aql.Query.describe_query(data_source, statement, parameters, views) do
          {:ok, columns, features} -> respond_to_air(from, :ok, %{columns: columns, features: features})
          {:error, reason} -> respond_to_air(from, :ok, %{error: reason})
        end
    end
    {:ok, state}
  end
  defp handle_air_call("validate_view", serialized_view, from, state) do
    data_source = Map.fetch!(serialized_view, "data_source")
    name = Map.fetch!(serialized_view, "name")
    sql = Map.fetch!(serialized_view, "sql")
    views = Map.fetch!(serialized_view, "views")

    case Cloak.DataSource.fetch(data_source) do
      :error ->
        respond_to_air(from, :error, "Unknown data source.")

      {:ok, data_source} ->
        case Cloak.Aql.Query.validate_view(data_source, name, sql, views) do
          {:ok, columns} -> respond_to_air(from, :ok, %{valid: true, columns: columns})
          {:error, field, reason} -> respond_to_air(from, :ok, %{valid: false, field: field, error: reason})
        end
    end
    {:ok, state}
  end
  defp handle_air_call("stop_query", query_id, from, state) do
    Logger.info("stopping query #{query_id} ...")
    Cloak.Query.Runner.stop(query_id)
    respond_to_air(from, :ok)
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp decode_params(params), do: :erlang.binary_to_term(Base.decode16!(params))

  defp air_socket_url(cloak_params) do
    # deploy specific configuration takes precedence over OTP app configuration
    full_path =
      case Aircloak.DeployConfig.fetch("air_site") do
        {:ok, air_site} -> air_site
        :error -> config(:air_site)
      end

    full_path
    |> URI.parse()
    |> Map.put(:path, "/cloak/socket/websocket")
    |> Map.put(:query, URI.encode_query(cloak_params))
    |> URI.to_string()
  end

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

  @spec call(GenServer.server, String.t, map, pos_integer) :: {:ok, any} | {:error, any}
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

  defp cloak_params() do
    %{cloak_name: cloak_name()}
  end

  defp cloak_name() do
    vm_short_name =
      Node.self()
      |> Atom.to_string()
      |> String.split("@")
      |> hd()
    {:ok, hostname} = :inet.gethostname()

    "#{vm_short_name}@#{hostname}"
  end

  defp get_join_info() do
    data_sources = for data_source <- Cloak.DataSource.all() do
      tables = for {id, table} <- data_source.tables do
        columns = for {name, type} <- table.columns do
          %{name: name, type: type, user_id: name == table.user_id}
        end
        %{id: id, columns: columns}
      end
      %{global_id: data_source.global_id, tables: tables}
    end
    %{data_sources: data_sources}
  end

  defp next_interval(current_interval) do
    min(current_interval * 2, config(:max_reconnect_interval))
  end

  defp config(key) do
    Application.get_env(:cloak, :air) |> Keyword.fetch!(key)
  end

  if Mix.env == :dev do
    # suppressing of some common log messages in dev env to avoid excessive noise
    defp log_connect(), do: :ok
    defp log_disconnected(_reason), do: :ok
  else
    defp log_connect(), do: Logger.info("connecting")
    defp log_disconnected(reason), do: Logger.error("disconnected: #{inspect reason}")
  end
end
