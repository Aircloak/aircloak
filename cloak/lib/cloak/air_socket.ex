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

  @timeout :timer.seconds(5)


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
      [serializer: config(:serializer)],
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
    Logger.info("sending query result to Air", query_id: result.query_id)
    call(socket, "main", "query_result", result)
  end

  @doc """
  Sends a query status update to the Air.

  The function returns when the Air responds. If the timeout occurs, it is
  still possible that the Air has received the request.
  """
  @spec send_query_state(GenServer.server, String.t, atom) :: :ok | {:error, any}
  def send_query_state(socket \\ __MODULE__, query_id, query_state), do:
    call(socket, "main", "query_state", %{query_id: query_id, query_state: query_state})

  @doc "Sends cloak memory stats to the air."
  @spec send_memory_stats(GenServer.server, Keyword.t) :: :ok | {:error, any}
  def send_memory_stats(socket \\ __MODULE__, memory_reading), do:
    cast(socket, "main", "memory_reading", memory_reading |> Enum.into(%{}))


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
  def handle_info({{__MODULE__, :cast}, topic, event, payload}, transport, state) do
    try do
      GenSocketClient.push(transport, topic, event, payload)
      {:ok, state}
    rescue
      error in Poison.EncodeError ->
        error =
          if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
            Poison.EncodeError.exception(message: "Poison encode error", value: "`sanitized`")
          else
            error
          end

        Logger.error("Message could not be encoded: #{Exception.message(error)}")
        {:ok, state}
    end
  end
  def handle_info({{__MODULE__, :call}, topic, timeout, from, event, payload}, transport, state) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    try do
      GenSocketClient.push(transport, topic, "cloak_call", %{request_id: request_id, event: event, payload: payload})
      timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)
      {:ok, put_in(state.pending_calls[request_id], %{from: from, timeout_ref: timeout_ref})}
    rescue
      error in Poison.EncodeError ->
        error =
          if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
            Poison.EncodeError.exception(message: "Poison encode error", value: "`sanitized`")
          else
            error
          end

        Logger.error("Message could not be encoded: #{Exception.message(error)}")
        respond_to_internal_request(from, {:error, error})
        {:ok, state}
    end
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
        Logger.info("starting query", query_id: id)
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
        case Cloak.Sql.Query.describe_query(data_source, statement, parameters, views) do
          {:ok, columns, features} -> respond_to_air(from, :ok, %{columns: columns, features: features})
          {:error, reason} -> respond_to_air(from, :ok, %{error: reason})
        end
    end
    {:ok, state}
  end
  defp handle_air_call("validate_views", serialized_view, from, state) do
    data_source = Map.fetch!(serialized_view, "data_source")
    views = Map.fetch!(serialized_view, "views")

    case Cloak.DataSource.fetch(data_source) do
      :error -> respond_to_air(from, :error, "Unknown data source.")
      {:ok, data_source} -> respond_to_air(from, :ok, validate_views(data_source, views))
    end

    {:ok, state}
  end
  defp handle_air_call("stop_query", query_id, from, state) do
    Logger.info("stopping query ...", query_id: query_id)
    Cloak.Query.Runner.stop(query_id, :cancelled)
    respond_to_air(from, :ok)
    {:ok, state}
  end
  defp handle_air_call("is_alive", query_id, from, state) do
    respond_to_air(from, :ok, Cloak.Query.Runner.alive?(query_id))
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

  @spec cast(GenServer.server, String.t, String.t, map) :: :ok
  defp cast(socket, topic, event, payload) do
    send(socket, {{__MODULE__, :cast}, topic, event, payload})
    :ok
  end

  @spec call(GenServer.server, String.t, String.t, map) :: :ok | {:error, any}
  defp call(socket, topic, event, payload) do
    case do_call(socket, topic, event, payload, @timeout) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  defp do_call(socket, topic, event, payload, timeout) do
    mref = Process.monitor(socket)
    send(socket, {{__MODULE__, :call}, topic, timeout, {self(), mref}, event, payload})
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
    %{cloak_name: cloak_name(), version: Aircloak.Version.for_app(:cloak) |> Aircloak.Version.to_string()}
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
      %{
        name: data_source.name,
        global_id: data_source.global_id,
        tables: tables,
        errors: data_source.errors,
      }
    end
    %{data_sources: data_sources, salt_hash: get_salt_hash()}
  end

  defp next_interval(current_interval) do
    min(current_interval * 2, config(:max_reconnect_interval))
  end

  defp config(key) do
    Application.get_env(:cloak, :air) |> Keyword.fetch!(key)
  end

  defp validate_views(data_source, views) do
    for {name, sql} <- views do
      Task.async(fn() ->
        case Cloak.Sql.Query.validate_view(data_source, name, sql, views) do
          {:ok, columns} -> %{name: name, valid: true, columns: columns}
          {:error, field, reason} -> %{name: name, valid: false, field: field, error: reason}
        end
      end)
    end
    |> Enum.map(&Task.await/1)
  end

  defp get_salt_hash(), do: :crypto.hash(:sha256, Cloak.Query.Anonymizer.config(:salt)) |> Base.encode16()

  if Mix.env == :dev do
    # suppressing of some common log messages in dev env to avoid excessive noise
    defp log_connect(), do: :ok
    defp log_disconnected(_reason), do: :ok
  else
    defp log_connect(), do: Logger.info("connecting")
    defp log_disconnected(reason), do: Logger.error("disconnected: #{inspect reason}")
  end
end
