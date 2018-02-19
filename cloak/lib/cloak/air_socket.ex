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
      {air_socket_url(), Enum.to_list(cloak_params)},
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
    call_air(socket, "main", "query_result", result, :timer.minutes(1))
    Logger.info("query result sent", query_id: result.query_id)
  catch :exit, {:timeout, _} ->
    {:error, :timeout}
  end

  @doc """
  Sends a query status update to the Air.

  The function returns when the Air responds. If the timeout occurs, it is
  still possible that the Air has received the request.
  """
  @spec send_query_state(GenServer.server, String.t, atom) :: :ok | {:error, any}
  def send_query_state(socket \\ __MODULE__, query_id, query_state), do:
    cast_air(socket, "main", "query_state", %{query_id: query_id, query_state: query_state})

  @doc "Sends cloak memory stats to the air."
  @spec send_memory_stats(GenServer.server, Keyword.t) :: :ok | {:error, any}
  def send_memory_stats(socket \\ __MODULE__, memory_reading), do:
    cast_air(socket, "main", "memory_reading", memory_reading |> Enum.into(%{}))

  @doc "Updates the configuration of the current cloak in the air."
  @spec update_config(GenServer.server, DataSource.t) :: :ok | {:error, any}
  def update_config(socket \\ __MODULE__, data_sources), do:
    cast_air(socket, "main", "update_config", get_join_info(data_sources))


  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @impl GenSocketClient
  def init({air_socket_url, params}) do
    Process.flag(:trap_exit, true)
    initial_interval = config(:min_reconnect_interval)

    state = %{
      pending_calls: %{},
      reconnect_interval: initial_interval,
      rejoin_interval: initial_interval,
      air_call_async_handlers: %{},
    }

    {:connect, air_socket_url, params, state}
  end

  @impl GenSocketClient
  def handle_connected(_transport, state) do
    Logger.info("connected")
    send(self(), {:join, "main"})
    initial_interval = config(:min_reconnect_interval)
    {:ok, %{state | reconnect_interval: initial_interval}}
  end

  @impl GenSocketClient
  def handle_disconnected(reason, %{reconnect_interval: interval} = state) do
    log_disconnected(reason)
    Process.send_after(self(), :connect, interval)
    {:ok, %{state | reconnect_interval: next_interval(interval)}}
  end

  @impl GenSocketClient
  def handle_joined(topic, _payload, _transport, state) do
    Logger.info("joined the topic #{topic}")
    initial_interval = config(:min_reconnect_interval)
    {:ok, %{state | rejoin_interval: initial_interval}}
  end

  @impl GenSocketClient
  def handle_join_error(topic, payload, _transport, state) do
    Logger.error("join error on the topic #{topic}: #{inspect payload}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_channel_closed(topic, payload, _transport, %{rejoin_interval: interval} = state) do
    Logger.error("disconnected from the topic #{topic}: #{inspect payload}")
    Process.send_after(self(), {:join, topic}, interval)
    {:ok, %{state | rejoin_interval: next_interval(interval)}}
  end

  @impl GenSocketClient
  def handle_message("main", "air_call", request, transport, state) do
    handle_air_call(request.event, request.payload, {transport, request.request_id}, state)
  end
  def handle_message("main", "air_response", payload, _transport, state) do
    request_id = payload.request_id
    case Map.fetch(state.pending_calls, request_id) do
      {:ok, request_data} ->
        Process.cancel_timer(request_data.timeout_ref)
        response = case payload.status do
          :ok -> {:ok, payload.result}
          :error -> {:error, payload.result}
          _other -> {:error, {:invalid_status, payload}}
        end
        GenSocketClient.reply(request_data.from, response)
      :error ->
        Logger.warn("unknown sync call response: #{inspect payload}")
    end
    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
  end
  def handle_message(topic, event, payload, _transport, state) do
    Logger.warn("unhandled message on topic #{topic}: #{event} #{inspect payload}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_reply(topic, _ref, payload, _transport, state) do
    Logger.warn("unhandled reply on topic #{topic}: #{inspect payload}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_info(:connect, _transport, state) do
    log_connect()
    {:connect, state}
  end
  def handle_info({:join, topic}, transport, state) do
    case GenSocketClient.join(transport, topic, get_join_info(Cloak.DataSource.all())) do
      {:error, reason} ->
        Logger.error("error joining the topic #{topic}: #{inspect reason}")
        Process.send_after(self(), {:join, topic}, config(:rejoin_interval))
      {:ok, _ref} -> :ok
    end
    {:ok, state}
  end
  def handle_info({{__MODULE__, :cast_air}, topic, event, payload}, transport, state) do
    push(transport, topic, event, payload)
    {:ok, state}
  end
  def handle_info({:call_timeout, request_id}, _transport, state) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout")
    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
  end
  def handle_info({ref, {:async_air_call_result, result}}, _transport, state) do
    state.air_call_async_handlers
    |> Map.fetch!(ref)
    |> respond_to_air(:ok, result)

    {:ok, state}
  end
  def handle_info({:DOWN, ref, :process, _pid, reason} = down_message, _transport, state) do
    case Map.fetch(state.air_call_async_handlers, ref) do
      {:ok, from} ->
        if reason != :normal, do: respond_to_air(from, :error, "Unknown cloak error")
        {:ok, update_in(state.air_call_async_handlers, &Map.delete(&1, ref))}

      :error ->
        Logger.warn("unknown :DOWN message #{inspect(down_message)}")
        {:ok, state}
    end
  end
  def handle_info({:EXIT, _pid, _reason}, _transport, state) do
    # Ignoring :EXIT messages which can be caused by the transport process or by async air call handlers
    {:ok, state}
  end
  def handle_info(message, _transport, state) do
    Logger.warn("unhandled message #{inspect(message)}")
    {:ok, state}
  end


  @impl GenSocketClient
  def handle_call({:call_air, request_id, topic, event, payload, timeout}, from, transport, state) do
    case push(transport, topic, "cloak_call", %{request_id: request_id, event: event, payload: payload}) do
      :ok ->
        timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)
        {:noreply, put_in(state.pending_calls[request_id], %{from: from, timeout_ref: timeout_ref})}
      {:error, error} ->
        {:reply, {:error, error}, state}
    end
  end


  # -------------------------------------------------------------------
  # Handling air sync calls
  # -------------------------------------------------------------------

  defp handle_air_call("run_query", serialized_query, from, state) do
    case Cloak.DataSource.fetch(serialized_query.data_source) do
      :error ->
        respond_to_air(from, :error, "Unknown data source.")

      {:ok, data_source} ->
        Logger.info("starting query", query_id: serialized_query.id)
        Cloak.Query.Runner.start(
          serialized_query.id,
          data_source,
          serialized_query.statement || "",
          decode_params(serialized_query.parameters),
          serialized_query.views
        )
        respond_to_air(from, :ok)
    end
    {:ok, state}
  end
  defp handle_air_call("describe_query", serialized_query, from, state) do
    case Cloak.DataSource.fetch(serialized_query.data_source) do
      :error ->
        respond_to_air(from, :error, "Unknown data source.")

      {:ok, data_source} ->
        case Cloak.Sql.Query.describe_query(
          data_source,
          serialized_query.statement || "",
          decode_params(serialized_query.parameters),
          serialized_query.views
        ) do
          {:ok, columns, features} -> respond_to_air(from, :ok, %{columns: columns, features: features})
          {:error, reason} -> respond_to_air(from, :ok, %{error: reason})
        end
    end
    {:ok, state}
  end
  defp handle_air_call("validate_views", serialized_view, from, state) do
    case Cloak.DataSource.fetch(serialized_view.data_source) do
      :error -> respond_to_air(from, :error, "Unknown data source.")
      {:ok, data_source} -> respond_to_air(from, :ok, validate_views(data_source, serialized_view.views))
    end

    {:ok, state}
  end
  defp handle_air_call("stop_query", query_id, from, state) do
    Logger.info("stopping query ...", query_id: query_id)
    Cloak.Query.Runner.stop(query_id, :cancelled)
    respond_to_air(from, :ok)
    {:ok, state}
  end
  defp handle_air_call("running_queries", _, from, state) do
    respond_to_air(from, :ok, Cloak.Query.Runner.running_queries())
    {:ok, state}
  end
  defp handle_air_call("performance", _, from, state) do
    task = start_async_air_call_handler(&Cloak.Performance.run/0)
    {:ok, put_in(state.air_call_async_handlers[task.ref], from)}
  end

  defp start_async_air_call_handler(fun), do: Task.async(fn -> {:async_air_call_result, fun.()} end)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp push(transport, topic, event, payload) do
    GenSocketClient.push(transport, topic, event, payload)
    :ok
  rescue
    error in Poison.EncodeError ->
      error =
        if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
          Poison.EncodeError.exception(message: "Poison encode error", value: "`sanitized`")
        else
          error
        end

      Logger.error("Message could not be encoded: #{Exception.message(error)}")
      {:error, error}
  end

  defp decode_params(params), do: :erlang.binary_to_term(Base.decode16!(params))

  defp air_socket_url() do
    full_path =
      case Aircloak.DeployConfig.fetch("air_site") do
        {:ok, air_site} -> air_site
        :error -> raise "The air_site is not configured in config.json. Please consult the " <>
          "\"Configuring the system\" section of the User Guide."
      end

    full_path
    |> URI.parse()
    |> Map.put(:path, "/cloak/socket/websocket")
    |> URI.to_string()
  end

  @spec respond_to_air({GenSocketClient.transport, request_id::String.t}, :ok | :error, any) ::
      :ok | {:error, any}
  defp respond_to_air({transport, request_id}, status, result \\ nil) do
    case GenSocketClient.push(transport, "main", "cloak_response", %{
          request_id: request_id,
          status: status,
          result: result
        }) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @spec cast_air(GenServer.server, String.t, String.t, map) :: :ok
  defp cast_air(socket, topic, event, payload) do
    send(socket, {{__MODULE__, :cast_air}, topic, event, payload})
    :ok
  end

  @spec call_air(GenServer.server, String.t, String.t, map, pos_integer) :: :ok | {:error, any}
  defp call_air(socket, topic, event, payload, timeout) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    case GenSocketClient.call(socket, {:call_air, request_id, topic, event, payload, timeout}, timeout) do
      {:ok, _} -> :ok
      error -> error
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

  defp get_join_info(data_sources) do
    data_sources = for data_source <- data_sources do
      tables = for {id, table} <- data_source.tables do
        columns = for column <- table.columns do
          %{name: column.name, type: column.type, user_id: column.name == table.user_id}
        end
        %{id: id, columns: columns}
      end
      %{
        name: data_source.name,
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


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do:
    Supervisor.Spec.worker(__MODULE__, [])
end
