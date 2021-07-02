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
  alias Cloak.AirSocket.DataSourceUpdater
  import Aircloak, only: [in_env: 1, unused: 2]

  @behaviour GenSocketClient

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the socket client."
  @spec start_link(map, GenServer.options()) :: GenServer.on_start()
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
  @spec send_query_result(GenServer.server(), map) :: :ok | {:error, any}
  def send_query_result(socket \\ __MODULE__, result) do
    Logger.info("sending query result to Air", query_id: result.query_id)
    call_air(socket, "main", "query_result", result, :timer.minutes(1))
    Logger.info("query result sent", query_id: result.query_id)
  catch
    :exit, {:timeout, _} ->
      {:error, :timeout}
  end

  @doc """
  Sends a query status update to the Air.

  The function returns when the Air responds. If the timeout occurs, it is
  still possible that the Air has received the request.
  """
  @spec send_query_state(GenServer.server(), String.t(), atom) :: :ok | {:error, any}
  def send_query_state(socket \\ __MODULE__, query_id, query_state),
    do: cast_air(socket, "main", "query_state", %{query_id: query_id, query_state: query_state})

  @doc "Sends cloak memory stats to the air."
  @spec send_memory_stats(GenServer.server(), Map.t()) :: :ok | {:error, any}
  def send_memory_stats(socket \\ __MODULE__, memory_reading),
    do: cast_air(socket, "main", "memory_reading", memory_reading |> Enum.into(%{}))

  @doc "Sends update on the state of an analyst table to the air."
  @spec send_analyst_table_state_update(
          GenServer.server(),
          non_neg_integer,
          String.t(),
          String.t(),
          :created | :create_error
        ) :: :ok | {:error, any}
  if Mix.env() == :test do
    def send_analyst_table_state_update(_socket \\ __MODULE__, _analyst_id, _table_name, _data_source_name, _status),
      do: :ignored
  else
    def send_analyst_table_state_update(socket \\ __MODULE__, analyst_id, table_name, data_source_name, status) do
      payload = %{
        analyst_id: analyst_id,
        analyst_table_name: table_name,
        data_source_name: data_source_name,
        status: status
      }

      cast_air(socket, "main", "analyst_table_state_update", payload)
    end
  end

  @doc "Send a cloak log message to the air."
  @spec send_log(GenServer.server(), NaiveDateTime.t(), String.t(), Logger.level(), String.t()) :: :ok | {:error, any}
  def send_log(socket \\ __MODULE__, timestamp, hostname, level, message),
    do: cast_air(socket, "main", "log", %{timestamp: timestamp, hostname: hostname, level: level, message: message})

  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @impl GenSocketClient
  def init({air_socket_url, params}) do
    initial_interval = config(:min_reconnect_interval)

    state = %{
      pending_calls: %{},
      reconnect_interval: initial_interval,
      rejoin_interval: initial_interval
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
    unused(reason, in: [:dev])
    in_env(dev: :ok, else: Logger.error("disconnected: #{inspect(reason)}"))
    Process.send_after(self(), :connect, interval)
    Cloak.Air.unregister_air()
    {:ok, %{state | reconnect_interval: next_interval(interval)}}
  end

  @impl GenSocketClient
  def handle_joined(topic, payload, _transport, state) do
    Logger.info("joined the topic #{topic}")

    if topic == "main" do
      Cloak.Air.register_air(payload.air_name)
      Cloak.AnalystTable.refresh()
    end

    initial_interval = config(:min_reconnect_interval)
    {:ok, %{state | rejoin_interval: initial_interval}}
  end

  @impl GenSocketClient
  def handle_join_error(topic, payload, _transport, state) do
    Logger.error("join error on the topic #{topic}: #{inspect(payload)}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_channel_closed(topic, payload, _transport, %{rejoin_interval: interval} = state) do
    Logger.error("disconnected from the topic #{topic}: #{inspect(payload)}")
    Process.send_after(self(), {:join, topic}, interval)
    {:ok, %{state | rejoin_interval: next_interval(interval)}}
  end

  @impl GenSocketClient
  def handle_message("main", "air_call", request, transport, state) do
    handle_air_call(request.event, request.payload, {transport, request.request_id}, state)
  end

  def handle_message("main", "air_cast", request, transport, state),
    do: handle_air_cast(request.event, request.payload, transport, state)

  def handle_message("main", "air_response", payload, _transport, state) do
    request_id = payload.request_id

    case Map.fetch(state.pending_calls, request_id) do
      {:ok, request_data} ->
        Process.cancel_timer(request_data.timeout_ref)

        response =
          case payload.status do
            :ok -> {:ok, payload.result}
            :error -> {:error, payload.result}
            _other -> {:error, {:invalid_status, payload}}
          end

        GenSocketClient.reply(request_data.from, response)

      :error ->
        Logger.warn("unknown sync call response: #{inspect(payload)}")
    end

    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
  end

  def handle_message(topic, event, payload, _transport, state) do
    Logger.warn("unhandled message on topic #{topic}: #{event} #{inspect(payload)}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_reply(topic, _ref, payload, _transport, state) do
    Logger.warn("unhandled reply on topic #{topic}: #{inspect(payload)}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_info(:connect, _transport, state) do
    in_env(dev: nil, else: Logger.info("connecting"))
    {:connect, state}
  end

  def handle_info({:join, topic}, transport, state) do
    case GenSocketClient.join(transport, topic, join_info(DataSourceUpdater.register_socket())) do
      {:error, reason} ->
        Logger.error("error joining the topic #{topic}: #{inspect(reason)}")
        Process.send_after(self(), {:join, topic}, config(:rejoin_interval))

      {:ok, _ref} ->
        :ok
    end

    {:ok, state}
  end

  def handle_info({{__MODULE__, :cast_air}, topic, event, payload}, transport, state) do
    GenSocketClient.push(transport, topic, event, payload)
    {:ok, state}
  end

  def handle_info({:call_timeout, request_id}, _transport, state) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout")
    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
  end

  def handle_info({:data_sources_changed, new_data_sources}, transport, state) do
    Logger.info("Data sources changed, sending new configurations to air ...")
    GenSocketClient.push(transport, "main", "update_config", join_info(new_data_sources))
    {:ok, state}
  end

  def handle_info(message, _transport, state) do
    Logger.warn("unhandled message #{inspect(message)}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_call({:call_air, request_id, topic, event, payload, timeout}, from, transport, state) do
    case GenSocketClient.push(transport, topic, "cloak_call", %{
           request_id: request_id,
           event: event,
           payload: payload
         }) do
      {:ok, _push_ref} ->
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
    with {:ok, data_source} <- fetch_data_source(serialized_query.data_source),
         :ok <-
           Cloak.Query.Runner.start(
             serialized_query.id,
             serialized_query.analyst_id,
             data_source,
             serialized_query.statement || "",
             decode_params(serialized_query.parameters),
             %{
               views: serialized_query[:views] || %{},
               analyst_tables: serialized_query[:analyst_tables] || %{}
             }
           ),
         do: respond_to_air(from, :ok),
         else: ({:error, reason} -> respond_to_air(from, :error, reason))

    {:ok, state}
  end

  defp handle_air_call("describe_query", serialized_query, from, state) do
    with {:ok, data_source} <- fetch_data_source(serialized_query.data_source),
         {:ok, columns, metadata} <-
           Cloak.Sql.Query.describe_query(
             serialized_query.analyst_id,
             data_source,
             serialized_query.statement || "",
             decode_params(serialized_query.parameters),
             serialized_query.views
           ),
         do:
           respond_to_air(from, :ok, %{
             columns: columns,
             selected_types: metadata.selected_types,
             parameter_types: metadata.parameter_types
           }),
         else: ({:error, reason} -> respond_to_air(from, :ok, %{error: reason}))

    {:ok, state}
  end

  defp handle_air_call("validate_views", serialized_view, from, state) do
    case fetch_data_source(serialized_view.data_source) do
      {:ok, data_source} ->
        respond_to_air(from, :ok, validate_views(serialized_view.analyst_id, data_source, serialized_view.views))

      {:error, reason} ->
        respond_to_air(from, :error, reason)
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

  defp handle_air_call("create_or_update_analyst_table", data, from, state) do
    with {:ok, data_source} <- fetch_data_source(data.data_source),
         {:ok, described_columns} <-
           Cloak.AnalystTable.create_or_update(
             data.analyst_id,
             data.table_name,
             data.old_table_name,
             data.statement,
             data_source,
             data.parameters,
             data.views
           ) do
      view_validation_result = validate_views(data.analyst_id, data_source, data.views)
      respond_to_air(from, :ok, {described_columns, view_validation_result})
    else
      {:error, reason} -> respond_to_air(from, :error, reason)
    end

    {:ok, state}
  end

  defp handle_air_call("drop_analyst_table", data, from, state) do
    with {:ok, data_source} <- fetch_data_source(data.data_source),
         :ok <- Cloak.AnalystTable.drop_tables(data.analyst_id, data_source.name, [data.table_name]),
         do: respond_to_air(from, :ok, validate_views(data.analyst_id, data_source, data.views)),
         else: ({:error, reason} -> respond_to_air(from, :error, reason))

    {:ok, state}
  end

  defp handle_air_call("type_check_query", data, from, state) do
    {:ok, data_source} = fetch_data_source(data.data_source)

    try do
      explanation = perform_type_check!(data, data_source)
      respond_to_air(from, :ok, %{"status" => "ok", "data" => Cloak.Sql.Query.Explainer.for_editor(explanation)})
    rescue
      e in Cloak.Sql.Parser.ParseError -> respond_with_error(from, e)
      e in Cloak.Sql.CompilationError -> respond_with_error(from, e)
    end

    {:ok, state}
  end

  defp perform_type_check!(data, data_source) do
    parse = Cloak.Sql.Parser.parse!(data.statement)

    case parse.command do
      {:explain, what} ->
        parse
        |> Map.put(:command, what)
        |> compile!(data, data_source)

      :show ->
        nil

      _ ->
        compile!(parse, data, data_source)
    end
  end

  defp compile!(parse, data, data_source) do
    parse
    |> Cloak.Sql.Compiler.compile!(
      data.analyst_id,
      data_source,
      nil,
      data.views || %{}
    )
    |> Cloak.Query.DbEmulator.compile()
    |> Cloak.Sql.Query.Explainer.explain()
  end

  defp respond_with_error(from, %Cloak.Sql.Parser.ParseError{message: message, source_location: location}),
    do:
      respond_to_air(from, :ok, %{
        "status" => "error",
        "data" => %{
          "message" => message,
          "location" => format_location(location),
          "type" => "ParseError"
        }
      })

  defp respond_with_error(from, %Cloak.Sql.CompilationError{message: message, source_location: location}),
    do:
      respond_to_air(from, :ok, %{
        "status" => "error",
        "data" => %{
          "message" => message,
          "location" => format_location(location),
          "type" => "CompilationError"
        }
      })

  defp format_location({row, col}), do: %{line: row - 1, ch: col - 1}
  defp format_location(nil), do: nil

  # -------------------------------------------------------------------
  # Handling air async casts
  # -------------------------------------------------------------------

  defp handle_air_cast("reinitialize_all_data_sources", _data, _transport, state) do
    Cloak.DataSource.SerializingUpdater.reinitialize_all_data_sources()
    {:ok, state}
  end

  defp handle_air_cast("refresh_analyst_tables", _payload, _transport, state) do
    Cloak.AnalystTable.refresh()
    {:ok, state}
  end

  defp handle_air_cast("drop_analyst_tables", data, _transport, state) do
    Cloak.AnalystTable.drop_tables(data.analyst_id, data.data_source, data.table_names)
    {:ok, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp fetch_data_source(data_source_name),
    do: with(:error <- Cloak.DataSource.fetch(data_source_name), do: {:error, "Unknown data source."})

  defp decode_params(params), do: :erlang.binary_to_term(Base.decode16!(params))

  defp air_socket_url() do
    full_path =
      case Aircloak.DeployConfig.fetch("air_site") do
        {:ok, air_site} ->
          air_site

        :error ->
          raise "The air_site is not configured in config.json. Please consult the " <>
                  "\"Configuring the system\" section of the User Guide."
      end

    full_path
    |> URI.parse()
    |> Map.put(:path, "/cloak/socket/websocket")
    |> URI.to_string()
  end

  @spec respond_to_air({GenSocketClient.transport(), request_id :: String.t()}, :ok | :error, any) ::
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

  @spec cast_air(GenServer.server(), String.t(), String.t(), map) :: :ok
  defp cast_air(socket, topic, event, payload) do
    send(socket, {{__MODULE__, :cast_air}, topic, event, payload})
    :ok
  end

  @spec call_air(GenServer.server(), String.t(), String.t(), map, pos_integer) :: :ok | {:error, any}
  defp call_air(socket, topic, event, payload, timeout) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()

    case GenSocketClient.call(
           socket,
           {:call_air, request_id, topic, event, payload, timeout},
           timeout
         ) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  defp cloak_params() do
    %{
      cloak_name: cloak_name(),
      version: Aircloak.Version.for_app(:cloak)
    }
  end

  @doc false
  def cloak_name() do
    vm_short_name =
      Node.self()
      |> Atom.to_string()
      |> String.split("@")
      |> hd()

    {:ok, hostname} = :inet.gethostname()

    "#{vm_short_name}@#{hostname}"
  end

  defp join_info(data_sources) do
    %{
      data_sources: data_sources,
      salt_hash: get_salt_hash(),
      secret_proof: Aircloak.DeployConfig.get("cloak_secret", "") |> Aircloak.SharedSecret.proof()
    }
  end

  defp next_interval(current_interval) do
    min(current_interval * 2, config(:max_reconnect_interval))
  end

  defp config(key) do
    Application.get_env(:cloak, :air) |> Keyword.fetch!(key)
  end

  defp validate_views(analyst_id, data_source, views) do
    for {name, view} <- views do
      Task.async(fn ->
        case Cloak.Sql.Query.validate_view(analyst_id, data_source, name, view.sql, views) do
          {:ok, columns} -> %{name: name, valid: true, columns: columns}
          {:error, field, reason} -> %{name: name, valid: false, field: field, error: reason}
        end
      end)
    end
    |> Enum.map(&Task.await/1)
  end

  defp get_salt_hash(), do: :crypto.hash(:sha256, Cloak.Query.Anonymizer.config(:salt)) |> Base.encode16()

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    Aircloak.ChildSpec.supervisor(
      [
        Cloak.AirSocket.DataSourceUpdater,
        %{id: __MODULE__, start: {__MODULE__, :start_link, []}}
      ],
      strategy: :rest_for_one,
      name: __MODULE__.Supervisor
    )
  end
end
