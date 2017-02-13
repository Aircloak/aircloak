defmodule Air.CentralClient.Socket do
  @moduledoc """
  Client side of the socket connection to the Central system.

  This module will connect to the Central system. The connection parameters are
  hardcoded in the configuration file. If the connection can't be established,
  the process will attempt to reconnect in regular intervals specified in the
  configuration file.
  """

  require Logger
  require Aircloak.DeployConfig
  alias Phoenix.Channels.GenSocketClient
  alias Air.{Schemas.CentralCall, Repo}

  @behaviour GenSocketClient


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the socket client."
  @spec start_link(Map.t, GenServer.options) :: GenServer.on_start
  def start_link(air_params \\ air_params(), gen_server_opts \\ [name: __MODULE__]) do
    GenSocketClient.start_link(
      __MODULE__,
      GenSocketClient.Transport.WebSocketClient,
      central_socket_url(air_params),
      [
        serializer: config(:serializer),
        transport_opts: [
          keepalive: :timer.seconds(30)
        ]
      ],
      gen_server_opts
    )
  end

  @doc "Records a completed query in the central - useful for billing and stats"
  @spec record_query(Map.t) :: :ok
  def record_query(payload), do:
    cast_with_retry(__MODULE__, "query_execution", payload)

  @doc "Records a connection of a cloak in the central."
  @spec record_cloak_online(String.t, [String.t], String.t) :: :ok
  def record_cloak_online(cloak_name, data_source_names, version), do:
    cast_with_retry(__MODULE__, "cloak_online", %{
      name: cloak_name, data_source_names: data_source_names, version: version})

  @doc "Records a disconnection of a cloak in the central."
  @spec record_cloak_offline(String.t) :: :ok
  def record_cloak_offline(cloak_name), do:
    cast_with_retry(__MODULE__, "cloak_offline", %{name: cloak_name})

  @doc "Sends usage info to central."
  @spec send_usage_info(Map.t) :: :ok
  def send_usage_info(usage_info), do:
    cast_with_retry(__MODULE__, "usage_info", usage_info)


  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(central_socket_url) do
    initial_interval = config(:min_reconnect_interval)
    state = %{
      pending_calls: %{},
      reconnect_interval: initial_interval,
      rejoin_interval: initial_interval
    }
    :timer.send_interval(:timer.minutes(5), :check_for_pending_rpcs)
    Logger.debug("Trying to connect to Central on #{central_socket_url}")
    {:connect, central_socket_url, state}
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
    reattempt_pending_rpcs()
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
  def handle_message("main", "central_call", request, transport, state) do
    handle_central_call(request["event"], request["payload"], {transport, request["request_id"]}, state)
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
    join_message = %{
      air_version: Aircloak.Version.for_app(:air) |> Aircloak.Version.to_string(),
      online_cloaks: online_cloaks(),
    }
    case GenSocketClient.join(transport, topic, join_message) do
      {:error, reason} ->
        Logger.error("error joining the topic #{topic}: #{inspect reason}")
        Process.send_after(self(), {:join, topic}, config(:rejoin_interval))
      {:ok, _ref} -> :ok
    end
    {:ok, state}
  end
  def handle_info({{__MODULE__, :call}, timeout, from, event, payload}, transport, state) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    GenSocketClient.push(transport, "main", "air_call",
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
  def handle_info(:check_for_pending_rpcs, _transport, state) do
    reattempt_pending_rpcs()
    {:ok, state}
  end
  def handle_info(message, _transport, state) do
    Logger.warn("unhandled message #{inspect message}")
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Handling central sync calls
  # -------------------------------------------------------------------

  defp handle_central_call(event, payload, _from, state) do
    Logger.info("Received call from central: #{event}, with payload: #{inspect payload}")
    {:ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @spec call(GenServer.server, String.t, Map.t, pos_integer) :: {:ok, any} | {:error, any}
  defp call(socket, event, payload, timeout \\ :timer.seconds(5)) do
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

  defp cast_with_retry(socket, event, payload) do
    case persist_rpc(event, payload) do
      {:ok, rpc} -> perform_cast_with_retry(socket, rpc)
      :error -> :error
    end
  end

  defp persist_rpc(event, payload) do
    changeset = CentralCall.changeset(%CentralCall{}, %{event: event, payload: payload})
    case Repo.insert(changeset) do
      {:error, changeset} ->
        Logger.error("Unable to persist RPC call to central to guarantee delivery. Aborting RPC. "
          <> "Failure: #{inspect changeset}")
        :error
      {:ok, _} = result -> result
    end
  end

  defp perform_cast_with_retry(socket, rpc) do
    Task.start(fn() ->
      payload = %{
        id: rpc.id,
        event: rpc.event,
        event_payload: rpc.payload,
      }
      case call(socket, "call_with_retry", payload) do
        {:error, reason} ->
          Logger.error("RPC '#{rpc.event}' to central failed: #{inspect reason}. Will retry later.")
        {:ok, _} -> Repo.delete!(rpc)
      end
    end)
    :ok
  end

  defp reattempt_pending_rpcs() do
    Logger.info("Checking for buffered RPC calls to central")
    for rpc <- Repo.all(CentralCall) do
      perform_cast_with_retry(__MODULE__, rpc)
    end
  end

  defp central_socket_url(air_params) do
    central_url()
    |> URI.parse()
    |> Map.put(:path, "/air/socket/websocket")
    |> Map.put(:query, URI.encode_query(air_params))
    |> URI.to_string()
  end

  # We allow for an alternate `central` to be used in the case of test deployments for the staging environment.
  # This parameter is not documented elsewhere. We want to reduce the chance of it becoming public knowledge.
  # Production deployments should talk to our central air for billing purposes and to allow us to collect stats.
  defp central_url() do
    case Map.fetch(Aircloak.DeployConfig.fetch!("site"), "alternate_aircloak_central") do
      {:ok, url} -> url
      :error -> config(:central_site)
    end
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  defp air_params() do
    %{air_name: air_name(), token: customer_token()}
  end

  defp air_name() do
    vm_short_name =
      Node.self()
      |> Atom.to_string()
      |> String.split("@")
      |> hd()
    {:ok, hostname} = :inet.gethostname()

    "#{vm_short_name}@#{hostname}"
  end

  defp next_interval(current_interval) do
    min(current_interval * 2, config(:max_reconnect_interval))
  end

  defp customer_token(), do: Air.site_setting("customer_token")

  defp config(key) do
    Application.get_env(:air, :central) |> Keyword.fetch!(key)
  end

  defp online_cloaks(), do:
    Air.DataSourceManager.cloaks()
    |> Enum.map(&%{
      name: &1.name,
      data_source_names: &1.data_source_ids,
      version: &1.version,
    })

  if Mix.env == :dev do
    # suppressing of some common log messages in dev env to avoid excessive noise
    defp log_connect(), do: :ok
    defp log_disconnected(_reason), do: :ok
  else
    defp log_connect(), do: Logger.info("connecting")
    defp log_disconnected(reason), do: Logger.error("disconnected: #{inspect reason}")
  end
end
