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

  @behaviour GenSocketClient

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if Air is connected to the main channel of the socket process."
  @spec connected?() :: boolean
  def connected?() do
    case Registry.lookup(Air.Service.Central.Registry, __MODULE__) do
      [{_pid, connected?}] -> connected?
      [] -> false
    end
  end

  @doc "Starts the socket client."
  @spec start_link(GenServer.options()) :: GenServer.on_start()
  def start_link(gen_server_opts \\ [name: __MODULE__]) do
    GenSocketClient.start_link(
      __MODULE__,
      GenSocketClient.Transport.WebSocketClient,
      nil,
      [serializer: config(:serializer)],
      gen_server_opts
    )
  end

  # -------------------------------------------------------------------
  # GenSocketClient callbacks
  # -------------------------------------------------------------------

  @impl GenSocketClient
  def init(_) do
    {:ok, _} = Registry.register(Air.Service.Central.Registry, __MODULE__, false)
    initial_interval = config(:min_reconnect_interval)

    state = %{
      pending_calls: %{},
      reconnect_interval: initial_interval,
      rejoin_interval: initial_interval
    }

    Logger.debug(fn -> "Trying to connect to Central on #{central_socket_url()}" end)
    {:connect, central_socket_url(), air_params(), state}
  end

  @impl GenSocketClient
  def handle_connected(_transport, state) do
    Logger.info("connected")
    send(self(), {:join, "main"})
    initial_interval = config(:min_reconnect_interval)
    set_connected(false)
    {:ok, %{state | reconnect_interval: initial_interval}}
  end

  @impl GenSocketClient
  def handle_disconnected(reason, %{reconnect_interval: interval} = state) do
    log_disconnected(reason)
    Process.send_after(self(), :connect, interval)
    set_connected(false)
    {:ok, %{state | reconnect_interval: next_interval(interval)}}
  end

  @impl GenSocketClient
  def handle_joined(topic, _payload, _transport, state) do
    Logger.info("joined the topic #{topic}")
    initial_interval = config(:min_reconnect_interval)
    if topic == "main", do: set_connected(true)
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
    if topic == "main", do: set_connected(false)
    {:ok, %{state | rejoin_interval: next_interval(interval)}}
  end

  @impl GenSocketClient
  def handle_message("main", "central_call", request, transport, state) do
    handle_central_call(
      request["event"],
      request["payload"],
      {transport, request["request_id"]},
      state
    )
  end

  def handle_message("main", "central_response", payload, _transport, state) do
    request_id = payload["request_id"]

    case Map.fetch(state.pending_calls, request_id) do
      {:ok, request_data} ->
        Process.cancel_timer(request_data.timeout_ref)

        response =
          case payload["status"] do
            "ok" -> {:ok, payload["result"]}
            "error" -> {:error, payload["result"]}
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
    log_connect()
    {:connect, central_socket_url(), air_params(), state}
  end

  def handle_info({:join, topic}, transport, state) do
    join_message = %{air_version: version()}

    case GenSocketClient.join(transport, topic, join_message) do
      {:error, reason} ->
        Logger.error("error joining the topic #{topic}: #{inspect(reason)}")
        Process.send_after(self(), {:join, topic}, config(:rejoin_interval))

      {:ok, _ref} ->
        :ok
    end

    {:ok, state}
  end

  def handle_info({:call_timeout, request_id}, _transport, state) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout")
    {:ok, update_in(state.pending_calls, &Map.delete(&1, request_id))}
  end

  def handle_info(message, _transport, state) do
    Logger.warn("unhandled message #{inspect(message)}")
    {:ok, state}
  end

  @impl GenSocketClient
  def handle_call({:call_central, timeout, event, payload}, from, transport, state) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()

    GenSocketClient.push(transport, "main", "air_call", %{
      request_id: request_id,
      event: event,
      payload: payload
    })

    timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)
    {:noreply, put_in(state.pending_calls[request_id], %{from: from, timeout_ref: timeout_ref})}
  end

  # -------------------------------------------------------------------
  # Handling central sync calls
  # -------------------------------------------------------------------

  defp handle_central_call(event, payload, _from, state) do
    Logger.info("Received call from central: #{event}, with payload: #{inspect(payload)}")
    {:ok, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp set_connected(value),
    do: {^value, _} = Registry.update_value(Air.Service.Central.Registry, __MODULE__, fn _ -> value end)

  defp central_socket_url(),
    do:
      central_url()
      |> URI.parse()
      |> Map.put(:path, "/air/socket/websocket")
      |> URI.to_string()

  # We allow for an alternate `central` to be used in the case of test deployments for the staging environment.
  # This parameter is not documented elsewhere. We want to reduce the chance of it becoming public knowledge.
  # Production deployments should talk to our central air for billing purposes and to allow us to collect stats.
  defp central_url() do
    case Map.fetch(Aircloak.DeployConfig.fetch!("site"), "alternate_aircloak_central") do
      {:ok, url} -> url
      :error -> config(:central_site)
    end
  end

  defp air_params(), do: [air_name: Air.instance_name()]

  defp next_interval(current_interval) do
    min(current_interval * 2, config(:max_reconnect_interval))
  end

  defp config(key) do
    Application.get_env(:air, :central) |> Keyword.fetch!(key)
  end

  if Mix.env() == :dev do
    # suppressing of some common log messages in dev env to avoid excessive noise
    defp log_connect(), do: :ok
    defp log_disconnected(_reason), do: :ok
  else
    defp log_connect(), do: Logger.info("connecting")
    defp log_disconnected(reason), do: Logger.error("disconnected: #{inspect(reason)}")
  end

  defp version(), do: Aircloak.Version.for_app(:air)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do: Supervisor.child_spec(__MODULE__, [])
end
