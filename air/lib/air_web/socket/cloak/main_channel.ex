defmodule AirWeb.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel, log_handle_in: false
  require Logger

  alias Air.CentralClient.Socket

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Executes a query on the given cloak.

  The function returns when the cloak responds. If the timeout occurs, it is
  still possible that a cloak has received the request.
  """
  @spec run_query(pid | nil, map) :: :ok | {:error, any}
  def run_query(channel_pid, query) do
    with {:ok, _} <- call(channel_pid, "run_query", query, :timer.seconds(5)), do: :ok
  end

  @doc """
  Asks the cloak to describe the query.

  Unlike `run_query/2`, this function is synchronous, meaning it waits for the
  cloak to respond, and returns the result obtained by the cloak.
  """
  @spec describe_query(pid | nil, map) :: {:ok, map} | {:error, any}
  def describe_query(channel_pid, query_data),
    do: call(channel_pid, "describe_query", query_data, :timer.seconds(5))

  @doc "Validates the view on the cloak."
  @spec validate_views(pid | nil, map) :: map
  def validate_views(channel_pid, view_data) do
    {:ok, results} = call(channel_pid, "validate_views", view_data, :timer.seconds(5))

    for result <- results, into: %{} do
      case result do
        %{name: name, valid: true, columns: columns} -> {name, {:ok, columns}}
        %{name: name, valid: false, field: field, error: error} -> {name, {:error, field, error}}
      end
    end
  end

  @doc "Stops a query on the given cloak."
  @spec stop_query(pid | nil, String.t()) :: :ok | {:error, any}
  def stop_query(channel_pid, query_id) do
    with {:ok, _} <- call(channel_pid, "stop_query", query_id, :timer.seconds(5)), do: :ok
  end

  @doc "Returns the list of queries running on this cloak."
  @spec running_queries(pid | nil) :: {:ok, [String.t()]} | {:error, any}
  def running_queries(channel_pid),
    do: call(channel_pid, "running_queries", nil, :timer.minutes(4))

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Channel
  def join("main", cloak_info, socket) do
    Process.flag(:trap_exit, true)

    cloak = create_cloak(cloak_info, socket)

    cloak
    |> Air.Service.Cloak.register(cloak_info.data_sources)
    |> revalidate_views()

    {:ok, %{}, assign(socket, :pending_calls, %{})}
  end

  @impl Phoenix.Channel
  def terminate(_reason, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("cloak '#{cloak_id}' left air")
    {:ok, socket}
  end

  @impl Phoenix.Channel
  def handle_in("cloak_call", payload, socket),
    do:
      Aircloak.report_long({:handle_cloak_call, payload.event}, fn ->
        handle_cloak_call(payload.event, payload.payload, payload.request_id, socket)
      end)

  def handle_in(cloak_message_name, payload, socket),
    do:
      Aircloak.report_long({:handle_cloak_cast, cloak_message_name}, fn ->
        handle_cloak_message(cloak_message_name, payload, socket)
      end)

  @impl Phoenix.Channel
  def handle_info(message, socket),
    do:
      Aircloak.report_long({:handle_info, message}, fn ->
        handle_erlang_message(message, socket)
      end)

  # -------------------------------------------------------------------
  # Handling of messages from other processes
  # -------------------------------------------------------------------

  defp handle_erlang_message({{__MODULE__, :call}, timeout, from, event, payload}, socket) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    push(socket, "air_call", %{request_id: request_id, event: event, payload: payload})
    timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)

    {:noreply,
     assign(
       socket,
       :pending_calls,
       Map.put(socket.assigns.pending_calls, request_id, %{from: from, timeout_ref: timeout_ref})
     )}
  end

  defp handle_erlang_message({:call_timeout, request_id}, socket) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout on #{socket.assigns.cloak_id}")
    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end

  defp handle_erlang_message({:EXIT, _, :normal}, socket) do
    # probably the linked reporter terminated successfully
    {:noreply, socket}
  end

  defp handle_erlang_message(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{inspect(message)} from '#{cloak_id}'")
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Handling of Cloak messages
  # -------------------------------------------------------------------

  defp handle_cloak_message("memory_reading", reading, socket) do
    Air.Service.Cloak.record_memory(reading)

    AirWeb.Socket.Frontend.MemoryChannel.broadcast_memory_reading(
      socket.assigns.cloak_id,
      reading
    )

    {:noreply, socket}
  end

  defp handle_cloak_message("update_config", cloak_info, socket) do
    cloak_info
    |> create_cloak(socket)
    |> Air.Service.Cloak.update(cloak_info.data_sources)
    |> revalidate_views()

    {:noreply, socket}
  end

  defp handle_cloak_message("query_state", payload, socket) do
    Air.Service.Query.Lifecycle.state_changed(payload.query_id, payload.query_state)
    {:noreply, socket}
  end

  defp handle_cloak_message("cloak_response", payload, socket) do
    request_id = payload.request_id

    case Map.fetch(socket.assigns.pending_calls, request_id) do
      {:ok, request_data} ->
        Process.cancel_timer(request_data.timeout_ref)

        response =
          case payload.status do
            :ok -> {:ok, payload[:result]}
            :error -> {:error, payload[:result]}
            _other -> {:error, {:invalid_status, payload}}
          end

        respond_to_internal_request(request_data.from, response)

      :error ->
        Logger.warn("unknown sync call response: #{inspect(payload)}")
    end

    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end

  defp handle_cloak_message(event, _payload, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.warn("unknown event #{event} from '#{cloak_id}'")
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Handling cloak sync calls
  # -------------------------------------------------------------------

  defp handle_cloak_call("query_result", query_result, request_id, socket) do
    Logger.info("received result for query #{query_result.query_id}")
    respond_to_cloak(socket, request_id, :ok)
    Air.Service.Query.Lifecycle.result_arrived(query_result)
    {:noreply, socket}
  end

  defp handle_cloak_call(other, payload, request_id, socket) do
    Logger.warn(
      "Received unknown cloak call #{inspect(other)} with payload #{
        inspect(payload, pretty: true)
      }"
    )

    respond_to_cloak(socket, request_id, :ok)
    {:noreply, socket}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @spec respond_to_cloak(Socket.t(), request_id :: String.t(), :ok | :error, any) :: :ok
  defp respond_to_cloak(socket, request_id, status, result \\ nil) do
    push(socket, "air_response", %{
      request_id: request_id,
      status: status,
      result: result
    })
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  @spec call(pid | nil, String.t(), any, pos_integer) :: {:ok, any} | {:error, any}
  defp call(nil, _event, _payload, _timeout), do: {:error, :not_connected}

  defp call(pid, event, payload, timeout) do
    mref = Process.monitor(pid)
    send(pid, {{__MODULE__, :call}, timeout, {self(), mref}, event, payload})

    receive do
      {^mref, response} ->
        Process.demonitor(mref, [:flush])
        response

      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp create_cloak(cloak_info, socket),
    do: %{
      id: socket.assigns.cloak_id,
      name: socket.assigns.name,
      version: socket.assigns.version,
      online_since: Timex.now(),
      salt_hash: cloak_info.salt_hash
    }

  if Mix.env() == :test do
    # do nothing in tests, because it leads to unexpected messages in various tests where cloak is mocked
    defp revalidate_views(_data_sources), do: :ok
  else
    defp revalidate_views(data_sources),
      do: Enum.each(data_sources, &Air.Service.View.revalidate_all_views/1)
  end
end
