defmodule Air.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Executes a query on the given cloak.

  The function returns when the cloak responds. If the timeout occurs, it is
  still possible that a cloak has received the request.
  """
  @spec run_query(pid | nil, Air.Query.cloak_query) :: :ok | {:error, any}
  def run_query(channel_pid, query) do
    try do
      case call(channel_pid, "run_query", query, :timer.seconds(5)) do
        {:ok, _} -> :ok
        error -> error
      end
    catch
      :exit, :noproc ->
        {:error, :not_connected}
    end
  end

  @doc "Stops a query on the given cloak."
  @spec stop_query(pid | nil, String.t) :: :ok | {:error, any}
  def stop_query(channel_pid, query_id) do
    try do
      case call(channel_pid, "stop_query", query_id, :timer.seconds(5)) do
        {:ok, _} -> :ok
        error -> error
      end
    catch
      :exit, :noproc ->
        {:error, :not_connected}
    end
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("main", cloak_info, socket) do
    Process.flag(:trap_exit, true)

    cloak = %{
      id: socket.assigns.cloak_id,
      name: socket.assigns.name,
      online_since: Timex.DateTime.now(),
    }
    data_sources = Map.fetch!(cloak_info, "data_sources")
    Air.DataSourceManager.register_cloak(cloak, data_sources)

    {:ok, %{}, assign(socket, :pending_calls, %{})}
  end

  @doc false
  def terminate(_reason, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("cloak '#{cloak_id}' left air")
    {:ok, socket}
  end

  @doc false
  def handle_in("call_response", payload, socket) do
    request_id = payload["request_id"]

    case Map.fetch(socket.assigns.pending_calls, request_id) do
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
    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end
  def handle_in("cloak_call", request, socket) do
    handle_cloak_call(request["event"], request["payload"], request["request_id"], socket)
  end
  def handle_in(event, _payload, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.warn("unknown event #{event} from '#{cloak_id}'")
    {:noreply, socket}
  end

  @doc false
  def handle_info({{__MODULE__, :call}, timeout, from, event, payload}, socket) do
    request_id = make_ref() |> :erlang.term_to_binary() |> Base.encode64()
    push(socket, "air_call", %{request_id: request_id, event: event, payload: payload})
    timeout_ref = Process.send_after(self(), {:call_timeout, request_id}, timeout)
    {:noreply,
      assign(socket, :pending_calls,
        Map.put(socket.assigns.pending_calls, request_id, %{from: from, timeout_ref: timeout_ref}))
    }
  end
  def handle_info({:call_timeout, request_id}, socket) do
    # We're just removing entries here without responding. It is the responsibility of the
    # client code to give up at some point.
    Logger.warn("#{request_id} sync call timeout on #{socket.assigns.cloak_id}")
    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end
  def handle_info({:EXIT, _, :normal}, socket) do
    # probably the linked reporter terminated successfully
    {:noreply, socket}
  end
  def handle_info(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{inspect(message)} from '#{cloak_id}'")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling cloak sync calls
  # -------------------------------------------------------------------

  defp handle_cloak_call("query_result", query_result, request_id, socket) do
    Logger.info("received result for query #{query_result["query_id"]}")
    respond_to_cloak(socket, request_id, :ok)
    Air.QueryEvents.trigger_result(query_result)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @spec respond_to_cloak(Socket.t, request_id::String.t, :ok | :error, any) :: :ok
  defp respond_to_cloak(socket, request_id, status, result \\ nil) do
    push(socket, "call_response", %{
      request_id: request_id,
      status: status,
      result: result
    })
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  @spec call(pid | nil, String.t, %{}, pos_integer) :: {:ok, any} | {:error, any}
  defp call(nil, _event, _payload, _timeout), do: exit(:noproc)
  defp call(pid, event, payload, timeout) do
    mref = Process.monitor(pid)
    send(pid, {{__MODULE__, :call}, timeout, {self(), mref}, event, payload})
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
end
