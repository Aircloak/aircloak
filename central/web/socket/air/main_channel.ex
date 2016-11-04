defmodule Central.Socket.Air.MainChannel do
  @moduledoc """
  Main communication channel between an air and the central system.
  """
  use Phoenix.Channel
  require Logger
  alias Central.Service.Customer


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  # None in the initial version

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("main", _air_info, socket) do
    Process.flag(:trap_exit, true)
    customer = socket.assigns.customer
    Logger.info("air for '#{customer.name}' (id: #{customer.id}) joined central")
    {:ok, %{}, socket}
  end

  @doc false
  def terminate(_reason, socket) do
    customer = socket.assigns.customer
    Logger.info("air for '#{customer.name}' (id: #{customer.id}) left central")
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
  def handle_in("air_call", request, socket) do
    handle_air_call(request["event"], request["payload"], request["request_id"], socket)
  end
  def handle_in(event, _payload, socket) do
    air_name = socket.assigns.air_name
    Logger.warn("unknown event #{event} from '#{air_name}'")
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
    Logger.warn("#{request_id} sync call timeout on #{socket.assigns.air_id}")
    pending_calls = Map.delete(socket.assigns.pending_calls, request_id)
    {:noreply, assign(socket, :pending_calls, pending_calls)}
  end
  def handle_info({:EXIT, _, :normal}, socket) do
    # probably the linked reporter terminated successfully
    {:noreply, socket}
  end
  def handle_info({:DOWN, ref, :process, _pid, _reason}, socket = %{assigns: %{manager_ref: ref}}), do:
    {:stop, :data_source_manager_down, socket}
  def handle_info(message, socket) do
    air_id = socket.assigns.air_id
    Logger.info("unhandled info #{inspect(message)} from '#{air_id}'")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling air sync calls
  # -------------------------------------------------------------------

  defp handle_air_call("query_execution", payload, request_id, socket) do
    Logger.info("Received query execution update with payload: #{inspect payload}")
    customer = socket.assigns.customer
    params = %{
      metrics: payload["metrics"],
      features: payload["features"],
      aux: payload["aux"],
    }
    result = Customer.record_query(customer, params)
    respond_to_air(socket, request_id, result)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @spec respond_to_air(Socket.t, request_id::String.t, :ok | :error, any) :: :ok
  defp respond_to_air(socket, request_id, status, result \\ nil) do
    push(socket, "call_response", %{
      request_id: request_id,
      status: status,
      result: result
    })
  end

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end
end
