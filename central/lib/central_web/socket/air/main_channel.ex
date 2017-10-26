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

  @impl Phoenix.Channel
  def join("main", raw_air_info, socket) do
    Logger.metadata(customer: socket.assigns.customer.name, air: socket.assigns.air_name)
    Process.flag(:trap_exit, true)
    Logger.info("joined central")
    online_cloaks = raw_air_info
    |> Map.get("online_cloaks", [])
    |> Enum.map(&%{
      name: Map.fetch!(&1, "name"),
      version: Map.get(&1, "version", "Unknown"),
      data_source_names: Map.get(&1, "data_source_names", []),
    })
    air_version = Map.get(raw_air_info, "air_version", "Unknown")
    air_info = %{
      air_version: air_version,
      online_cloaks: online_cloaks,
    }
    monitor_channel(socket.assigns.customer, socket.assigns.air_name, air_info)
    {:ok, %{}, socket |> assign(:air_version, air_version)}
  end

  @impl Phoenix.Channel
  def terminate(_reason, socket) do
    Logger.info("left central")
    {:ok, socket}
  end

  @impl Phoenix.Channel
  def handle_in("air_call", request, socket) do
    handle_air_call(request["event"], request["payload"], request["request_id"], socket)
  end
  def handle_in(event, _payload, socket) do
    Logger.warn("unknown event #{event}")
    {:noreply, socket}
  end

  @impl Phoenix.Channel
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
    Logger.warn("#{request_id} sync call timeout")
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
    Logger.info("unhandled info #{inspect(message)}")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling air sync calls
  # -------------------------------------------------------------------

  defp handle_air_call("call_with_retry", call_data, request_id, socket) do
    Customer.start_air_message_handler(
      call_data,
      socket.assigns.customer,
      socket.assigns.air_name,
      socket.assigns.air_version
    )

    respond_to_air(socket, request_id, :ok)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @spec respond_to_air(Socket.t, request_id::String.t, :ok | :error, any) :: :ok
  defp respond_to_air(socket, request_id, status, result \\ nil) do
    push(socket, "central_response", %{
      request_id: request_id,
      status: status,
      result: result
    })
  end

  if Mix.env == :test do
    # We avoid monitoring in test env, since this will start asynchronous processes storing
    # to the database, which will in turn lead to noisy errors in test output.
    defp monitor_channel(_customer, _air_name, _air_info), do: :ok
  else
    defp monitor_channel(customer, air_name, air_info), do:
      Central.AirStats.register(customer, air_name, air_info)
  end
end
