defmodule Air.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel
  require Logger
  require Air.SyncRequester


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs a task on the given cloak"
  @spec run_task(String.t, %{}, timeout) :: any
  def run_task(cloak_id, task, timeout \\ :timer.seconds(5)) do
    call(cloak_id, {:run_task, task}, timeout)
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("main", cloak_info, socket) do
    cloak_id = socket.assigns.cloak_id

    # Using `ServiceRegistration` for strongly consistent discovery of the channel process.
    {:ok, _} = Air.ServiceRegistration.start_link(
          registration_key(cloak_id),
          registration_value(),
          crash_on_error: true
        )

    {:ok, %{}, assign(socket, :cloak_info, cloak_info)}
  end

  @doc false
  def terminate(_reason, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("cloak '#{cloak_id}' left air")
    {:ok, socket}
  end

  @doc false
  def handle_in(Air.SyncRequester.response_event(command), encoded_payload, socket) do
    {request, request_meta, response} = Air.SyncRequester.decode_response!(
        Air.Endpoint.sync_requester(), encoded_payload)
    handle_sync_response(command, response, request, request_meta, socket)
  end
  def handle_in(event, _payload, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.warn("unknown event #{event} from '#{cloak_id}'")
    {:noreply, socket}
  end

  @doc false
  def handle_info({{__MODULE__, :call}, from, message}, socket),
    do: handle_call(message, from, socket)
  def handle_info(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{message} from '#{cloak_id}'")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling internal requests
  # -------------------------------------------------------------------

  defp handle_call({:run_task, task}, from, socket) do
    Logger.info("starting task #{task.id} on #{socket.assigns.cloak_id}")
    payload = Air.SyncRequester.encode_request(Air.Endpoint.sync_requester(), task)
    push(socket, Air.SyncRequester.request_event("run_task"), payload)
    respond_to_internal_request(from, :ok)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling cloak responses
  # -------------------------------------------------------------------

  defp handle_sync_response("run_task", status, task, _req_meta, socket) do
    Logger.info("cloak #{socket.assigns.cloak_id}, task #{task.id}: #{inspect status}")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp respond_to_internal_request({client_pid, mref}, response) do
    send(client_pid, {mref, response})
  end

  defp call(cloak_id, message, timeout) do
    case channel_pid(cloak_id) do
      nil -> exit(:noproc)
      pid ->
        mref = Process.monitor(pid)
        send(pid, {{__MODULE__, :call}, {self(), mref}, message})
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

  defp channel_pid(cloak_id) do
    case :air_etcd.fetch(registration_key(cloak_id)) do
      :error -> nil
      {:ok, encoded_pid} ->
        encoded_pid |> Base.decode64!() |> :erlang.binary_to_term
    end
  end

  defp registration_key(cloak_id) do
    # base16 is used because the supported character set in the key is limited
    "/settings/air/cloaks/#{Base.encode16(cloak_id)}/main"
  end

  defp registration_value do
    self() |> :erlang.term_to_binary() |> Base.encode64()
  end
end
