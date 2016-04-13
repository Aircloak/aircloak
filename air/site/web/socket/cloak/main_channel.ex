defmodule Air.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel
  require Logger
  require Aircloak.SyncRequester
  alias Aircloak.SyncRequester


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
  def handle_in(SyncRequester.request_event(command), payload, socket) do
    {request_ref, request} = SyncRequester.decode_request!(payload)
    handle_sync_request(command, request, request_ref, socket)
  end
  def handle_in(SyncRequester.response_event(command), payload, socket) do
    case SyncRequester.decode_response!(
          SyncRequester.Backend.Etcd,
          request_etcd_path(socket),
          payload
        ) do
      {:matched, {request, request_meta, response}} ->
        handle_sync_response(command, response, request, request_meta, socket)
      {:not_matched, response} ->
        Logger.warn("unmatched response #{inspect response}")
        {:noreply, socket}
    end
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
    payload = SyncRequester.encode_request(SyncRequester.Backend.Etcd, request_etcd_path(socket), task)
    push(socket, SyncRequester.request_event("run_task"), payload)
    respond_to_internal_request(from, :ok)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling sync requests from Cloak
  # -------------------------------------------------------------------

  defp handle_sync_request("task_results", task_results, request_ref, socket) do
    Logger.info("cloak #{socket.assigns.cloak_id} sent task results #{inspect task_results}")
    response = :ok
    payload = SyncRequester.encode_response(request_ref, response)
    push(socket, SyncRequester.response_event("task_results"), payload)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling sync responses from Cloak
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
    # base32 is used because the supported character set in the etcd key is limited
    "/settings/air/cloaks/#{Base.encode32(cloak_id)}/main"
  end

  defp registration_value do
    self() |> :erlang.term_to_binary() |> Base.encode64()
  end

  defp request_etcd_path(socket) do
    # base32 is used because the supported character set in the etcd key is limited
    "/air/sync_requests/#{Base.encode32(socket.assigns.cloak_id)}"
  end
end
