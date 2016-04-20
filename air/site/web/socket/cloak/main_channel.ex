defmodule Air.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel
  require Logger
  alias Air.CloakInfo


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Runs a task on the given cloak.

  The function returns when the cloak responds. If the timeout occurs, it is
  still possible that a cloak has received the request.
  """
  @spec run_task(CloakInfo.cloak_id, %{}) :: :ok | {:error, any}
  def run_task(cloak_id, task) do
    case call(cloak_id, "run_task", task, :timer.seconds(5)) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  # Temporary example on how to execute a task
  # TODO: remove this when UI integration is in place
  @doc false
  def test_run_task() do
    task = %{
      id: "1",
      prefetch: [%{table: "local/test"}],
      code: "report_property(\"test\", 1)"
    }
    run_task("unknown_org/nonode@nohost", task)
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("main", cloak_info, socket) do
    cloak_id = socket.assigns.cloak_id
    {:ok, _} = CloakInfo.start_link(cloak_id, cloak_info)

    {:ok, %{},
      socket
      |> assign(:pending_calls, %{})
    }
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
  def handle_info(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{message} from '#{cloak_id}'")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Handling cloak sync calls
  # -------------------------------------------------------------------

  defp handle_cloak_call("task_results", task_results, request_id, socket) do
    # TODO: do something with task results
    Logger.info("received task results #{inspect task_results}")
    respond_to_cloak(socket, request_id, :ok)
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

  @spec call(String.t, String.t, %{}, pos_integer) :: {:ok, any} | {:error, any}
  defp call(cloak_id, event, payload, timeout) do
    case CloakInfo.main_channel_pid(cloak_id) do
      nil -> exit(:noproc)
      pid ->
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
end
