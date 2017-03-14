defmodule Air.Service.Central.Worker do
  @moduledoc "Serializes requests to central."

  use GenServer
  alias Air.Schemas.CentralCall
  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc false
  @spec start_link(Keyword.t) :: GenServer.on_start
  def start_link(options \\ []) do
    options = Keyword.merge(default_options(), options)
    GenServer.start_link(__MODULE__, options, name: Keyword.fetch!(options, :name))
  end

  @doc "Schedules the given central call to be performed asynchronously."
  @spec perform_rpc(pid | __MODULE__, CentralCall.t) :: :ok
  def perform_rpc(pid \\ __MODULE__, central_call), do:
    GenServer.call(pid, {:perform_rpc, central_call})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(options) do
    Process.flag(:trap_exit, true)
    {:ok, %{current_send: nil, send_paused?: false, queue: :queue.new(), pending_count: 0, options: options}}
  end

  @doc false
  def handle_call({:perform_rpc, central_call}, _from, state), do:
    {:reply, :ok, do_perform_rpc(state, central_call)}

  def handle_info({:EXIT, pid, reason}, %{current_send: %{pid: pid}} = state), do:
    {:noreply, send_finished(state, reason)}
  def handle_info(:resume_send, state), do:
    {:noreply, maybe_start_rpc_task(%{state | send_paused?: false})}
  def handle_info(msg, state) do
    Logger.warn("Unhandled message #{inspect msg}")
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp default_options(), do:
    [
      name: __MODULE__,
      sender_fun: &send_to_central/1,
      central_retry_delay: Application.fetch_env!(:air, :central_retry_delay)
    ]

  defp option(state, name), do:
    Keyword.fetch!(state.options, name)

  defp do_perform_rpc(state, central_call) do
    state
    |> push_to_queue_back(central_call)
    |> inc_pending_count()
    |> maybe_start_rpc_task()
  end

  defp push_to_queue_front(state, central_call), do:
    %{state | queue: :queue.in_r(central_call, state.queue)}

  defp push_to_queue_back(state, central_call), do:
    %{state | queue: :queue.in(central_call, state.queue)}

  defp inc_pending_count(state), do:
    %{state | pending_count: state.pending_count + 1}

  defp dec_pending_count(state), do:
    %{state | pending_count: state.pending_count - 1}

  defp maybe_start_rpc_task(%{send_paused?: true} = state), do:
    state
  defp maybe_start_rpc_task(%{current_send: current_send} = state) when current_send != nil, do:
    state
  defp maybe_start_rpc_task(state) do
    case :queue.out(state.queue) do
      {:empty, _} -> state
      {{:value, central_call}, queue} ->
        {:ok, pid} = Task.start_link(fn ->
          central_call
          |> CentralCall.export()
          |> option(state, :sender_fun).()
        end)
        %{state |
          current_send: %{pid: pid, central_call: central_call},
          queue: queue
        }
    end
  end

  defp send_to_central(central_call), do:
    {:ok, _} = Air.CentralClient.Socket.rpc(central_call)

  defp send_finished(state, reason), do:
    state
    |> handle_finished_reason(reason)
    |> Map.put(:current_send, nil)
    |> maybe_start_rpc_task()

  defp handle_finished_reason(state, :normal), do:
    dec_pending_count(state)
  defp handle_finished_reason(%{current_send: %{central_call: central_call}} = state, abnormal_reason) do
    Logger.error("RPC '#{central_call.event}' to central failed: #{inspect abnormal_reason}. Will retry later.")
    state
    |> pause_send()
    |> push_to_queue_front(central_call)
  end

  defp pause_send(state) do
    Process.send_after(self(), :resume_send, option(state, :central_retry_delay))
    %{state | send_paused?: true}
  end
end
