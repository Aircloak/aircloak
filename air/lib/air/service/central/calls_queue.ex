defmodule Air.Service.Central.CallsQueue do
  @moduledoc """
  Queue of calls to central.

  This module powers the process which holds a queue of calls that must be sent to central.
  The queue has following properties:

    - preserved ordering
    - retry logic on send error
    - overflow control

  The current implementation is fairly simple and naive, with following known deficiencies:

    - Queue is not persisted. If the queue process crashes (or the system is restarted), pending messages
      are lost.
    - There is no support for progressive backoff.
    - Retry is dumb. A single item which repeatedly causes a send failure will not be removed until
      queue is overflowed.
  """

  use GenServer
  alias Air.Schemas.CentralCall
  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the queue process."
  @spec start_link(Keyword.t) :: GenServer.on_start
  def start_link(options \\ []) do
    options = Keyword.merge(default_options(), options)
    GenServer.start_link(__MODULE__, Map.new(options), name: Keyword.fetch!(options, :name))
  end

  @doc "Schedules the given central call to be performed asynchronously."
  @spec push(pid | __MODULE__, CentralCall.t) :: :ok
  def push(pid \\ __MODULE__, central_call), do:
    GenServer.call(pid, {:push, central_call})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(options) do
    Process.flag(:trap_exit, true)
    {:ok, %{current_send: nil, send_paused?: false, queue: Aircloak.Queue.new(), options: options}}
  end

  @doc false
  def handle_call({:push, central_call}, _from, state), do:
    {:reply, :ok, handle_push(state, central_call)}

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
    [name: __MODULE__, sender_fun: &send_to_central/1] ++ Application.fetch_env!(:air, :central_queue)

  defp handle_push(state, central_call), do:
    state
    |> update_in([:queue], &Aircloak.Queue.push(&1, central_call))
    |> update_in([:queue], &Aircloak.Queue.drop_while(&1, fn(queue) -> queue.size > state.options.max_size end))
    |> maybe_start_rpc_task()

  defp maybe_start_rpc_task(%{send_paused?: true} = state), do:
    state
  defp maybe_start_rpc_task(%{current_send: current_send} = state) when current_send != nil, do:
    state
  defp maybe_start_rpc_task(%{queue: %Aircloak.Queue{size: 0}} = state), do:
    state
  defp maybe_start_rpc_task(state) do
    {:value, central_call} = Aircloak.Queue.peek(state.queue)
    {:ok, pid} = Task.start_link(fn ->
      central_call
      |> CentralCall.export()
      |> state.options.sender_fun.()
    end)
    %{state | current_send: %{pid: pid, central_call: central_call}}
  end

  defp send_to_central(central_call), do:
    {:ok, _} = Air.CentralClient.Socket.rpc(central_call)

  defp send_finished(state, reason), do:
    state
    |> handle_finished_reason(reason)
    |> Map.put(:current_send, nil)
    |> maybe_start_rpc_task()

  defp handle_finished_reason(state, :normal), do:
    %{state | queue: Aircloak.Queue.drop_if(state.queue, &(&1 == state.current_send.central_call))}
  defp handle_finished_reason(state, abnormal_reason) do
    central_call = state.current_send.central_call
    Logger.error("RPC '#{central_call.event}' to central failed: #{inspect abnormal_reason}. Will retry later.")
    pause_send(state)
  end

  defp pause_send(state) do
    Process.send_after(self(), :resume_send, state.options.retry_delay)
    %{state | send_paused?: true}
  end
end
