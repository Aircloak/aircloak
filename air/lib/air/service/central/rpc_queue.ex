defmodule Air.Service.Central.RpcQueue do
  @moduledoc """
  Queue of RPCs to central.

  This module powers the process which holds a queue of RPCs that must be sent to central.
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

  use GenServer, start: {__MODULE__, :start_link, []}
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

  @doc "Schedules an RPC with the given event and a payload to be delivered asynchronously."
  @spec push(pid | __MODULE__, String.t, map) :: :ok
  def push(pid \\ __MODULE__, event, payload), do:
    GenServer.call(pid, {:push, event, payload})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(options) do
    Process.flag(:trap_exit, true)
    {:ok, %{current_send: nil, send_paused?: false, queue: Aircloak.Queue.new(), options: options}}
  end

  @impl GenServer
  def handle_call({:push, event, payload}, _from, state), do:
    {:reply, :ok, handle_push(state, event, payload)}

  @impl GenServer
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
      connected_fun: &Air.CentralClient.Socket.connected?/0
    ] ++ Application.fetch_env!(:air, :central_queue)

  defp handle_push(state, event, payload), do:
    state
    |> update_in([:queue], &Aircloak.Queue.push(&1, new_rpc(event, payload)))
    |> update_in([:queue], &Aircloak.Queue.drop_while(&1, fn(queue) -> queue.size > state.options.max_size end))
    |> maybe_start_rpc_task()

  defp new_rpc(event, payload), do:
    Air.Service.Central.new_rpc(
      # Generation of an almost unique id passed to the central. We can't use database ids, since in auto
      # export mode we're not storing RPCs to database. So instead, we're choosing id based on BEAM instance
      # unique integer and current time, which reduces the likelihood of two different RPCs with the same id.
      # Even if that happens, the damage is not big, since it will lead to one message not being imported into
      # central - a property which already exists, since we're not persisting pending RPCs.
      {:erlang.unique_integer(), NaiveDateTime.utc_now()},
      event,
      payload
    )

  defp maybe_start_rpc_task(%{send_paused?: true} = state), do:
    state
  defp maybe_start_rpc_task(%{current_send: current_send} = state) when current_send != nil, do:
    state
  defp maybe_start_rpc_task(%{queue: %Aircloak.Queue{size: 0}} = state), do:
    state
  defp maybe_start_rpc_task(state) do
    if state.options.connected_fun.() do
      {:value, rpc} = Aircloak.Queue.peek(state.queue)
      {:ok, pid} = Task.start_link(fn -> state.options.sender_fun.(rpc) end)
      %{state | current_send: %{pid: pid, rpc: rpc}}
    else
      pause_send(state)
    end
  end

  defp send_to_central(rpc), do:
    {:ok, _} = Air.CentralClient.Socket.rpc(rpc)

  defp send_finished(state, reason), do:
    state
    |> handle_finished_reason(reason)
    |> Map.put(:current_send, nil)
    |> maybe_start_rpc_task()

  defp handle_finished_reason(state, :normal), do:
    %{state | queue: Aircloak.Queue.drop_if(state.queue, &(&1 == state.current_send.rpc))}
  defp handle_finished_reason(state, abnormal_reason) do
    rpc = state.current_send.rpc
    Logger.error("RPC '#{rpc.event}' to central failed: #{inspect abnormal_reason}. Will retry later.")
    pause_send(state)
  end

  defp pause_send(state) do
    Process.send_after(self(), :resume_send, state.options.retry_delay)
    %{state | send_paused?: true}
  end
end
