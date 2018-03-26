defmodule Aircloak.ProcessMonitor do
  @moduledoc "Allows invoking a custom function when a process terminates."
  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the supervisor for process monitors."
  @spec start_link() :: Supervisor.on_start()
  def start_link() do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [worker(GenServer, [__MODULE__], restart: :temporary)],
      name: __MODULE__,
      strategy: :simple_one_for_one
    )
  end

  @doc "Registers a callback function which is invoked when the caller process terminates."
  @spec on_exit((() -> any)) :: :ok
  def on_exit(exit_callback) do
    {:ok, _} = Supervisor.start_child(__MODULE__, [{self(), exit_callback}])
    :ok
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({monitored_process, exit_callback}),
    do:
      {:ok,
       %{
         mref: Process.monitor(monitored_process),
         exit_callback: exit_callback
       }}

  @impl GenServer
  def handle_info({:DOWN, mref, :process, _, _}, %{mref: mref} = state) do
    state.exit_callback.()
    {:stop, :normal, %{state | mref: nil}}
  end

  def handle_info(_, state), do: {:noreply, state}
end
