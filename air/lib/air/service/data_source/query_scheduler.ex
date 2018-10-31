defmodule Air.Service.DataSource.QueryScheduler do
  @moduledoc "Synchronous starting of queries"

  use Parent.GenServer
  require Aircloak
  alias Air.Service.DataSource.QueryScheduler.Starter

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Notifies the scheduler that some queries are pending to be started."
  @spec notify() :: :ok
  def notify(), do: GenServer.cast(__MODULE__, :notify)

  # -------------------------------------------------------------------
  # GenServer and Parent.GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    schedule_start()
    {:ok, %{changed?: false}}
  end

  @impl GenServer
  def handle_cast(:notify, state), do: {:noreply, maybe_start_queries(%{state | changed?: true})}

  @impl GenServer
  def handle_call(:sync, _from, state) do
    with {:ok, query_starter_pid} <- Parent.GenServer.child_pid(:query_starter) do
      mref = Process.monitor(query_starter_pid)

      receive do
        {:DOWN, ^mref, _, _, _} -> :ok
      end
    end

    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_info(:start_queries, state) do
    schedule_start()
    {:noreply, maybe_start_queries(%{state | changed?: true})}
  end

  # this clause handles normal exits of parallel tasks which are started when ecto preloads associations
  def handle_info({:EXIT, _pid, :normal}, state), do: {:noreply, :ok, state}

  @impl Parent.GenServer
  def handle_child_terminated(:query_starter, _meta, _pid, _reason, state), do: {:noreply, maybe_start_queries(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # A safeguard to ensure that we'll start the queries even if we missed some notification.
  defp schedule_start(),
    do: Aircloak.in_env(test: nil, else: Process.send_after(self(), :start_queries, :timer.minutes(1)))

  @doc false
  # Needed in tests to ensure synchronism
  def sync(), do: GenServer.call(__MODULE__, :sync)

  defp maybe_start_queries(state) do
    if state.changed? and not Parent.GenServer.child?(:query_starter) do
      Parent.GenServer.start_child(%{id: :query_starter, start: {Task, :start_link, [&Starter.run/0]}})
      %{state | changed?: false}
    else
      state
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
