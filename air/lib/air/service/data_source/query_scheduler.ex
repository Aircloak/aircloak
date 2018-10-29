defmodule Air.Service.DataSource.QueryScheduler do
  @moduledoc "Synchronous starting of queries"

  use Parent.GenServer

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
  def init(nil), do: {:ok, %{changed?: false}}

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
  # this clause handles normal exits of parallel tasks which are started when ecto preloads associations
  def handle_info({:EXIT, _pid, :normal}, state), do: {:noreply, :ok, state}

  @impl Parent.GenServer
  def handle_child_terminated(:query_starter, _meta, _pid, _reason, state), do: {:noreply, maybe_start_queries(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp maybe_start_queries(state) do
    if state.changed? and not Parent.GenServer.child?(:query_starter) do
      Parent.GenServer.start_child(%{id: :query_starter, start: {Task, :start_link, [&start_pending_queries/0]}})
      %{state | changed?: false}
    else
      state
    end
  end

  defp start_pending_queries(),
    do: Enum.each(Air.Service.Query.awaiting_start(), &Air.Service.DataSource.start_query(&1, {:id, &1.data_source.id}))

  @doc false
  # Needed in tests to ensure synchronism
  def sync(), do: GenServer.call(__MODULE__, :sync)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
