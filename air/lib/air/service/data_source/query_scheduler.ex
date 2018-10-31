defmodule Air.Service.DataSource.QueryScheduler do
  @moduledoc "Synchronous starting of queries"

  use Parent.GenServer
  require Aircloak

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Notifies the scheduler that some queries are pending to be started."
  @spec notify(GenServer.server()) :: :ok
  def notify(server \\ __MODULE__), do: GenServer.cast(server, :notify)

  # -------------------------------------------------------------------
  # GenServer and Parent.GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(opts) do
    schedule_start()
    {:ok, %{changed?: false, runner: Keyword.fetch!(opts, :runner)}}
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
      Parent.GenServer.start_child(%{id: :query_starter, start: {Task, :start_link, [state.runner]}})
      %{state | changed?: false}
    else
      state
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(opts) do
    opts = Keyword.merge([runner: &__MODULE__.Starter.run/0], opts)

    {gen_server_opts, opts} =
      case Keyword.pop(opts, :name, __MODULE__) do
        {nil, opts} -> {[], opts}
        {name, opts} -> {[name: name], opts}
      end

    Parent.GenServer.start_link(__MODULE__, opts, gen_server_opts)
  end
end
