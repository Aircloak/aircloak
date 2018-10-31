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
  def init(initial_state), do: {:ok, Map.put(initial_state, :changed?, false), initial_state.idle_timeout}

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
  def handle_info(:timeout, state), do: {:noreply, maybe_start_queries(%{state | changed?: true})}

  @impl Parent.GenServer
  def handle_child_terminated(:query_starter, _meta, _pid, reason, state) do
    timeout = if reason == :normal, do: state.idle_timeout, else: state.failure_timeout
    {:noreply, maybe_start_queries(state), timeout}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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
    opts = Keyword.merge(default_opts(), opts)

    gen_server_opts =
      case Keyword.get(opts, :name) do
        nil -> []
        name -> [name: name]
      end

    initial_state = opts |> Keyword.take(~w/runner idle_timeout failure_timeout/a) |> Map.new()
    Parent.GenServer.start_link(__MODULE__, initial_state, gen_server_opts)
  end

  defp default_opts() do
    [
      name: __MODULE__,
      runner: &__MODULE__.Starter.run/0,
      idle_timeout: Aircloak.in_env(test: :infinity, else: :timer.minutes(1)),
      failure_timeout: Aircloak.in_env(test: :infinity, else: :timer.seconds(10))
    ]
  end
end
