defmodule Air.Service.DataSource.QueryScheduler do
  @moduledoc "Synchronous starting of queries"

  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Notifies the scheduler that some queries are pending to be started."
  @spec notify() :: :ok
  def notify(), do: GenServer.cast(__MODULE__, :notify)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do: {:ok, nil}

  @impl GenServer
  def handle_cast(:notify, state) do
    start_pending_queries()
    {:noreply, state}
  end

  @impl GenServer
  def handle_call(:sync, _from, state), do: {:reply, :ok, state}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_pending_queries(),
    do: Enum.each(Air.Service.Query.not_started(), &Air.Service.DataSource.start_query(&1, {:id, &1.data_source.id}))

  @doc false
  # Needed in tests to ensure synchronism
  def sync(), do: GenServer.call(__MODULE__, :sync)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
