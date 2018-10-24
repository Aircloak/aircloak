defmodule Air.Service.Query.Lifecycle do
  @moduledoc """
  Serialized processing of query events.

  Since there might be multiple parallel events regarding a single query, such as results or state changes, we
  serialize these changes through query-specific process. Events for the same query are handled inside the same process,
  while events for different queries are handled in different processes.
  """

  alias Aircloak.ChildSpec
  alias Air.Service.Query
  require Logger
  use GenServer

  @type cloak_result :: %{
          query_id: String.t(),
          columns: [String.t()],
          features: map,
          error: String.t(),
          info: [String.t()],
          row_count: nil | non_neg_integer,
          rows: binary
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Asynchronously handles query state change."
  @spec state_changed(String.t(), Air.Schemas.Query.QueryState.t()) :: :ok
  def state_changed(query_id, query_state), do: enqueue(query_id, {:state_changed, query_id, query_state})

  @doc "Asynchronously handles query result arrival."
  @spec result_arrived(cloak_result) :: :ok
  def result_arrived(result), do: enqueue(result.query_id, {:result_arrived, result})

  @doc "Asynchronously handles query's termination."
  @spec query_died(String.t()) :: :ok
  def query_died(query_id), do: enqueue(query_id, {:query_died, query_id})

  @doc "Asynchronously reports query error."
  @spec report_query_error(String.t(), String.t()) :: :ok
  def report_query_error(query_id, error), do: enqueue(query_id, {:report_query_error, query_id, error})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do: {:ok, nil}

  @impl GenServer
  def handle_cast({:result_arrived, result}, state) do
    :jobs.run(__MODULE__, fn -> Query.process_result(result) end)
    {:stop, :normal, state}
  end

  def handle_cast({:state_changed, query_id, query_state}, state) do
    :jobs.run(__MODULE__, fn -> Query.update_state(query_id, query_state) end)
    Air.Service.Query.Events.trigger_state_change(%{query_id: query_id, state: query_state})
    {:noreply, state}
  end

  def handle_cast({:query_died, query_id}, state) do
    :jobs.run(__MODULE__, fn -> Query.query_died(query_id) end)
    Air.Service.Query.Events.trigger_state_change(%{query_id: query_id, state: :query_died})
    {:stop, :normal, state}
  end

  def handle_cast({:report_query_error, query_id, error}, state) do
    :jobs.run(__MODULE__, fn -> Query.process_result(%{query_id: query_id, error: error, row_count: 0, chunks: []}) end)
    {:stop, :normal, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @doc false
  def start_link(query_id), do: GenServer.start_link(__MODULE__, nil, name: name(query_id))

  @doc false
  def setup_queue() do
    with :undefined <- :jobs.queue_info(__MODULE__),
         do: :jobs.add_queue(__MODULE__, max_time: :timer.hours(1), regulators: [counter: [limit: 5]])

    :proc_lib.init_ack({:ok, self()})
  end

  defp name(query_id), do: {:via, Registry, {__MODULE__.Registry, query_id}}

  defp enqueue(query_id, message) do
    server_pid =
      case DynamicSupervisor.start_child(__MODULE__.QuerySupervisor, %{
             start: {__MODULE__, :start_link, [query_id]},
             restart: :temporary,
             id: __MODULE__
           }) do
        {:ok, pid} -> pid
        {:error, {:already_started, pid}} -> pid
      end

    GenServer.cast(server_pid, message)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        setup_queue_spec(),
        ChildSpec.registry(:unique, __MODULE__.Registry),
        ChildSpec.dynamic_supervisor(name: __MODULE__.QuerySupervisor)
      ],
      strategy: :rest_for_one,
      name: __MODULE__
    )
  end

  defp setup_queue_spec() do
    %{
      id: :setup_queue,
      # using :proc_lib ensures that the supervisor will start the next child only after the queue has been setup
      start: {:proc_lib, :start_link, [__MODULE__, :setup_queue, []]},
      restart: :transient
    }
  end
end
