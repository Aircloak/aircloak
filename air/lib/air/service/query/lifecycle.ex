defmodule Air.Service.Query.Lifecycle do
  @moduledoc """
  Serialized processing of query events.

  Since there might be multiple parallel events regarding a single query, such as results or state changes, we
  serialize these changes through query-specific process. Events for the same query are handled inside the same process,
  while events for different queries are handled in different processes.
  """

  alias Air.Service.Query
  require Logger
  use GenServer


  @type cloak_result :: %{
    query_id: String.t,
    columns: [String.t],
    features: map,
    error: String.t,
    info: [String.t],
    row_count: nil | non_neg_integer,
    rows: binary,
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Asynchronously handles query state change."
  @spec state_changed(String.t, Air.Schemas.Query.QueryState.t) :: :ok
  def state_changed(query_id, query_state), do:
    enqueue(query_id, {:state_changed, query_id, query_state})

  @doc "Asynchronously handles query result arrival."
  @spec result_arrived(cloak_result) :: :ok
  def result_arrived(result), do:
    enqueue(result.query_id, {:result_arrived, result})

  @doc "Asynchronously handles query's termination."
  @spec query_died(String.t) :: :ok
  def query_died(query_id), do:
    enqueue(query_id, {:query_died, query_id})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do:
    {:ok, nil}

  @impl GenServer
  def handle_cast({:result_arrived, result}, state) do
    Air.ProcessQueue.run(__MODULE__.Queue, fn -> Query.process_result(result) end)
    {:stop, :normal, state}
  end
  def handle_cast({:state_changed, query_id, query_state}, state) do
    Air.ProcessQueue.run(__MODULE__.Queue, fn -> Query.update_state(query_id, query_state) end)
    Air.ProcessQueue.run(__MODULE__.Queue, fn ->
      Air.Service.Query.Events.trigger_state_change(%{
        query_id: query_id,
        state: query_state,
      })
    end)
    {:noreply, state}
  end
  def handle_cast({:query_died, query_id}, state) do
    Air.ProcessQueue.run(__MODULE__.Queue, fn -> Query.query_died(query_id) end)
    Air.ProcessQueue.run(__MODULE__.Queue, fn ->
      Air.Service.Query.Events.trigger_state_change(%{
        query_id: query_id,
        state: :query_died,
      })
    end)
    {:stop, :normal, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @doc false
  def start_link(query_id), do:
    GenServer.start_link(__MODULE__, nil, name: name(query_id))

  defp name(query_id), do:
    {:via, Registry, {__MODULE__.Registry, query_id}}

  defp enqueue(query_id, message) do
    server_pid =
      case DynamicSupervisor.start_child(
        __MODULE__.QuerySupervisor,
        %{start: {__MODULE__, :start_link, [query_id]}, restart: :temporary, id: __MODULE__}
      ) do
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
    import Aircloak.ChildSpec, warn: false

    supervisor(
      [
        registry(:unique, __MODULE__.Registry),
        {Air.ProcessQueue, {__MODULE__.Queue, size: 5}},
        dynamic_supervisor(name: __MODULE__.QuerySupervisor)
      ],
      strategy: :rest_for_one, name: __MODULE__
    )
  end
end
