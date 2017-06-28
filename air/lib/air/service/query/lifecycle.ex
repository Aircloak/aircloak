defmodule Air.Service.Query.Lifecycle do
  @moduledoc """
  Serialized processing of query events.

  Since there might be multiple parallel events regarding a single query, such as results or state changes, we
  serialize these changes through query-specific process. Events for the same query are handled inside the same process,
  while events for different queries are handled in different processes.
  """

  import Supervisor.Spec, warn: false
  alias Air.Service.Query

  use GenServer


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a worker specification for the query result processor"
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec, do:
    supervisor(
      Supervisor,
      [
        [
          worker(Registry, [:unique, __MODULE__.Registry]),
          supervisor(Supervisor, [
            [worker(__MODULE__, [], restart: :temporary)],
            [strategy: :simple_one_for_one, name: __MODULE__.QuerySupervisor, id: __MODULE__.QuerySupervisor]
          ])
        ],
        [
          strategy: :rest_for_one,
          name: __MODULE__
        ]
      ]
    )

  @doc "Asynchronously handles query state change."
  @spec state_changed(String.t, Air.Schemas.Query.QueryState.t) :: :ok
  def state_changed(query_id, query_state), do:
    enqueue(query_id, {:state_changed, query_id, query_state})

  @doc "Asynchronously handles query result arrival."
  @spec result_arrived(map) :: :ok
  def result_arrived(result), do:
    enqueue(result.query_id, {:result_arrived, result})

  @doc "Asynchronously handles query's termination."
  @spec query_died(String.t) :: :ok
  def query_died(query_id), do:
    enqueue(query_id, {:query_died, query_id})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(nil), do:
    {:ok, nil}

  @doc false
  def handle_cast({:result_arrived, result}, state) do
    Query.process_result(result)
    {:stop, :normal, state}
  end
  def handle_cast({:state_changed, query_id, query_state}, state) do
    Query.update_state(query_id, query_state)
    {:noreply, state}
  end
  def handle_cast({:query_died, query_id}, state) do
    Query.query_died(query_id)
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
      case Supervisor.start_child(__MODULE__.QuerySupervisor, [query_id]) do
        {:ok, pid} -> pid
        {:error, {:already_started, pid}} -> pid
      end

    GenServer.cast(server_pid, message)
  end
end
