defmodule Air.QueryEvents do
  @moduledoc "Allows reporting and receiving lifetime events about Queries."

  use GenEvent

  @registry_name Module.concat(__MODULE__, Registry)

  @doc false
  @spec start_link() :: GenEvent.on_start
  def start_link do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [
        supervisor(Registry, [:unique, @registry_name]),
        worker(GenEvent, [[name: __MODULE__]])
      ],
      strategy: :one_for_one
    )
  end

  @doc """
  Registers the caller process as the subscriber for the query result.

  The process will receive the query result in the form of `{:query_result, result}`
  message. Upon receiving the message, the process should unsubscribe itself using
  `unsubscribe/1`.
  """
  @spec subscribe(String.t) :: :ok
  def subscribe(query_id) do
    {:ok, _} = Registry.register(@registry_name, query_id, nil)
    :ok
  end

  @doc "Unsubscribes the registered query result subscriber."
  @spec unsubscribe(String.t) :: :ok
  def unsubscribe(query_id), do:
    Registry.unregister(@registry_name, query_id)

  @doc "Triggers a :query_result event, indicating a result has been returned from the cloak for the query."
  @spec trigger_result(map) :: :ok
  def trigger_result(payload) do
    # notify dedicated listener for this query first
    case Registry.lookup(@registry_name, Map.fetch!(payload, "query_id")) do
      [] -> :ok
      [{pid, nil}] ->
        send(pid, {:query_result, payload})
        :ok
    end

    # notify all result observers
    GenEvent.ack_notify(__MODULE__, {:query_result, payload})
  end

  @doc "Triggers a :state_change event, indicating an update in the processing state of the query."
  @spec trigger_state_change(String.t, Air.Schemas.Query.QueryStatus.t) :: :ok
  def trigger_state_change(query_id, state), do:
    GenEvent.ack_notify(__MODULE__, {:query_state_change, query_id, state})

  @doc """
  Triggers a :query_died event, indicating the process responsible for the query went away without sending a result.
  """
  @spec trigger_query_died(String.t) :: :ok
  def trigger_query_died(query_id), do:
    GenEvent.ack_notify(__MODULE__, {:query_died, query_id})

  @doc "Returns a stream of all events."
  @spec stream() :: GenEvent.Stream.t
  def stream do
    GenEvent.stream(__MODULE__)
  end
end
