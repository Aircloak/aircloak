defmodule Air.Service.Query.Events do
  @moduledoc "Allows reporting and receiving lifetime events about Queries."

  use Aircloak.ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the registry for subscribers."
  @spec start_link() :: {:ok, pid}
  def start_link, do:
    Registry.start_link(:duplicate, __MODULE__.Registry)

  @doc """
  Registers the caller process as the subscriber for the query result and state updates.

  The process will receive the query state changes in the form of `{:query_state_change, payload}`
  messages. The query result in the form of a `{:query_result, result}` message.

  Upon receiving the query result message, the process should unsubscribe itself using `unsubscribe/1`.
  """
  @spec subscribe(String.t) :: :ok
  def subscribe(query_id) do
    {:ok, _} = Registry.register(__MODULE__.Registry, {:subscriber, query_id}, nil)
    :ok
  end

  @doc "Unsubscribes the registered query result subscriber."
  @spec unsubscribe(String.t) :: :ok
  def unsubscribe(query_id), do:
    Registry.unregister(__MODULE__.Registry, {:subscriber, query_id})

  @doc "Notifies subscribers that a result has been returned from the cloak. See `subscribe/1` for more details."
  @spec trigger_result(map) :: :ok
  def trigger_result(payload) do
    for {pid, _} <- Registry.lookup(__MODULE__.Registry, {:subscriber, payload.query_id}), do:
      send(pid, {:query_result, payload})
    :ok
  end

  @doc """
  Notifies subscribers that a query state change has been returned from the cloak.
  See `subscribe/1` for more details.
  """
  @spec trigger_state_change(map) :: :ok
  def trigger_state_change(payload) do
    for {pid, _} <- Registry.lookup(__MODULE__.Registry, {:subscriber, payload.query_id}), do:
      send(pid, {:query_state_change, payload})
    :ok
  end
end
