defmodule Air.Service.Query.Events do
  @moduledoc "Allows reporting and receiving lifetime events about Queries."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the registry for subscribers."
  @spec start_link() :: {:ok, pid}
  def start_link, do:
    Registry.start_link(:unique, __MODULE__.Registry)

  @doc """
  Registers the caller process as the subscriber for the query result.

  The process will receive the query result in the form of `{:query_result, result}`
  message. Upon receiving the message, the process should unsubscribe itself using
  `unsubscribe/1`.
  """
  @spec subscribe(String.t) :: :ok
  def subscribe(query_id) do
    {:ok, _} = Registry.register(__MODULE__.Registry, query_id, nil)
    :ok
  end

  @doc "Unsubscribes the registered query result subscriber."
  @spec unsubscribe(String.t) :: :ok
  def unsubscribe(query_id), do:
    Registry.unregister(__MODULE__.Registry, query_id)

  @doc "Triggers a :query_result event, indicating a result has been returned from the cloak for the query."
  @spec trigger_result(map) :: :ok
  def trigger_result(payload) do
    # notify dedicated listener for this query first
    case Registry.lookup(__MODULE__.Registry, payload.query_id) do
      [] -> :ok
      [{pid, nil}] ->
        send(pid, {:query_result, payload})
        :ok
    end
  end
end
