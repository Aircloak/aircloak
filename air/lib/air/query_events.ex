defmodule Air.QueryEvents do
  @moduledoc "Allows reporting and receiving lifetime events about Queries."

  use GenEvent

  @doc false
  @spec start_link() :: GenEvent.on_start
  def start_link do
    GenEvent.start_link(name: __MODULE__)
  end

  @doc """
  Registers the caller process as the subscriber for the query result.

  The process will receive the query result in the form of `{:query_result, result}`
  message. Upon receiving the message, the process should unsubscribe itself using
  `unsubscribe/1`.
  """
  @spec subscribe(String.t) :: true
  def subscribe(query_id), do:
    :gproc.reg(query_result_handler_name(query_id))

  @doc "Unsubscribes the registered query result subscriber."
  @spec unsubscribe(String.t) :: true
  def unsubscribe(query_id), do:
    :gproc.unreg(query_result_handler_name(query_id))

  @doc "Triggers a :result event, indicating a result has been returned from the cloak for the query."
  @spec trigger_result(%{}) :: :ok
  def trigger_result(payload) do
    # notify dedicated listener for this query first
    case :gproc.where(query_result_handler_name(Map.fetch!(payload, "query_id"))) do
      :undefined -> :ok
      pid -> send(pid, {:query_result, payload})
    end

    # notify all result observers
    GenEvent.ack_notify(__MODULE__, {:result, payload})
  end

  @doc "Returns a stream of all events."
  @spec stream() :: GenEvent.Stream.t
  def stream do
    GenEvent.stream(__MODULE__)
  end

  defp query_result_handler_name(query_id), do:
    {:n, :l, {__MODULE__, :query_result_handler, query_id}}
end
