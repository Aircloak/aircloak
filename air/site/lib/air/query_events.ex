defmodule Air.QueryEvents do
  @moduledoc "Allows reporting and receiving lifetime events about Queries."

  use GenEvent

  @doc false
  @spec start_link() :: GenEvent.on_start
  def start_link do
    GenEvent.start_link(name: __MODULE__)
  end

  @doc "Triggers a :result event, indicating a result has been returned from the cloak for the query."
  @spec trigger_result(%{}) :: :ok
  def trigger_result(payload) do
    GenEvent.ack_notify(__MODULE__, {:result, payload})
  end

  @doc "Returns a stream of all events."
  @spec stream() :: GenEvent.Stream.t
  def stream do
    GenEvent.stream(__MODULE__)
  end
end
