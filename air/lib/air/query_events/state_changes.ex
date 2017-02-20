defmodule Air.QueryEvents.StateChanges do
  @moduledoc "Allows reporting and subscribing to query life cycle events."

  use GenEvent

  @type event :: :started | :completed


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  @spec start_link() :: GenEvent.on_start
  def start_link do
    import Supervisor.Spec, warn: false
    Supervisor.start_link([worker(GenEvent, [[name: __MODULE__]])], strategy: :one_for_one)
  end

  @doc "Triggers a :query_event indicating the state of a query."
  @spec trigger_event(String.t, event) :: :ok
  def trigger_event(query_id, event), do:
    GenEvent.ack_notify(__MODULE__, {:query_event, query_id, event})

  @doc "Returns a stream of all events."
  @spec stream() :: GenEvent.Stream.t
  def stream do
    GenEvent.stream(__MODULE__)
  end
end
