defmodule Air.QueryEvents.StateChanges do
  @moduledoc "Allows reporting of query life cycle events."

  @type event :: :started | :completed


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Triggers a :query_event indicating the state of a query."
  @spec trigger_event(String.t, event, Map.t) :: :ok
  def trigger_event(query_id, event, metadata \\ %{}), do:
    Air.Socket.Frontend.UserChannel.broadcast_query_state_change(query_id, event, metadata)
end
