defmodule Air.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __result__: reports new results as queries finish executing
  - __state_change__: reports the state changes of a query
  """
  use Air.Web, :channel

  alias Air.Schemas.Query


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts the results of a query execution to all listening clients.
  """
  @spec broadcast_result(Query.t) :: :ok
  def broadcast_result(query) do
    Air.Endpoint.broadcast_from!(self(), "session:#{query.session_id}", "result", Query.for_display(query))
    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("session:" <> session_id, _, socket) do
    case Ecto.UUID.cast(session_id) do
      {:ok, _} -> {:ok, socket}
      _ -> {:error, %{success: false, description: "Channel not found"}}
    end
  end
  def join("state_changes:all", _, socket) do
    send(self(), {:stream_state_changes, :all})
    {:ok, socket}
  end

  def handle_info({:stream_state_changes, :all}, socket) do
    Task.start_link(fn() ->
      for {:query_event, query_id, event} <- Air.QueryEvents.StateChanges.stream() do
        push(socket, "state_change", message_for_event(event, query_id))
      end
    end)
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp message_for_event(:started, query_id) do
    {:ok, query} = Air.Service.Query.get(query_id)
    query_data = %{
      started_at: query.inserted_at,
      user: %{
        name: query.user.name,
      },
      data_source: %{
        name: query.data_source.name,
      },
    }
    %{query_id: query_id, event: :started, query: query_data}
  end
  defp message_for_event(:completed, query_id), do:
    %{query_id: query_id, event: :completed}
end
