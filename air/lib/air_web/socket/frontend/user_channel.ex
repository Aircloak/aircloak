defmodule AirWeb.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing types of messages:

  - __result__: reports new results as queries finish executing
  - __state_change__: reports the state changes of a query
  """
  use Air.Web, :channel

  alias Air.Schemas


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts the change in the state of a query to all listening clients.
  """
  @spec broadcast_state_change(Schemas.Query.t, nil | [map]) :: :ok
  def broadcast_state_change(query, buckets \\ nil) do
    AirWeb.Endpoint.broadcast_from!(self(), "state_changes:all", "state_change", state_change_message(query))
    payload = Schemas.Query.for_display(query, buckets)
    AirWeb.Endpoint.broadcast_from!(self(), "user_queries:#{query.user_id}", "state_change", payload)
    AirWeb.Endpoint.broadcast_from!(self(), "query:#{query.id}", "state_change", payload)
    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("user_queries:" <> user_id, _, socket) do
    current_user_id = socket.assigns.user.id
    case Integer.parse(user_id) do
      {^current_user_id, ""} -> {:ok, socket}
      _ -> {:error, %{success: false, description: "Forbidden"}}
    end
  end
  def join("state_changes:all", _, socket), do:
    accept_join_for_admins(socket)
  def join("query:" <> _query_id, _, socket), do:
    accept_join_for_admins(socket)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp accept_join_for_admins(socket) do
    user = socket.assigns.user
    if Air.Schemas.User.admin?(user) do
      {:ok, socket}
    else
      {:error, %{reason: "Only admin users are allowed to connect"}}
    end
  end

  defp state_change_message(query), do:
    %{query_id: query.id, event: query.query_state, query: format_query(query)}

  def format_query(query), do:
    hd(AirWeb.Admin.ActivityMonitorView.format_queries([query]))
end
