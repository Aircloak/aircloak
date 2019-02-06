defmodule AirWeb.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing types of messages:

  - __result__: reports new results as queries finish executing
  - __state_change__: reports the state changes of a query
  """
  use Air.Web, :channel

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts the change in the state of a query to all listening clients.
  """
  @spec broadcast_state_change(Air.Schemas.Query.t(), nil | [map]) :: :ok
  def broadcast_state_change(query, buckets \\ nil) do
    AirWeb.Endpoint.broadcast_from!(
      self(),
      "state_changes:all",
      "state_change",
      state_change_message(query)
    )

    payload = AirWeb.Query.for_display(query, authenticated?: true, buckets: buckets)

    AirWeb.Endpoint.broadcast_from!(
      self(),
      "user_queries:#{query.user_id}",
      "state_change",
      payload
    )

    AirWeb.Endpoint.broadcast_from!(self(), "query:#{query.id}", "state_change", payload)
    :ok
  end

  @doc """
  Broadcasts changes to a data sources selectables for a given user.
  This function is used when analyst generated selectables have changed.
  """
  @spec broadcast_analyst_selectables_change(Air.Schemas.User.t(), String.t()) :: :ok
  def broadcast_analyst_selectables_change(user, data_source_name) do
    AirWeb.Endpoint.broadcast_from!(
      self(),
      "selectables:#{data_source_name}",
      "selectables_change",
      %{user_id: user.id, data_source_name: data_source_name}
    )

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

  def join("selectables:" <> data_source_name, _, socket) do
    case Air.Service.DataSource.fetch_as_user({:name, data_source_name}, socket.assigns.user) do
      {:ok, data_source} -> {:ok, assign(socket, :data_source, data_source)}
      {:error, :unauthorized} -> {:error, %{reason: "Not authorized to subscribe to this data source"}}
    end
  end

  def join("state_changes:all", _, socket), do: accept_join_for_admins(socket)
  def join("query:" <> _query_id, _, socket), do: accept_join_for_admins(socket)

  def handle_in("delete_selectable", %{"internal_id" => id, "kind" => kind}, socket) do
    user = socket.assigns.user

    case kind do
      "view" -> Air.Service.View.delete(id, user, revalidation_timeout: :timer.seconds(5))
      "analyst_table" -> Air.Service.AnalystTable.delete(id, user)
    end

    # We start the broadcast in a task, as the caller will not receive the broadcast being the sender.
    # This is a hack to avoid having to implement a second API function for broadcasting changes.
    Task.start(fn ->
      broadcast_analyst_selectables_change(user, socket.assigns.data_source.name)
    end)

    {:noreply, socket}
  end

  intercept(["selectables_change"])

  def handle_out("selectables_change", %{user_id: user_id, data_source_name: data_source_name}, socket) do
    user = socket.assigns.user
    data_source = socket.assigns.data_source

    if user_id == user.id and data_source_name == data_source.name do
      push(socket, "selectables_change", %{
        selectables: AirWeb.ViewHelpers.selectables(user, data_source)
      })
    end

    {:noreply, socket}
  end

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

  defp state_change_message(query), do: %{query_id: query.id, event: query.query_state, query: format_query(query)}

  def format_query(query), do: hd(AirWeb.Admin.ActivityMonitorView.format_queries([query]))
end
