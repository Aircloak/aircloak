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
  @spec broadcast_analyst_selectables_change(Air.Schemas.User.t(), Air.Schemas.DataSource.t()) :: :ok
  if Mix.env() == :test do
    def broadcast_analyst_selectables_change(_user, _data_source), do: :ignored
  else
    def broadcast_analyst_selectables_change(user, data_source) do
      AirWeb.Endpoint.broadcast_from!(
        self(),
        "selectables:#{data_source.name}:#{user.id}",
        "selectables_change",
        selectable_payload(user, data_source)
      )
    end
  end

  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("user_queries:" <> user_id, _, socket) do
    if user_id_matches_user(user_id, socket) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Forbidden"}}
    end
  end

  def join("selectables:" <> params, _, socket) do
    [data_source_name, user_id] = String.split(params, ":")

    if user_id_matches_user(user_id, socket) do
      case Air.Service.DataSource.fetch_as_user({:name, data_source_name}, socket.assigns.user) do
        {:ok, data_source} ->
          initial_selectables = selectable_payload(socket.assigns.user, data_source)
          {:ok, initial_selectables, assign(socket, :data_source, data_source)}

        {:error, :unauthorized} ->
          {:error, %{reason: "Not authorized to subscribe to this data source"}}
      end
    else
      {:error, %{reason: "Not authorized to subscribe to this data source"}}
    end
  end

  def join("state_changes:all", _, socket), do: accept_join_for_admins(socket)
  def join("query:" <> _query_id, _, socket), do: accept_join_for_admins(socket)

  def join("type_check:" <> user_id, _, socket) do
    if user_id_matches_user(user_id, socket) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Forbidden"}}
    end
  end

  def handle_in("delete_selectable", %{"internal_id" => id, "kind" => kind}, socket) do
    user = socket.assigns.user

    case kind do
      "view" -> Air.Service.View.delete(id, user, revalidation_timeout: :timer.seconds(5))
      "analyst_table" -> Air.Service.AnalystTable.delete(id, user)
    end

    broadcast(socket, "selectables_change", selectable_payload(user, socket.assigns.data_source))

    {:noreply, socket}
  end

  def handle_in("type_check", %{"query" => statement, "data_source" => data_source}, socket) do
    user = socket.assigns.user

    type_check_result =
      Air.Service.DataSource.with_available_cloak(
        {:name, data_source},
        user,
        &AirWeb.Socket.Cloak.MainChannel.type_check_query(
          &1.channel_pid,
          user.id,
          statement,
          &1.data_source.name,
          Air.Service.View.user_views(user, &1.data_source.id)
        )
      )

    case type_check_result do
      {:ok, result} ->
        {:reply, {:ok, %{"result" => result}}, socket}

      {:error, error} ->
        potential_reason =
          case error do
            :unauthorized -> " as you are not authorized to query this data source"
            :not_connected -> " as the data source is offline"
            :internal_error -> " due to an internal error"
            _ -> ""
          end

        error_message = %{
          type: "SystemError",
          message: "Could not type check the query #{potential_reason}",
          location: nil
        }

        {:reply, {:ok, %{"result" => %{data: error_message}}}, socket}
    end
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

  defp selectable_payload(user, data_source) do
    explorer_results = Air.Service.Explorer.results_for_datasource(data_source)

    selectables =
      Air.Service.DataSource.selectables(user, data_source)
      |> Enum.sort_by(& &1.id)
      |> Enum.map(fn table ->
        result = Enum.find(explorer_results, fn result -> result.table_name == table.id end)

        if result do
          table
          |> update_in([:columns, Access.all()], fn column ->
            column_metrics =
              Enum.find(result.results["columns"] || [], %{"metrics" => []}, fn %{"column" => column_name} ->
                column_name == column.name
              end)["metrics"]

            if is_nil(column_metrics) or Enum.empty?(column_metrics) do
              column
            else
              Map.put(column, :analysis, [
                %{name: "updated_at", value: result.updated_at} | column_metrics
              ])
            end
          end)
          |> put_in([:sample_data], result.results["sampleData"])
        else
          table
        end
      end)

    %{selectables: selectables}
  end

  defp user_id_matches_user(user_id_string, socket) when is_binary(user_id_string) do
    case(Integer.parse(user_id_string)) do
      {id, ""} -> user_id_matches_user(id, socket)
      _ -> false
    end
  end

  defp user_id_matches_user(user_id, socket), do: user_id == socket.assigns.user.id
end
