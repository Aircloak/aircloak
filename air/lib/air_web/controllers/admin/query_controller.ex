defmodule AirWeb.Admin.QueryController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  require Logger
  alias Air.Service.Query
  alias Plug.CSRFProtection

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def active(conn, _params) do
    render(
      conn,
      "active.html",
      csrf_token: CSRFProtection.get_csrf_token(),
      socket_token: AirWeb.Plug.Session.current_token(conn),
      running_queries: Query.not_finished()
    )
  end

  def show(conn, %{"id" => query_id}) do
    case Query.get_as_user(conn.assigns.current_user, query_id) do
      {:ok, query} ->
        render(conn, %{
          query: AirWeb.Query.for_display(query, authenticated?: true, buckets: Query.buckets(query, 0)),
          socket_token: AirWeb.Plug.Session.current_token(conn),
          csrf_token: CSRFProtection.get_csrf_token(),
          number_format: Air.Service.User.number_format_settings(conn.assigns.current_user),
          debug_mode_enabled: conn.assigns.current_user.debug_mode_enabled
        })

      {:error, _} ->
        not_found(conn)
    end
  end

  def failed(conn, params) do
    filters = %{
      from: parse_datetime(params["from"], Timex.now() |> Timex.shift(days: -1)),
      to: parse_datetime(params["to"], Timex.now()),
      users: params["users"] || [],
      data_sources: params["data_sources"] || [],
      query_states: [:error],
      max_results: 100
    }

    render(
      conn,
      "failed.html",
      Map.merge(filters, %{
        full_width: true,
        csrf_token: CSRFProtection.get_csrf_token(),
        number_format: Air.Service.User.number_format_settings(conn.assigns.current_user),
        debug_mode_enabled: conn.assigns.current_user.debug_mode_enabled,
        failed_queries: Query.queries(filters) |> Enum.map(&AirWeb.Query.for_display/1),
        users:
          Query.users_for_filters(%{filters | users: [], data_sources: []})
          |> Enum.map(&%{label: &1.name, value: &1.id}),
        data_sources:
          Query.data_sources_for_filters(%{filters | users: [], data_sources: []})
          |> Enum.map(&%{label: &1.name, value: &1.id})
      })
    )
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp parse_datetime(value, default) do
    case Timex.parse(value, "{ISOdate} {ISOtime}") do
      {:ok, result} -> result
      _error -> default
    end
  end
end
