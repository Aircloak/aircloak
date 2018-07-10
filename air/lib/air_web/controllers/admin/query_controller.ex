defmodule AirWeb.Admin.QueryController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  require Logger
  alias Air.Schemas.Query
  alias Plug.CSRFProtection

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{admin: :all}
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, %{"id" => query_id}) do
    case Air.Service.Query.get_as_user(conn.assigns.current_user, query_id) do
      {:ok, query} ->
        render(conn, %{
          query: Query.for_display(query, Air.Service.Query.buckets(query, 0)),
          guardian_token: Air.Guardian.Plug.current_token(conn),
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
      data_sources: [],
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
        failed_queries: Air.Service.Query.queries(filters) |> Enum.map(&Query.for_display(&1, nil)),
        users: Air.Service.Query.users_for_filters(filters) |> Enum.map(&%{label: &1.name, value: &1.id}),
        data_sources: Air.Service.Query.data_sources_for_filters(filters) |> Enum.map(&%{label: &1.name, value: &1.id})
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
