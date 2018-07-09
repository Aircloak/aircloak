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
    from = parse_datetime(params["from"], Timex.now() |> Timex.shift(days: -1))
    to = parse_datetime(params["to"], Timex.now())
    max_results = 100
    failed_queries = Air.Service.Query.failed_queries()

    render(
      conn,
      "failed.html",
      full_width: true,
      failed_queries: failed_queries,
      from: from,
      to: to,
      max_results: max_results
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
