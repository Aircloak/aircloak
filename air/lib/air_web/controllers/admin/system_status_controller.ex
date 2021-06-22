defmodule AirWeb.Admin.SystemStatusController do
  @moduledoc """
  Controller for administrators to get a view of the live state of their system.
  """

  use Air.Web, :admin_controller

  alias Plug.CSRFProtection
  alias Air.Service.{AuditLog, Cloak.Stats, Query}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(
      conn,
      "index.html",
      csrf_token: CSRFProtection.get_csrf_token(),
      socket_token: AirWeb.Plug.Session.current_token(conn),
      running_queries: Query.not_finished(),
      cloak_stats: Stats.cloak_stats(),
      login_events: AuditLog.login_events_stats(),
      query_stats: %{
        counts: AuditLog.query_stats(),
        users: Query.most_active_users()
      }
    )
  end
end
