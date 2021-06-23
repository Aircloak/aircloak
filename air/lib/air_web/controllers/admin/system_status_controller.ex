defmodule AirWeb.Admin.SystemStatusController do
  @moduledoc """
  Controller for administrators to get a view of the live state of their system.
  """

  use Air.Web, :admin_controller

  alias Plug.CSRFProtection
  alias Air.Service.{AuditLog, Cloak.Stats, Query, User, Warnings}

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
      },
      token_stats: User.token_stats(),
      problems: Warnings.problems()
    )
  end

  def warnings(conn, _params), do: render(conn, "warnings.html", problems: Air.Service.Warnings.problems())

  @doc """
  Redirects to the warnings index if there are any, otherwise
  to the activity monitor, which is the default admin page.
  """
  def warnings_if_any(conn, _params) do
    if length(Warnings.problems()) > 0 do
      redirect(conn, to: admin_system_status_path(conn, :warnings))
    else
      redirect(conn, to: admin_system_status_path(conn, :index))
    end
  end
end
