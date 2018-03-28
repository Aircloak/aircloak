defmodule AirWeb.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.AuditLog

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index, :confirm_deletion, :delete_all]
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, params) do
    from = parse_datetime(params["from"], Timex.now() |> Timex.shift(days: -1))
    to = parse_datetime(params["to"], Timex.now())
    max_results = 1000

    filters = %{
      from: from,
      to: to,
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: params["data_sources"] || [],
      max_results: max_results
    }

    audit_logs = AuditLog.for(filters)

    render(
      conn,
      "index.html",
      audit_logs: audit_logs,
      full_width: true,
      users: AuditLog.users(filters),
      event_types: AuditLog.event_types(filters),
      data_sources: AuditLog.data_sources(filters),
      from: from,
      to: to,
      max_results: max_results
    )
  end

  def confirm_deletion(conn, _params), do: render(conn, "confirm_deletion.html", entries_count: AuditLog.count())

  def delete_all(conn, _params) do
    Repo.delete_all(Air.Schemas.AuditLog)

    conn
    |> put_flash(:info, "All audit log entries have been deleted")
    |> redirect(to: admin_settings_path(conn, :show))
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
