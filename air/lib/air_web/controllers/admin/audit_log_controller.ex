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
    service_params = %{
      page: String.to_integer(params["page"] || "1"),
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: (params["data_sources"] || []),
    }
    render(conn, "index.html",
      audit_logs: AuditLog.for(service_params),
      full_width: true,
      users: AuditLog.users(service_params),
      event_types: AuditLog.event_types(service_params),
      data_sources: AuditLog.data_sources(service_params)
    )
  end

  def confirm_deletion(conn, _params), do:
    render(conn, "confirm_deletion.html", entries_count: AuditLog.count())

  def delete_all(conn, _params) do
    Repo.delete_all(Air.Schemas.AuditLog)
    conn
    |> put_flash(:info, "All audit log entries have been deleted")
    |> redirect(to: admin_settings_path(conn, :show))
  end
end
