defmodule Air.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.AuditLog


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index]
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
      data_sources: (params["data_sources"] || []) |> Enum.map(&String.to_integer/1),
    }
    render(conn, "index.html",
      audit_logs: AuditLog.for(service_params),
      full_width: true,
      users: AuditLog.users(service_params),
      event_types: AuditLog.event_types(service_params),
      data_sources: AuditLog.data_sources(service_params),
    )
  end
end
