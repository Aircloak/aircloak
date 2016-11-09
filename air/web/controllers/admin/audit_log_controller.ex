defmodule Air.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller


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
      page: params["page"] || 1,
      users: params["users"] || [],
      events: params["events"] || [],
      data_sources: params["data_sources"] || [],
    }
    render(conn, "index.html",
      audit_logs: Air.Service.AuditLog.for(service_params),
      full_width: true,
      users: Air.Service.User.all(),
      event_types: event_types(service_params),
      data_sources: Air.Service.AuditLog.data_sources(service_params),
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp event_types(params) do
    already_selected_event_types = params["events"] || []
    combined_event_types = Air.Service.AuditLog.event_types(params) ++
      already_selected_event_types
    Enum.uniq(combined_event_types)
  end
end
