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
    entries = Air.Service.AuditLog.for(%{
      page: params["page"] || 1,
      users: params["users"] || [],
      events: params["events"] || [],
    })
    render(conn, "index.html",
      audit_logs: entries,
      full_width: true,
      users: Air.Service.User.all(),
      event_types: event_types(params),
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp event_types(params) do
    already_selected_event_types = params["events"] || []
    combined_event_types = Air.Service.AuditLog.event_types(params["users"] || []) ++
      already_selected_event_types
    Enum.uniq(combined_event_types)
  end
end
