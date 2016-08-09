defmodule Air.AuditLogController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.AuditLog


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

  def index(conn, _params) do
    audit_logs = Repo.all(from a in AuditLog, order_by: [desc: :inserted_at])
    render(conn, "index.html", audit_logs: audit_logs)
  end
end
