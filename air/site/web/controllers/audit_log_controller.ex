defmodule Air.AuditLogController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.AuditLog
  alias Poison, as: JSON


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
    |> Enum.map(&AuditLog.for_display/1)
    |> Poison.encode!()
    render(conn, "index.html", audit_logs: audit_logs)
  end
end
