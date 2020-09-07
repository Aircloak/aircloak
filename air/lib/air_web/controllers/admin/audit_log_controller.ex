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

  def confirm_deletion(conn, _params), do: render(conn, "confirm_deletion.html", entries_count: AuditLog.count())

  def delete_all(conn, _params) do
    Repo.delete_all(Air.Schemas.AuditLog)

    conn
    |> put_flash(:info, "All audit log entries have been deleted")
    |> redirect(to: admin_settings_path(conn, :show))
  end
end
