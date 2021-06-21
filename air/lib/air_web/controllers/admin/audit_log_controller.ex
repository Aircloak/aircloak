defmodule AirWeb.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.AuditLog

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  @spec confirm_deletion(Plug.Conn.t(), any) :: Plug.Conn.t()
  def confirm_deletion(conn, _params), do: render(conn, "confirm_deletion.html", entries_count: AuditLog.count())

  def delete_all(conn, _params) do
    Repo.delete_all(Air.Schemas.AuditLog)

    conn
    |> put_flash(:info, "All audit log entries have been deleted.")
    |> redirect(to: admin_settings_path(conn, :show))
  end
end
