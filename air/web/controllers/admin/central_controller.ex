defmodule Air.Admin.CentralController do
  @moduledoc false
  use Air.Web, :admin_controller
  alias Air.Service.Central

  require Logger


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def export(conn, _params) do
    render conn, oldest_pending_call_time: Central.oldest_pending_call_time()
  end

  def new_export(conn, _params) do
    case Central.export_pending_calls() do
      {:ok, export} ->
        send_attachment(conn, "export_for_aircloak.acd", export.payload)
      {:error, :nothing_to_export} ->
        conn
        |> put_flash(:error, "Nothing to export")
        |> redirect(to: admin_central_path(conn, :export))
      other_error ->
        Logger.error("Export for aircloak error: #{inspect other_error}")
        conn
        |> put_flash(:error, "An error has occurred, export not generated.")
        |> redirect(to: admin_central_path(conn, :export))
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp send_attachment(conn, file_name, data), do:
    conn
    |> put_resp_content_type("application/octet-stream")
    |> put_resp_header("content-disposition", ~s[attachment; filename="#{file_name}"])
    |> send_resp(200, data)
end
