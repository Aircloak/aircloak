defmodule Air.Admin.CentralController do
  @moduledoc false
  use Air.Web, :admin_controller
  alias Air.Service.Central
  alias Air.Schemas.ExportForAircloak

  require Logger

  plug :verify_manual_export_allowed when action in [:export, :new_export, :download_export]


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

  def export(conn, params), do:
    conn
    |> maybe_download_generated_export()
    |> render(oldest_pending_call_time: Central.oldest_pending_call_time(), exports: exports(params))

  def new_export(conn, _params) do
    case Central.export_pending_calls() do
      {:ok, export} ->
        conn
        |> put_session("download_generated_export", export.id)
        |> put_flash(:info, "Export generated successfully, download will begin shortly.")
      {:error, :nothing_to_export} ->
        put_flash(conn, :error, "Nothing to export")
      other_error ->
        Logger.error("Export for aircloak error: #{inspect other_error}")
        put_flash(conn, :error, "An error has occurred. No export was generated.")
    end
    |> redirect(to: admin_central_path(conn, :export))
  end

  def download_export(conn, %{"export_id" => export_id}) do
    export = Central.get_export!(export_id)
    send_attachment(conn, ExportForAircloak.file_name(export), ExportForAircloak.content(export))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp verify_manual_export_allowed(conn, _params) do
    if Central.manual_export?() do
      conn
    else
      conn
      |> redirect(to: admin_user_path(conn, :index))
      |> halt()
    end
  end

  defp maybe_download_generated_export(conn) do
    case get_session(conn, "download_generated_export") do
      nil -> conn
      export_id ->
        conn
        |> assign(:refresh, admin_central_path(conn, :download_export, export_id))
        |> delete_session("download_generated_export")
    end
  end

  defp send_attachment(conn, file_name, data), do:
    conn
    |> put_resp_content_type("application/octet-stream")
    |> put_resp_header("content-disposition", ~s[attachment; filename="#{file_name}"])
    |> send_resp(200, data)

  defp exports(params), do:
    Central.exports(String.to_integer(params["page"] || "1"), 10)
end
