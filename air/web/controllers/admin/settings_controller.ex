defmodule Air.Admin.SettingsController do
  @moduledoc false
  use Air.Web, :admin_controller


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{admin: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, _params) do
    render(conn, "show.html", settings: Air.Service.Settings.read())
  end

  def update(conn, params) do
    Air.Service.Settings.update(conn.assigns.current_user, %{
      query_retention_days: parse_int(params["settings"]["query_retention_days"], :unlimited),
      audit_log_enabled: params["settings"]["audit_log_enabled"],
    })
    conn
    |> put_flash(:info, "The settings were saved.")
    |> render("show.html", settings: Air.Service.Settings.read())
  end

  defp parse_int(nil, default), do: default
  defp parse_int(string, default) do
    case Integer.parse(string) do
      {res, _} -> res
      :error -> default
    end
  end
end
