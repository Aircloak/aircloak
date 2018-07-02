defmodule AirWeb.Admin.LDAPController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.AuditLog

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{admin: :all}
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, _params) do
    render(conn, "show.html", changeset: Air.Service.Settings.latest_changeset())
  end

  def create(conn, params), do: save(conn, params)
  def update(conn, params), do: save(conn, params)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp save(conn, params) do
    case Air.Service.Settings.save_ldap(params["settings"]) do
      {:ok, settings} ->
        AuditLog.log(conn.assigns.current_user, "Updated LDAP settings", settings)

        conn
        |> put_flash(:info, "The settings were saved.")
        |> redirect(to: admin_ldap_path(conn, :show))

      {:error, changeset} ->
        render(conn, "show.html", changeset: changeset)
    end
  end
end
