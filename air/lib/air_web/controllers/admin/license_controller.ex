defmodule AirWeb.Admin.LicenseController do
  @moduledoc false
  use Air.Web, :admin_controller


  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all,
      user: [:invalid],
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def edit(conn, _params) do
    render(conn, "edit.html")
  end

  def invalid(conn, _params) do
    if Air.Schemas.User.admin?(conn.assigns.current_user) do
      redirect(conn, to: admin_license_path(conn, :edit))
    else
      render(conn, "invalid.html", layout: false)
    end
  end
end
