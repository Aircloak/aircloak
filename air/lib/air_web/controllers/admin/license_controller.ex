defmodule AirWeb.Admin.LicenseController do
  @moduledoc false
  use Air.Web, :admin_controller


  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{admin: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def edit(conn, _params) do
    render(conn, "edit.html")
  end
end
