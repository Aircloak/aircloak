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
    render(conn, "show.html")
  end
end
