defmodule Air.Admin.CentralController do
  @moduledoc false
  use Air.Web, :admin_controller

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
    render conn
  end
end
