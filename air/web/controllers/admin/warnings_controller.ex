defmodule Air.Admin.WarningsController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Schemas.Query


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params), do:
    render(conn, "index.html")
end
