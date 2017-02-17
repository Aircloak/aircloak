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
end
