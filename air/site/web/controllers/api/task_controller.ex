defmodule Air.API.TaskController do
  @moduledoc false
  use Air.Web, :controller

  require Logger


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def run_task(conn, _params) do
    json(conn, %{success: true})
  end
end
