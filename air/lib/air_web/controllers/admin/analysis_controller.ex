defmodule AirWeb.Admin.AnalysisController do
  @moduledoc """
  Controller for administrators to get a view of the state of the analysis queries in the system.
  """

  use Air.Web, :admin_controller

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions(), do: %{admin: :all}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
