defmodule AirWeb.Admin.ActivityMonitorController do
  @moduledoc """
  Controller for administrators to get a view of the live state of their system.
  """

  use Air.Web, :admin_controller

  alias Plug.CSRFProtection
  alias Air.Service.{Cloak.Stats, Query}

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(
      conn,
      "index.html",
      csrf_token: CSRFProtection.get_csrf_token(),
      guardian_token: Air.Guardian.Plug.current_token(conn),
      running_queries: Query.not_finished(),
      cloak_stats: Stats.cloak_stats()
    )
  end
end
