defmodule Air.Admin.ActivityMonitorController do
  @moduledoc """
  Controller for administrators to get a view of the live state of their system.
  """

  use Air.Web, :admin_controller

  alias Plug.CSRFProtection
  alias Air.Service.Query


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

  def index(conn, _params) do
    queries = Query.currently_running()
    |> Enum.concat(Query.recently_completed())
    |> Query.format_for_activity_monitor_view()

    render(
      conn,
      "index.html",
      csrf_token: CSRFProtection.get_csrf_token(),
      guardian_token: Guardian.Plug.current_token(conn),
      queries: queries,
    )
  end
end
