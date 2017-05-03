defmodule Air.Admin.WarningsController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.Warnings


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index, :warnings_if_any]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params), do:
    render(conn, "index.html")

  @doc """
  Redirects to the warnings index if there are any, otherwise
  to the activity monitor, which is the default admin page.
  """
  def warnings_if_any(conn, _params) do
    if Warnings.known_problems?() do
      redirect(conn, to: admin_warnings_path(conn, :index))
    else
      redirect(conn, to: admin_activity_monitor_path(conn, :index))
    end
  end
end
