defmodule AirWeb.Admin.WarningsController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.Warnings

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params), do: render(conn, "index.html", problems: Air.Service.Warnings.problems())

  @doc """
  Redirects to the warnings index if there are any, otherwise
  to the activity monitor, which is the default admin page.
  """
  def warnings_if_any(conn, _params) do
    if length(Warnings.problems()) > 0 do
      redirect(conn, to: admin_warnings_path(conn, :index))
    else
      redirect(conn, to: admin_system_status_path(conn, :index))
    end
  end
end
