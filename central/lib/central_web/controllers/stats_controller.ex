defmodule CentralWeb.StatsController do
  @moduledoc false
  use Central.Web, :controller

  plug(:set_layout)

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    dash_url = Central.site_setting("kibana_main_dash")
    render(conn, "index.html", dash: dash_url)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp set_layout(conn, _) do
    conn
    |> put_layout("fullscreen.html")
  end
end
