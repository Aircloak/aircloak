defmodule CentralWeb.StatsController do
  @moduledoc false
  use Central.Web, :controller

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params), do: render(conn, "index.html")
end
