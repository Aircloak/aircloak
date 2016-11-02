defmodule Central.StatsController do
  @moduledoc false
  use Central.Web, :controller

  alias Central.Service.Stats

  plug :set_layout


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html", basic_stats: Stats.basic_stats())
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp set_layout(conn, _) do
    conn
    |> put_layout("fullscreen.html")
  end
end
