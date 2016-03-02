defmodule Air.PageController do
  use Air.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
