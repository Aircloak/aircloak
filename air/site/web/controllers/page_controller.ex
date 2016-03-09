defmodule Air.PageController do
  @moduledoc false
  use Air.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
