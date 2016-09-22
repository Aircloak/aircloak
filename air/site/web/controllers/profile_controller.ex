defmodule Air.ProfileController do
  @moduledoc false
  use Air.Web, :controller

  def permissions do
    %{user: :all}
  end

  def show(conn, _params) do
    render(conn, "show.html")
  end
end
