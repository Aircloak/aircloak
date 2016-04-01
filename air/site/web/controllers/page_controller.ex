defmodule Air.PageController do
  @moduledoc false
  use Air.Web, :controller

  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: [:index]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render conn, "index.html"
  end
end
