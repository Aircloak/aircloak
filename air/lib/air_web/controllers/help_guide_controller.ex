defmodule AirWeb.HelpGuideController do
  @moduledoc false
  use Air.Web, :controller


  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def article(conn, params) do
    render(conn, "#{params["article"]}.html")
  end
end
