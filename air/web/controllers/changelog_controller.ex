defmodule Air.ChangelogController do
  @moduledoc "Controller for displaying a changelog in the web interface"

  use Air.Web, :controller


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: :all,
      admin: :all,
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
