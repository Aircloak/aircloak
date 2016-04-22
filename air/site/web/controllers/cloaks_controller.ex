defmodule Air.CloaksController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{Organisation, CloakInfo}


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
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
    organisation = Repo.get!(Organisation, current_user(conn).organisation_id)
    cloaks = CloakInfo.all(organisation)
    render(conn, "index.html", cloaks: cloaks)
  end


  # -------------------------------------------------------------------
  # Private methods
  # -------------------------------------------------------------------

  defp current_user(conn) do
    Guardian.Plug.current_resource(conn)
  end
end
