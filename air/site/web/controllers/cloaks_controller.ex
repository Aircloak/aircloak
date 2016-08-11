defmodule Air.CloaksController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

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
    organisation = Repo.get!(Organisation, conn.assigns.current_user.organisation_id)
    render(conn, "index.html", cloaks: CloakInfo.all(organisation))
  end
end
