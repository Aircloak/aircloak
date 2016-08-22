defmodule Air.CloaksController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  alias Air.Cloak


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
    cloaks = Repo.all(Cloak)
    render(conn, "index.html", cloaks: cloaks)
  end
end
