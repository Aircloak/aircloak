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
    organisation = Repo.get!(Organisation, current_user(conn).organisation_id)
    cloaks = CloakInfo.all(organisation)
    cloaks = for cloak <- cloaks do
      # capture time difference since connected, ignoring sub-minute values
      conn_uptime =  :erlang.monotonic_time(:seconds) - cloak.created_at
          |> div(60)
          |> Time.from(:minutes)
          |> Timex.Format.Time.Formatters.Humanized.format()
      Map.put_new(cloak, :conn_uptime, conn_uptime)
    end
    render(conn, "index.html", cloaks: cloaks)
  end


  # -------------------------------------------------------------------
  # Private methods
  # -------------------------------------------------------------------

  defp current_user(conn) do
    Guardian.Plug.current_resource(conn)
  end
end
