defmodule AirWeb.AppLoginController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Schemas.ApiToken

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
    changeset = ApiToken.changeset(%ApiToken{})
    render(conn, "index.html", api_tokens: existing_tokens(conn), changeset: changeset)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp existing_tokens(conn) do
    Repo.all(from(t in ApiToken, where: t.user_id == ^conn.assigns.current_user.id, select: t))
  end
end
