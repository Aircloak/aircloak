defmodule Air.ApiTokenController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{Schemas.ApiToken, Token}


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
    changeset = ApiToken.changeset(%ApiToken{})
    render(conn, "index.html", api_tokens: existing_tokens(conn), changeset: changeset)
  end

  def create(conn, %{"api_token" => params}) do
    access = Map.get(params, "access", :api)
    description = Map.get(params, "description", "")

    case Token.create_api_token(conn.assigns.current_user, access, description) do
      {:error, changeset} ->
        conn
        |> render("index.html", api_tokens: existing_tokens(conn), changeset: changeset)
      token ->
        audit_log(conn, "Created API token")
        conn
        |> put_flash(:api_token, token)
        |> redirect(to: api_token_path(conn, :index))
    end
  end

  def delete(conn, %{"id" => id}) do
    token = Repo.get(ApiToken, id)
    case token.user_id == conn.assigns.current_user.id do
      true ->
        Repo.delete!(token)
        audit_log(conn, "Invalidated API token")
        conn
        |> put_flash(:info, "Token revoked")
        |> redirect(to: api_token_path(conn, :index))
      false ->
        conn
        |> put_status(:not_found)
        |> render(Air.ErrorView, "404.html")
        |> halt
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp existing_tokens(conn) do
    Repo.all(from t in ApiToken, where: t.user_id == ^conn.assigns.current_user.id, select: t)
  end
end
