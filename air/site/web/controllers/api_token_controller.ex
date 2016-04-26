defmodule Air.ApiTokenController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.ApiToken


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

  def create(conn, %{"api_token" => %{"description" => description}}) do
    case ApiToken.create_for(conn, current_user(conn), description) do
      {:error, changeset} ->
        conn
        |> render("index.html", api_tokens: existing_tokens(conn), changeset: changeset)
      token ->
        conn
        |> put_flash(:api_token, token)
        |> redirect(to: api_token_path(conn, :index))
    end
  end

  def delete(conn, %{"id" => id}) do
    token = Repo.get(ApiToken, id)
    case token.user_id == current_user(conn).id do
      true ->
        Repo.delete(token)
        conn
        |> put_flash(:info, "Token revoked")
        |> redirect(to: api_token_path(conn, :index))
      false ->
        conn
        |> put_flash(:info, "The token is unknown and cannot be revoked")
        |> redirect(to: api_token_path(conn, :index))
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp current_user(conn) do
    Guardian.Plug.current_resource(conn)
  end

  defp existing_tokens(conn) do
    Repo.all(from t in ApiToken, where: t.user_id == ^current_user(conn).id, select: t)
  end
end
