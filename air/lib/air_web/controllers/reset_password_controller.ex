defmodule AirWeb.ResetPasswordController do
  @moduledoc false
  use Air.Web, :controller
  alias Air.Service.User

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{anonymous: [:show, :update]}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, params), do: render(conn, "show.html", token: params["token"], changeset: User.empty_changeset())

  def update(conn, params) do
    case User.reset_password(params["token"], params["user"]) do
      {:ok, _} ->
        conn
        |> put_flash(:info, "Password reset.")
        |> redirect(to: session_path(conn, :new))

      {:error, :invalid_token} ->
        conn
        |> put_flash(:error, "The reset link has expired.")
        |> redirect(to: reset_password_path(conn, :show))

      {:error, changeset} ->
        render(conn, "show.html", token: params["token"], changeset: changeset)
    end
  end
end
