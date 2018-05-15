defmodule AirWeb.ResetPasswordController do
  @moduledoc false
  use Air.Web, :controller
  alias Air.Service.User

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{anonymous: [:show, :update, :forgot]}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, params),
    do: render(conn, "show.html", token: String.trim(params["token"] || ""), changeset: User.empty_changeset())

  def update(conn, params) do
    case User.reset_password(params["token"], params["user"]) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "Your new password has been saved.")
        |> Guardian.Plug.sign_in(user)
        |> redirect(to: "/")

      {:error, :invalid_token} ->
        conn
        |> put_flash(:error, "The reset link or token is invalid. It might have expired.")
        |> redirect(to: reset_password_path(conn, :show))

      {:error, changeset} ->
        render(conn, "show.html", token: params["token"], changeset: changeset)
    end
  end

  def forgot(conn, _params), do: render(conn, "forgot.html")
end
