defmodule AirWeb.AppLoginController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.User

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
    render(conn, "index.html", app_logins: app_logins(conn), changeset: User.app_login_changeset())
  end

  def create(conn, %{"login" => params}) do
    case User.create_app_login(conn.assigns.current_user, params) do
      {:error, changeset} ->
        render(conn, "index.html", app_logins: app_logins(conn), changeset: changeset)

      {:ok, login, password} ->
        audit_log(conn, "Created app login")

        conn
        |> put_flash(:app_login, login)
        |> put_flash(:app_password, password)
        |> redirect(to: app_login_path(conn, :index))
    end
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  defp app_logins(conn), do: Enum.reject(conn.assigns.current_user.logins, &(&1.login_type == :main))
end
