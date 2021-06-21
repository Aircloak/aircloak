defmodule AirWeb.SessionController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Service.User

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      anonymous: [:new, :create],
      user: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def new(conn, _params) do
    if User.active_admin_user_exists?() do
      conn
      |> put_layout("login.html")
      |> render("new.html", login_message: Air.Service.Settings.read().login_message)
    else
      conn
      |> clear_flash()
      |> clear_session()
      |> redirect(to: onboarding_user_path(conn, :new))
    end
  end

  def create(conn, params) do
    case User.login(params["login"], params["password"], audit_log_meta(conn)) do
      {:ok, user} ->
        return_path = get_session(conn, :return_path) || data_source_path(conn, :redirect_to_last_used)

        conn
        |> AirWeb.Plug.Session.sign_in(user)
        |> conditionally_create_persistent_login(params)
        |> put_session(:return_path, nil)
        |> put_flash(:info, "Logged in successfully. Welcome back!")
        |> redirect(to: return_path)

      _ ->
        conn
        |> put_layout("login.html")
        |> put_flash(:error, "Invalid login or password.")
        |> render("new.html", login_message: Air.Service.Settings.read().login_message)
    end
  end

  def delete(conn, _params) do
    audit_log(conn, "Logged out")

    conn
    |> AirWeb.Plug.Session.sign_out()
    |> put_flash(:info, "Logged out successfully.")
    |> redirect(to: session_path(conn, :new))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conditionally_create_persistent_login(conn, %{"remember" => "on"}) do
    AirWeb.Plug.Session.Restoration.persist_token(conn)
  end

  defp conditionally_create_persistent_login(conn, _params), do: conn
end
