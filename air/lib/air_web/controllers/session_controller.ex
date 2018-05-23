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
    if User.admin_user_exists?() do
      render(conn, "new.html")
    else
      conn
      |> clear_flash()
      |> clear_session()
      |> redirect(to: onboarding_user_path(conn, :new))
    end
  end

  def create(conn, params) do
    case User.login(params["email"], params["password"], audit_log_meta(conn)) do
      {:ok, user} ->
        return_path = get_session(conn, :return_path) || data_source_path(conn, :redirect_to_last_used)

        conn
        |> Air.Guardian.Plug.sign_in(user)
        |> conditionally_create_persistent_login(user, params)
        |> put_session(:return_path, nil)
        |> put_flash(:info, "Logged in successfully. Welcome back!")
        |> redirect(to: return_path)

      _ ->
        conn
        |> put_flash(:error, "Invalid e-mail or password.")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    audit_log(conn, "Logged out")

    conn
    |> Air.Guardian.Plug.sign_out()
    |> AirWeb.Plug.Session.Restoration.remove_token()
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: session_path(conn, :new))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conditionally_create_persistent_login(conn, user, %{"remember" => "on"}) do
    AirWeb.Plug.Session.Restoration.persist_token(conn, user)
  end

  defp conditionally_create_persistent_login(conn, _user, _params), do: conn
end
