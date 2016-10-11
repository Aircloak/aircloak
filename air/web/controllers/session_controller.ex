defmodule Air.SessionController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{User, AuditLog}

  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
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
      redirect(conn, to: onboarding_user_path(conn, :new))
    end
  end

  def create(conn, params) do
    case Air.Service.User.login(params["email"], params["password"]) do
      {:ok, user} ->
        AuditLog.log(temporary_set_user(conn, user), "Logged in")
        return_path = get_session(conn, :return_path) || query_path(conn, :index)
        conn
        |> Guardian.Plug.sign_in(user)
        |> conditionally_create_persistent_login(params)
        |> put_session(:return_path, nil)
        |> put_flash(:info, "Logged in successfully. Welcome back!")
        |> redirect(to: return_path)
      {:error, user} ->
        AuditLog.log(temporary_set_user(conn, user), "Failed login")
        conn
        |> put_flash(:error, "Invalid e-mail or password.")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    AuditLog.log(conn, "Logged out")
    conn
    |> Guardian.Plug.sign_out()
    |> Air.Plug.Session.Restoration.remove_token()
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: session_path(conn, :new))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp temporary_set_user(conn, user), do: Plug.Conn.assign(conn, :current_user, user)

  defp conditionally_create_persistent_login(conn, %{"remember" => "on"}) do
    Air.Plug.Session.Restoration.persist_token(conn)
  end
  defp conditionally_create_persistent_login(conn, _params), do: conn
end
