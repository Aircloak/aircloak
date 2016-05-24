defmodule Air.SessionController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.User

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
    render(conn, "new.html")
  end

  def create(conn, params) do
    user = Repo.get_by(User, email: params["email"])
    case User.validate_password(user, params["password"]) do
      true ->
        return_path = get_session(conn, :return_path) || queries_path(conn, :index)
        conn
        |> Guardian.Plug.sign_in(user)
        |> conditionally_create_persistent_login(params)
        |> put_session(:return_path, nil)
        |> put_flash(:info, "Logged in successfully. Welcome back!")
        |> redirect(to: return_path)
      false ->
        conn
        |> put_flash(:error, "Invalid e-mail or password.")
        |> render("new.html")
    end
  end

  def delete(conn, _params) do
    conn
    |> Guardian.Plug.sign_out()
    |> Air.Plug.Session.Restoration.remove_token()
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: session_path(conn, :new))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conditionally_create_persistent_login(conn, %{"remember" => "on"}) do
    Air.Plug.Session.Restoration.persist_token(conn)
  end
  defp conditionally_create_persistent_login(conn, _params), do: conn
end
