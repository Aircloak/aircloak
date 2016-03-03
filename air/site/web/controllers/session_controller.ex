defmodule Air.SessionController do
  use Air.Web, :controller

  alias Air.User
  alias Plug.Conn

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, params) do
    user = Repo.get_by(User, email: params["email"])
    case User.validate_password(user, params["password"]) do
      true ->
        return_path = get_session(conn, :return_path) || page_path(conn, :index)
        conn
        |> Guardian.Plug.sign_in(user)
        |> put_session(:return_path, nil)
        |> put_flash(:info, "Logged inn successfully. Welcome back")
        |> redirect(to: return_path)
      false ->
        render(conn, "new.html")
    end
  end

  def delete(conn, _params) do
    conn
    |> Guardian.Plug.sign_out
    |> put_flash(:info, "Logged out successfully")
    |> redirect(to: session_path(conn, :new))
  end

  def unauthenticated(%Conn{request_path: path} = conn, _params) do
    conn
    |> put_flash(:error, "You must be authenticated to view this page")
    |> put_session(:return_path, path)
    |> redirect(to: session_path(conn, :new))
  end
end
