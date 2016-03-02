defmodule Air.SessionController do
  use Air.Web, :controller

  alias Air.User

  def new(conn, _params) do
    render(conn, "new.html")
  end

  def create(conn, params) do
    user = User |> Repo.get_by(email: params["email"])
    case User.validate_password(user, params["password"]) do
      true -> text(conn, "Login successful")
      false -> text(conn, "Login failed")
    end
  end

  def delete(conn, _params) do
    text(conn, "Eventually this will log the user out")
  end
end
