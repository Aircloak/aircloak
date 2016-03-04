defmodule Air.UserController do
  use Air.Web, :controller

  alias Air.{User, Organisation}

  plug :preload_organisations, except: [:index, :delete]

  def index(conn, _params) do
    users = Repo.all(User) |> Repo.preload([:organisation])
    render(conn, "index.html", users: users)
  end

  def new(conn, _params) do
    changeset = User.changeset(%User{})
    render(conn, "new.html", changeset: changeset)
  end

  def edit(conn, %{"id" => id}) do
    user = Repo.get!(User, id)
    render(conn, "edit.html", changeset: User.changeset(user))
  end

  def create(conn, params) do
    changeset = User.changeset(%User{}, params["user"])
    case Repo.insert(changeset) do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: user_path(conn, :index))
      {:error, changeset} -> render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, %{"id" => id} = params) do
    user = Repo.get!(User, id)
    changeset = User.changeset(user, params["user"])
    case Repo.update(changeset) do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "User updated")
        |> redirect(to: user_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Repo.get!(User, id)
    Repo.delete!(user)
    conn
    |> put_flash(:info, "User deleted")
    |> redirect(to: user_path(conn, :index))
  end

  defp preload_organisations(conn, _params) do
    assign(conn, :organisations, Repo.all(Organisation))
  end
end
