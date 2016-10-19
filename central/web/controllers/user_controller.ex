defmodule Central.UserController do
  @moduledoc false
  use Central.Web, :controller

  alias Central.User

  plug :load_user when action in [:edit, :update, :delete]


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    users = Repo.all(User)
    render(conn, "index.html", users: users)
  end

  def new(conn, _params) do
    changeset = User.changeset(%User{})
    render(conn, "new.html", changeset: changeset)
  end

  def edit(conn, _params) do
    user = conn.assigns.user
    render(conn, "edit.html", changeset: User.changeset(user), user: user)
  end

  def create(conn, params) do
    changeset = User.new_user_changeset(%User{}, params["user"])
    case Repo.insert(changeset) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: user_path(conn, :index))
      {:error, changeset} -> render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, params) do
    changeset = User.changeset(conn.assigns.user, params["user"])
    case Repo.update(changeset) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User updated")
        |> redirect(to: user_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def delete(conn, _params) do
    user = conn.assigns.user
    Repo.delete!(user)
    conn
    |> put_flash(:info, "User deleted")
    |> redirect(to: user_path(conn, :index))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_user(conn, _) do
    case Repo.get(User, conn.params["id"]) do
      nil ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> render(Central.ErrorView, "404.html")
        |> halt()
      user -> assign(conn, :user, user)
    end
  end
end
