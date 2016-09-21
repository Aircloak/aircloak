defmodule Air.Admin.UserController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{User, AuditLog}


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    users = Repo.all(User) |> Repo.preload([:groups])
    render(conn, "index.html", users: users)
  end

  def new(conn, _params) do
    changeset = User.changeset(%User{})
    render(conn, "new.html", changeset: changeset)
  end

  def edit(conn, %{"id" => id}) do
    user = Repo.get!(User, id) |> Repo.preload([:groups])
    render(conn, "edit.html", changeset: User.changeset(user), user: user)
  end

  def create(conn, params) do
    changeset = User.changeset(%User{}, params["user"])
    case Repo.insert(changeset) do
      {:ok, user} ->
        AuditLog.log(conn, "Created user", user: user.email, name: user.name)
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: admin_user_path(conn, :index))
      {:error, changeset} -> render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, %{"id" => id} = params) do
    user = Repo.get!(User, id) |> Repo.preload([:groups])
    changeset = User.changeset(user, params["user"])
    case Repo.update(changeset) do
      {:ok, user} ->
        AuditLog.log(conn, "Altered user", user: user.email, name: user.name)
        conn
        |> put_flash(:info, "User updated")
        |> redirect(to: admin_user_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Repo.get!(User, id)
    Repo.delete!(user)
    AuditLog.log(conn, "Removed user", user: user.email, name: user.name)
    conn
    |> put_flash(:info, "User deleted")
    |> redirect(to: admin_user_path(conn, :index))
  end
end
