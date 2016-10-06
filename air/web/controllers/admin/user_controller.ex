defmodule Air.Admin.UserController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{User, AuditLog}

  plug :load_user when action in [:edit, :update, :delete]


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

    query = from user in User,
      inner_join: group in assoc(user, :groups),
      inner_join: data_source in assoc(group, :data_sources),
      group_by: user.id,
      select: %{
        id: user.id,
        data_source_count: count(data_source.id, :distinct)
      }
    data_sources_count = for user <- Repo.all(query), into: %{} do
      {user.id, user.data_source_count}
    end

    render(conn, "index.html", users: users, data_sources_count: data_sources_count)
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
        AuditLog.log(conn, "Created user", user: user.email, name: user.name)
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: admin_user_path(conn, :index))
      {:error, changeset} -> render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, params) do
    changeset = User.changeset(conn.assigns.user, params["user"])
    case Repo.update(changeset) do
      {:ok, user} ->
        AuditLog.log(conn, "Altered user", user: user.email, name: user.name)
        conn
        |> put_flash(:info, "User updated")
        |> redirect(to: admin_user_path(conn, :index))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end

  def delete(conn, _params) do
    user = conn.assigns.user
    Repo.delete!(user)
    AuditLog.log(conn, "Removed user", user: user.email, name: user.name)
    conn
    |> put_flash(:info, "User deleted")
    |> redirect(to: admin_user_path(conn, :index))
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
        |> render(Air.ErrorView, "404.html")
        |> halt()
      user ->
        user = Repo.preload(user, [:groups])
        assign(conn, :user, user)
    end
  end
end
