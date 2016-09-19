defmodule Air.Admin.UserController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{User, Organisation, AuditLog}

  plug :preload_organisations, except: [:index, :delete]


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      org_admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    users = Repo.all(User) |> Repo.preload([:organisation, :groups])
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
    if create_permitted?(conn, changeset) do
      case Repo.insert(changeset) do
        {:ok, user} ->
          AuditLog.log(conn, "Created user", user: user.email, name: user.name)
          conn
          |> put_flash(:info, "User created")
          |> redirect(to: admin_user_path(conn, :index))
        {:error, changeset} -> render(conn, "new.html", changeset: changeset)
      end
    else
      conn
      |> put_flash(:error, "Action not allowed!")
      |> render("new.html", changeset: changeset)
    end
  end

  def update(conn, %{"id" => id} = params) do
    user = Repo.get!(User, id) |> Repo.preload([:groups])
    user_params = params["user"]

    if edit_permitted?(conn, user, user_params) do
      changeset = User.changeset(user, params["user"])
      case Repo.update(changeset) do
        {:ok, user} ->
          AuditLog.log(conn, "Altered user", user: user.email, name: user.name)
          conn
          |> put_flash(:info, "User updated")
          |> redirect(to: admin_user_path(conn, :index))
        {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
      end
    else
      conn
      |> put_flash(:error, "Action not allowed!")
      |> redirect(to: admin_user_path(conn, :index))
      |> Plug.Conn.halt()
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


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp preload_organisations(conn, _params) do
    assign(conn, :organisations, Repo.all(Organisation))
  end

  defp create_permitted?(conn, changeset) do
    org_id = conn.assigns.current_user.organisation_id

    (
      # admin is permitted to do everything
      User.admin?(conn.assigns.current_user) or
      # non-admin can only store to the same organisation
      Ecto.Changeset.get_change(changeset, :organisation_id, org_id) == org_id
    )
  end

  defp edit_permitted?(conn, user, user_params) do
    # admin is permitted to do everything
    User.admin?(conn.assigns.current_user) or (
      # non-admin can only change it's own users
      conn.assigns.current_user.organisation_id == user.organisation_id and
      # non-admin is not allowed to change user's organisation
      user_params["organisation_id"] == nil
    )
  end
end
