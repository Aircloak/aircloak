defmodule Air.Admin.UserController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.User

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

  def index(conn, _params), do:
    render(conn, "index.html", users: User.all(), data_sources_count: User.data_sources_count())

  def new(conn, _params), do:
    render(conn, "new.html", changeset: User.empty_changeset())

  def edit(conn, _params), do:
    render(conn, "edit.html", changeset: User.to_changeset(conn.assigns.user), user: conn.assigns.user)

  def create(conn, params) do
    case User.create(params["user"]) do
      {:ok, user} ->
        audit_log(conn, "Created user", user: user.email, name: user.name)
        conn
        |> put_flash(:info, "User created")
        |> redirect(to: admin_user_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, params) do
    verify_last_admin_deleted(User.update(conn.assigns.user, params["user"]), conn,
      fn
        {:ok, user} ->
          audit_log(conn, "Altered user", user: user.email, name: user.name)
          conn
          |> put_flash(:info, "User updated")
          |> redirect(to: admin_user_path(conn, :index))
        {:error, changeset} ->
          render(conn, "edit.html", changeset: changeset)
      end
    )
  end

  def delete(conn, _params) do
    user = conn.assigns.user
    verify_last_admin_deleted(User.delete(user), conn, fn({:ok, _}) ->
      audit_log(conn, "Removed user", user: user.email, name: user.name)
      conn
      |> put_flash(:info, "User deleted")
      |> redirect(to: admin_user_path(conn, :index))
    end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_user(conn, _) do
    case User.load(conn.params["id"]) do
      nil ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> put_view(Air.ErrorView)
        |> render("404.html")
        |> halt()
      user ->
        assign(conn, :user, user)
    end
  end

  defp verify_last_admin_deleted({:error, :forbidden_last_admin_deletion}, conn, _fun), do:
    conn
    |> put_flash(:error, "The given action cannot be performed, because it would remove the only administrator.")
    |> redirect(to: admin_user_path(conn, :index))
  defp verify_last_admin_deleted(result, _conn, fun), do:
    fun.(result)
end
