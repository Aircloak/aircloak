defmodule AirWeb.Admin.UserController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.User

  plug(:load_user when action in [:edit, :update, :delete, :reset_password])

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params),
    do: render(conn, "index.html", users: User.all(), data_sources_count: User.data_sources_count())

  def new(conn, _params), do: render(conn, "new.html", changeset: User.empty_changeset())

  def edit(conn, _params),
    do:
      render(
        conn,
        "edit.html",
        changeset: User.to_changeset(conn.assigns.user),
        user: conn.assigns.user
      )

  def create(conn, params) do
    case User.create(params["user"]) do
      {:ok, user} ->
        audit_log(conn, "Created user")
        audit_log_for_user(conn, user, "User created")

        conn
        |> put_flash(:info, "User created")
        |> redirect(to: admin_user_path(conn, :index))

      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def update(conn, params) do
    verify_last_admin_deleted(User.update(conn.assigns.user, params["user"]), conn, fn
      {:ok, user} ->
        audit_log(conn, "Altered user")
        audit_log_for_user(conn, user, "User altered")

        conn
        |> put_flash(:info, "User updated")
        |> redirect(to: admin_user_path(conn, :index))

      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset)
    end)
  end

  def delete(conn, _params) do
    user = conn.assigns.user

    audit_log(conn, "User removal scheduled")
    audit_log_for_user(conn, user, "User scheduled for removal")
    success_callback = fn -> audit_log(conn, "User removal succeeded") end
    failure_callback = fn reason -> audit_log(conn, "User removal failed", %{reason: reason}) end
    User.delete_async(user, success_callback, failure_callback)

    conn
    |> put_flash(:info, "User deletion will be performed in the background")
    |> redirect(to: admin_user_path(conn, :index))
  end

  def reset_password(conn, _params) do
    render(conn, "reset_password.html", user: conn.assigns.user, reset_link: "http://example.com")
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_user(conn, _) do
    case User.load(conn.params["id"]) do
      nil -> not_found(conn)
      user -> assign(conn, :user, user)
    end
  end

  defp verify_last_admin_deleted({:error, :forbidden_last_admin_deletion}, conn, _fun),
    do:
      conn
      |> put_flash(
        :error,
        "The given action cannot be performed, because it would remove the only administrator."
      )
      |> redirect(to: admin_user_path(conn, :index))

  defp verify_last_admin_deleted(result, _conn, fun), do: fun.(result)
end
