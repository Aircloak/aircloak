defmodule AirWeb.Admin.UserController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.User

  plug(:load_user when action in [:edit, :update, :delete, :disable, :enable])

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

  def index(conn, _params) do
    {enabled_users, disabled_users} =
      User.all()
      |> Enum.sort_by(& &1.name)
      |> Enum.split_with(& &1.enabled)

    {enabled_users, enabled_ldap_users} = Enum.split_with(enabled_users, &(&1.source == :native))
    {disabled_users, disabled_ldap_users} = Enum.split_with(disabled_users, &(&1.source == :native))

    render(
      conn,
      "index.html",
      enabled_users: enabled_users,
      disabled_users: disabled_users,
      enabled_ldap_users: enabled_ldap_users,
      disabled_ldap_users: disabled_ldap_users,
      data_sources_count: User.data_sources_count()
    )
  end

  def new(conn, _params), do: render(conn, "new.html", changeset: User.empty_changeset())

  def edit(conn, _params),
    do:
      render(
        conn,
        "edit.html",
        changeset: User.to_changeset(conn.assigns.user),
        user: conn.assigns.user,
        reset_path: reset_path(conn)
      )

  def create(conn, params) do
    case User.create(params["user"]) do
      {:ok, user} ->
        audit_log(conn, "Created user")
        audit_log_for_user(conn, user, "User created")

        conn
        |> put_flash(:info, "User created")
        |> redirect(to: admin_user_path(conn, :edit, user.id))

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
        render(conn, "edit.html", changeset: changeset, reset_path: reset_path(conn))
    end)
  end

  def delete(conn, _params) do
    user = conn.assigns.user

    start_callback = fn ->
      audit_log(conn, "Disabled a user account prior to deletion")
      audit_log_for_user(conn, user, "User account disabled prior to deletion")

      audit_log(conn, "User removal scheduled")
      audit_log_for_user(conn, user, "User scheduled for removal")
    end

    success_callback = fn -> audit_log(conn, "User removal succeeded") end
    failure_callback = fn reason -> audit_log(conn, "User removal failed", %{reason: reason}) end

    case User.delete_async(user, start_callback, success_callback, failure_callback) do
      :ok ->
        conn
        |> put_flash(:info, "The user has been disabled. The deletion will be performed in the background")
        |> redirect(to: admin_user_path(conn, :index))

      {:error, error} ->
        conn
        |> put_flash(:error, delete_error_message(error))
        |> redirect(to: admin_user_path(conn, :index))
    end
  end

  def disable(conn, _params) do
    case User.disable(conn.assigns.user) do
      {:error, :forbidden_no_active_admin} ->
        conn
        |> put_flash(:error, "Cannot disable the user as it would leave the system without an administrator.")
        |> redirect(to: admin_user_path(conn, :index))

      {:ok, user} ->
        audit_log(conn, "Disabled a user account")
        audit_log_for_user(conn, user, "User account disabled")

        conn
        |> redirect(to: admin_user_path(conn, :index))
    end
  end

  def enable(conn, _params) do
    User.enable!(conn.assigns.user)
    audit_log(conn, "Enabled a user account")
    audit_log_for_user(conn, conn.assigns.user, "User account enabled")

    conn
    |> redirect(to: admin_user_path(conn, :index))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_user(conn, _) do
    case User.load(conn.params["id"] || conn.params["user_id"]) do
      nil -> not_found(conn)
      user -> assign(conn, :user, user)
    end
  end

  defp verify_last_admin_deleted({:error, :forbidden_no_active_admin}, conn, _fun),
    do:
      conn
      |> put_flash(
        :error,
        "The given action cannot be performed, because it would remove the only administrator."
      )
      |> redirect(to: admin_user_path(conn, :index))

  defp verify_last_admin_deleted(result, _conn, fun), do: fun.(result)

  defp reset_path(conn) do
    token = User.reset_password_token(conn.assigns.user)
    "#{reset_password_path(conn, :show)}?token=#{token}"
  end

  defp delete_error_message(:forbidden_no_active_admin),
    do: "The user cannot be deleted as it would leave the system without an active administrator."

  defp delete_error_message(:invalid_ldap_delete), do: "The user can only be deleted in LDAP."
end
