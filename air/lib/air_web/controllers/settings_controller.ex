defmodule AirWeb.SettingsController do
  @moduledoc false
  use Air.Web, :controller
  alias Air.Service.User

  plug(:put_layout, "settings.html")

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def profile(conn, _params) do
    changeset = User.to_changeset(conn.assigns.current_user)
    global_settings = Air.Service.Settings.read()

    render(conn, "profile.html", changeset: changeset, global_settings: global_settings)
  end

  def update(conn, params) do
    case update_profile(conn.assigns.current_user, params["user"]) do
      {:ok, _, sessions_revoked?} ->
        audit_log(conn, "Altered their profile")

        conn
        |> maybe_login(sessions_revoked?)
        |> put_flash(:info, "Profile updated.")
        |> redirect(to: settings_path(conn, :profile))

      {:error, changeset} ->
        global_settings = Air.Service.Settings.read()

        render(conn, "profile.html", changeset: changeset, global_settings: global_settings)
    end
  end

  def toggle_debug_mode(conn, _params) do
    User.toggle_debug_mode(conn.assigns.current_user)
    redirect(conn, to: settings_path(conn, :profile))
  end

  def security(conn, _params) do
    render(conn, "security.html", changeset: User.to_changeset(conn.assigns.current_user))
  end

  def change_password(conn, params) do
    case update_password(conn.assigns.current_user, params["user"]) do
      {:ok, _, sessions_revoked?} ->
        audit_log(conn, "Changed their password")

        conn
        |> maybe_login(sessions_revoked?)
        |> put_flash(:info, "Changed own password.")
        |> redirect(to: settings_path(conn, :security))

      {:error, changeset} ->
        conn
        |> put_status(:unauthorized)
        |> render("security.html", changeset: changeset)
    end
  end

  def delete_sessions(conn, _params) do
    Air.Service.RevokableToken.revoke_all(conn.assigns.current_user, :session)

    audit_log(conn, "Cleared their sessions")

    conn
    |> AirWeb.Plug.Session.sign_in(conn.assigns.current_user)
    |> put_flash(:info, "All sessions signed out.")
    |> redirect(to: settings_path(conn, :security))
  end

  def privacy(conn, _params) do
    render(conn, "privacy.html")
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp maybe_login(conn, false), do: conn
  defp maybe_login(conn, true), do: AirWeb.Plug.Session.sign_in(conn, conn.assigns.current_user)

  defp update_profile(user = %{source: :native}, params), do: User.update_full_profile(user, params)

  defp update_profile(user = %{source: :ldap}, params) do
    with {:ok, user} <- User.update_profile_settings(user, params) do
      {:ok, user, false}
    end
  end

  defp update_password(user, params), do: User.update_password(user, params)
end
