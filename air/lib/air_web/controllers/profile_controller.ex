defmodule AirWeb.ProfileController do
  @moduledoc false
  use Air.Web, :controller
  alias Air.Service.User

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def edit(conn, _params) do
    changeset = User.to_changeset(conn.assigns.current_user)
    global_settings = Air.Service.Settings.read()
    render(conn, "edit.html", changeset: changeset, global_settings: global_settings)
  end

  def update(conn, params),
    do: update(conn, params, _flash = "Profile updated", _log_tag = "Altered own profile")

  def change_password(conn, params),
    do: update(conn, params, _flash = "Password changed", _log_tag = "Changed own password")

  def toggle_debug_mode(conn, _params) do
    User.toggle_debug_mode(conn.assigns.current_user)
    redirect(conn, to: profile_path(conn, :edit))
  end

  # -------------------------------------------------------------------
  # Update
  # -------------------------------------------------------------------

  defp update(conn, params, flash, log_tag) do
    case User.update_profile(conn.assigns.current_user, params["user"]) do
      {:ok, _} ->
        audit_log(conn, log_tag)

        conn
        |> put_flash(:info, flash)
        |> redirect(to: profile_path(conn, :edit))

      {:error, changeset} ->
        global_settings = Air.Service.Settings.read()
        render(conn, "edit.html", changeset: changeset, global_settings: global_settings)
    end
  end
end
