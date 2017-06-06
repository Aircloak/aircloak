defmodule Air.ProfileController do
  @moduledoc false
  use Air.Web, :controller
  alias Air.Service.User


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def edit(conn, _params) do
    render(conn, "edit.html", changeset: User.to_changeset(conn.assigns.current_user))
  end

  def update(conn, params) do
    case User.update_profile(conn.assigns.current_user, params["user"]) do
      {:ok, _} ->
        audit_log(conn, "Altered own profile")
        conn
        |> put_flash(:info, "Profile updated")
        |> redirect(to: profile_path(conn, :edit))
      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset)
    end
  end
end
