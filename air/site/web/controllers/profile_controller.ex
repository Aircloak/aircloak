defmodule Air.ProfileController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{AuditLog, User}

  def permissions do
    %{user: :all}
  end

  def edit(conn, _params) do
    render(conn, "edit.html", changeset: User.changeset(conn.assigns.current_user))
  end

  def update(conn, params) do
    changeset = User.changeset(conn.assigns.current_user, params["user"])

    case Repo.update(changeset) do
      {:ok, user} ->
        AuditLog.log(conn, "Altered own profile")
        conn
        |> put_flash(:info, "Profile updated")
        |> redirect(to: profile_path(conn, :edit))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end
end
