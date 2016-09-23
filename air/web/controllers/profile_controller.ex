defmodule Air.ProfileController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.{AuditLog, User}
  alias Ecto.Changeset

  @simple_fields ~w(name email)


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
    render(conn, "edit.html", changeset: User.changeset(conn.assigns.current_user))
  end

  def update(conn, params) do
    conn
    |> build_changeset(params["user"])
    |> Repo.update()
    |> case do
      {:ok, _} ->
        AuditLog.log(conn, "Altered own profile")
        conn
        |> put_flash(:info, "Profile updated")
        |> redirect(to: profile_path(conn, :edit))
      {:error, changeset} -> render(conn, "edit.html", changeset: changeset)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp build_changeset(conn, params) do
    Changeset.merge(
      User.changeset(conn.assigns.current_user, Map.take(params, @simple_fields)),
      password_changeset(conn.assigns.current_user, params)
    )
  end

  defp password_changeset(user, params) do
    old_password_valid = User.validate_password(user, params["old_password"] || "")

    case {params["password"], old_password_valid} do
      {"", _}    -> User.changeset(user, %{})
      {nil, _}   -> User.changeset(user, %{})
      {_, true}  -> User.changeset(user, Map.take(params, ["password", "password_confirmation"]))
      {_, false} -> User.changeset(user, %{}) |> Changeset.add_error(:old_password, "Password invalid")
    end
  end
end
