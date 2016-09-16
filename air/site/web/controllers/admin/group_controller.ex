defmodule Air.Admin.GroupController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{Group, AuditLog}


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
    changeset = Group.changeset(%Group{})
    render(conn, "index.html", groups: all_groups(), changeset: changeset)
  end

  def create(conn, params) do
    changeset = Group.changeset(%Group{}, params["group"])
    case Repo.insert(changeset) do
      {:ok, group} ->
        AuditLog.log(conn, "Created group", name: group.name)
        conn
        |> put_flash(:info, "Group created")
        |> redirect(to: group_path(conn, :index))
      {:error, changeset} ->
        render(conn, "index.html", changeset: changeset, groups: all_groups())
    end
  end

  def delete(conn, %{"id" => id}) do
    group = Repo.get!(Group, id)
    Repo.delete!(group)
    AuditLog.log(conn, "Removed group", name: group.name)
    conn
    |> put_flash(:info, "Group deleted")
    |> redirect(to: group_path(conn, :index))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp all_groups() do
    Repo.all(Group) |> Repo.preload([:users, :data_sources])
  end
end
