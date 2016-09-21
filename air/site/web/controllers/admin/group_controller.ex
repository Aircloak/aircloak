defmodule Air.Admin.GroupController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{Group, AuditLog}


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
        |> redirect(to: admin_group_path(conn, :index))
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
    |> redirect(to: admin_group_path(conn, :index))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp all_groups() do
    Group
    |> Repo.all()
    |> Repo.preload([:users, :data_sources])
    |> Enum.sort_by(&{not &1.admin, &1.id})
  end
end
