defmodule Air.Admin.GroupController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Schemas.{Group, User, DataSource}

  plug :load_group when action in [:edit, :update, :delete]


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

  def edit(conn, _params) do
    render(conn, "edit.html", data: edit_form_data(conn))
  end

  def update(conn, params) do
    changeset = Group.changeset(conn.assigns.group, params["group"])
    case Repo.update(changeset) do
      {:ok, group} ->
        audit_log(conn, "Altered group", group: group.name, admin: group.admin)
        conn
        |> put_flash(:info, "Group updated")
        |> redirect(to: admin_group_path(conn, :index))
      {:error, changeset} ->
        render(conn, "edit.html", data: edit_form_data(conn, changeset: changeset))
    end
  end

  def create(conn, params) do
    changeset = Group.changeset(%Group{}, params["group"])
    case Repo.insert(changeset) do
      {:ok, group} ->
        audit_log(conn, "Created group", name: group.name)
        conn
        |> put_flash(:info, "Group created")
        |> redirect(to: admin_group_path(conn, :edit, group))
      {:error, changeset} ->
        render(conn, "index.html", changeset: changeset, groups: all_groups())
    end
  end

  def delete(conn, _params) do
    group = conn.assigns.group
    Repo.delete!(group)
    audit_log(conn, "Removed group", name: group.name)
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

  defp load_group(conn, _) do
    case Repo.get(Group, conn.params["id"]) do
      nil ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> render(Air.ErrorView, "404.html")
        |> halt()
      group ->
        group = Repo.preload(group, [:users, :data_sources])
        assign(conn, :group, group)
    end
  end

  defp edit_form_data(conn, options \\ []) do
    group = conn.assigns.group
    %{
      group: group,
      all_data_sources: Repo.all(DataSource),
      all_users: Repo.all(User),
      changeset: Keyword.get(options, :changeset) || Group.changeset(group),
    }
  end
end
