defmodule AirWeb.Admin.GroupController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.{DataSource, User}

  plug :load_group when action in [:edit, :update, :delete]


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

  def index(conn, _params), do:
    render(conn, "index.html", groups: User.all_groups())

  def new(conn, _params), do:
    render(conn, "new.html", data: edit_form_data(conn, changeset: User.empty_group_changeset()))

  def edit(conn, _params), do:
    render(conn, "edit.html", data: edit_form_data(conn))

  def update(conn, params) do
    verify_last_admin_deleted(User.update_group(conn.assigns.group, params["group"]), conn,
      fn
        {:ok, group} ->
          audit_log(conn, "Altered group", group: group.name, admin: group.admin)
          conn
          |> put_flash(:info, "Group updated")
          |> redirect(to: admin_group_path(conn, :index))
        {:error, changeset} ->
          render(conn, "edit.html", data: edit_form_data(conn, changeset: changeset))
      end
    )
  end

  def create(conn, params) do
    case User.create_group(params["group"]) do
      {:ok, group} ->
        audit_log(conn, "Created group", name: group.name)
        conn
        |> put_flash(:info, "Group created")
        |> redirect(to: admin_group_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", data: edit_form_data(conn, changeset: changeset))
    end
  end

  def delete(conn, _params) do
    group = conn.assigns.group
    verify_last_admin_deleted(User.delete_group(group), conn, fn({:ok, _}) ->
      audit_log(conn, "Removed group", name: group.name)
      conn
      |> put_flash(:info, "Group deleted")
      |> redirect(to: admin_group_path(conn, :index))
    end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_group(conn, _) do
    case User.load_group(conn.params["id"]) do
      nil ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> put_view(AirWeb.ErrorView)
        |> render("404.html")
        |> halt()
      group ->
        assign(conn, :group, group)
    end
  end

  defp edit_form_data(conn, options \\ []) do
    group = group(conn.assigns[:group])
    %{
      group: group,
      all_data_sources: DataSource.all(),
      all_users: User.all(),
      changeset: Keyword.get(options, :changeset) || User.group_to_changeset(group),
    }
  end

  defp group(nil), do: %{users: [], data_sources: []}
  defp group(group), do: group

  defp verify_last_admin_deleted({:error, :forbidden_last_admin_deletion}, conn, _fun), do:
    conn
    |> put_flash(:error, "The given action cannot be performed, because it would remove the only administrator.")
    |> redirect(to: admin_group_path(conn, :index))
  defp verify_last_admin_deleted(result, _conn, fun), do:
    fun.(result)
end
