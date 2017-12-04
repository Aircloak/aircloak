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
    render(conn, "new.html", data: edit_form_data(nil, nil), changeset: User.empty_group_changeset())

  def edit(conn, _params) do
    group = conn.assigns.group
    render(conn, "edit.html", data: edit_form_data(nil, group), group: group,
      changeset: User.group_to_changeset(group))
  end

  def update(conn, params) do
    group = conn.assigns.group
    verify_last_admin_deleted(User.update_group(group, params["group"]), conn,
      fn
        {:ok, group} ->
          audit_log(conn, "Altered group", group: group.name, admin: group.admin)
          conn
          |> put_flash(:info, "Group updated")
          |> redirect(to: admin_group_path(conn, :index))
        {:error, changeset} ->
          render(conn, "edit.html", data: edit_form_data(params, group), changeset: changeset)
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
        render(conn, "new.html", data: edit_form_data(params, nil), changeset: changeset)
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

  defp edit_form_data(params, group) do
    params = Aircloak.atomize_keys(params)
    %{
      selected_user_ids: selected_user_ids(params, group),
      selected_data_source_ids: selected_data_source_ids(params, group),
      all_data_sources: Enum.map(DataSource.all(), & {{&1.name, &1.description}, &1.id}),
      all_users: Enum.map(User.all(), & {{&1.name, &1.email}, &1.id}),
    }
  end

  defp selected_user_ids(params, group)
  defp selected_user_ids(nil, nil), do: []
  defp selected_user_ids(nil, group), do: Enum.map(group.users, & &1.id)
  defp selected_user_ids(params, _), do: to_numerical_ids(get_in(params, [:group, :users]))

  defp selected_data_source_ids(params, group)
  defp selected_data_source_ids(nil, nil), do: []
  defp selected_data_source_ids(nil, group), do: Enum.map(group.data_sources, & &1.id)
  defp selected_data_source_ids(params, _), do: to_numerical_ids(get_in(params, [:group, :data_sources]))

  defp to_numerical_ids(nil), do: []
  defp to_numerical_ids(values), do:
    values
    |> Enum.reject(& &1 == "")
    |> Enum.map(& String.to_integer/1)

  defp verify_last_admin_deleted({:error, :forbidden_last_admin_deletion}, conn, _fun), do:
    conn
    |> put_flash(:error, "The given action cannot be performed, because it would remove the only administrator.")
    |> redirect(to: admin_group_path(conn, :index))
  defp verify_last_admin_deleted(result, _conn, fun), do:
    fun.(result)
end
