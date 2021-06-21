defmodule AirWeb.Admin.GroupController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Service.{DataSource, User, Group, LDAP}

  plug(:load_group when action in [:edit, :update, :delete])

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    {groups, ldap_groups} = Group.all() |> Enum.sort_by(& &1.name) |> Enum.split_with(&(&1.source == :native))
    render(conn, "index.html", groups: groups, ldap_enabled?: LDAP.enabled?(), ldap_groups: ldap_groups)
  end

  def new(conn, _params),
    do:
      render(
        conn,
        "new.html",
        data: edit_form_data(nil, nil),
        changeset: Group.empty_changeset()
      )

  def edit(conn, _params) do
    group = conn.assigns.group

    render(
      conn,
      "edit.html",
      data: edit_form_data(nil, group),
      group: group,
      changeset: Group.to_changeset(group)
    )
  end

  def update(conn, params) do
    old_group = conn.assigns.group

    verify_last_admin_deleted(update_group(old_group, params["group"]), conn, fn
      {:ok, group} ->
        audit_log(conn, "Altered group", group_name: group.name, group_id: group.id, before: old_group, after: group)

        conn
        |> put_flash(:info, "Group updated.")
        |> redirect(to: admin_group_path(conn, :index))

      {:error, changeset} ->
        render(conn, "edit.html", data: edit_form_data(params, old_group), changeset: changeset)
    end)
  end

  def create(conn, params) do
    case Group.create(params["group"]) do
      {:ok, group} ->
        audit_log(conn, "Created group", group_name: group.name, group_id: group.id)

        conn
        |> put_flash(:info, "Group created.")
        |> redirect(to: admin_group_path(conn, :index))

      {:error, changeset} ->
        render(conn, "new.html", data: edit_form_data(params, nil), changeset: changeset)
    end
  end

  def delete(conn, _params) do
    group = conn.assigns.group

    verify_last_admin_deleted(Group.delete(group), conn, fn {:ok, _} ->
      audit_log(conn, "Removed group", group_name: group.name)

      conn
      |> put_flash(:info, "Group deleted.")
      |> redirect(to: admin_group_path(conn, :index))
    end)
  end

  def sync_ldap(conn, _params) do
    case LDAP.sync() do
      :ok -> put_flash(conn, :info, "LDAP sync finished.")
      {:error, :timeout} -> put_flash(conn, :info, "LDAP sync is being performed in the background.")
      {:error, error} -> put_flash(conn, :error, "LDAP sync failed, reason: #{inspect(error)}.")
    end
    |> redirect(to: admin_group_path(conn, :index))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_group(conn, _) do
    case Group.load(conn.params["id"]) do
      nil -> not_found(conn)
      group -> assign(conn, :group, group)
    end
  end

  defp edit_form_data(params, group) do
    params = Aircloak.atomize_keys(params)

    %{
      selected_user_ids: selected_user_ids(params, group),
      selected_data_source_ids: selected_data_source_ids(params, group),
      all_data_sources: Enum.map(DataSource.all(), &{{&1.name, &1.description}, &1.id}),
      all_users: Enum.map(all_users(group), &{{&1.name, User.main_login(&1)}, &1.id})
    }
  end

  defp all_users(%{source: :ldap}), do: User.all()
  defp all_users(_), do: User.all_native()

  defp selected_user_ids(params, group)
  defp selected_user_ids(nil, nil), do: []
  defp selected_user_ids(nil, group), do: Enum.map(group.users, & &1.id)
  defp selected_user_ids(params, _), do: to_numerical_ids(get_in(params, [:group, :users]))

  defp selected_data_source_ids(params, group)
  defp selected_data_source_ids(nil, nil), do: []
  defp selected_data_source_ids(nil, group), do: Enum.map(group.data_sources, & &1.id)

  defp selected_data_source_ids(params, _), do: to_numerical_ids(get_in(params, [:group, :data_sources]))

  defp to_numerical_ids(nil), do: []

  defp to_numerical_ids(values),
    do:
      values
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&String.to_integer/1)

  defp verify_last_admin_deleted({:error, :forbidden_no_active_admin}, conn, _fun),
    do:
      conn
      |> put_flash(
        :error,
        "The given action cannot be performed, because it would remove the only administrator."
      )
      |> redirect(to: admin_group_path(conn, :index))

  defp verify_last_admin_deleted(result, _conn, fun), do: fun.(result)

  defp update_group(group = %{source: :ldap}, params), do: Group.update_data_sources(group, params)
  defp update_group(group = %{source: :native}, params), do: Group.update(group, params)
end
