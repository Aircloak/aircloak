defmodule AirWeb.Admin.DataSourceController do
  @moduledoc """
  Controller for administrators to manage data sources.
  """

  require Logger
  use Air.Web, :admin_controller

  alias Air.Service.{DataSource, User, Warnings, AnalystTable}
  alias AirWeb.Socket.Frontend.UserChannel

  plug(:load_data_source when action in [:show, :edit, :update, :delete, :show_analyst_table, :convert_table_to_view])
  plug(:load_analyst_table when action in [:show_analyst_table, :convert_table_to_view])

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    data_sources =
      DataSource.all()
      |> Enum.sort_by(&{not DataSource.available?(&1.name), &1.name})

    render(
      conn,
      "index.html",
      data_sources: data_sources,
      users_count: DataSource.users_count(),
      data_source_problem_severity: highest_severity_class_map(data_sources),
      full_width: true
    )
  end

  def edit(conn, _params) do
    data_source = conn.assigns.data_source

    render(
      conn,
      "edit.html",
      changeset: DataSource.to_changeset(data_source),
      chosen_groups: data_source.groups
    )
  end

  def update(conn, params) do
    case DataSource.update(conn.assigns.data_source, params["data_source"]) do
      {:ok, data_source} ->
        audit_log(
          conn,
          "Altered data source",
          data_source: data_source.name,
          before: conn.assigns.data_source,
          after: data_source
        )

        conn
        |> put_flash(:info, "Data source updated.")
        |> redirect(to: admin_data_source_path(conn, :index))

      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset, chosen_groups: changeset.data.groups)
    end
  end

  def show(conn, _params) do
    data_source = conn.assigns.data_source

    render(
      conn,
      "show.html",
      data_source: data_source,
      tables: Air.Schemas.DataSource.tables(data_source),
      analyst_tables: AnalystTable.all_for_data_source(data_source),
      conn: conn,
      users: User.data_source_users(data_source),
      problems: Warnings.problems_for_resource(data_source)
    )
  end

  def delete(conn, _params) do
    data_source = conn.assigns.data_source
    data_source_audit_log(conn, data_source, "Data source removal scheduled")
    DataSource.mark_as_pending_delete!(data_source)

    on_success = fn -> data_source_audit_log(conn, data_source, "Data source removal succeeded") end
    on_failure = fn -> data_source_audit_log(conn, data_source, "Data source removal failed") end
    DataSource.delete!(data_source, on_success, on_failure)

    conn
    |> put_flash(:info, "Data source deletion will be performed in the background.")
    |> redirect(to: admin_data_source_path(conn, :index))
  end

  def show_analyst_table(conn, _params) do
    render(conn, "show_analyst_table.html",
      conn: conn,
      analyst_table: conn.assigns.analyst_table,
      data_source: conn.assigns.data_source
    )
  end

  def convert_table_to_view(conn, _params) do
    data_source = conn.assigns.data_source
    analyst_table = conn.assigns.analyst_table

    case AnalystTable.convert_to_view(analyst_table.id) do
      {:ok, view} ->
        audit_log(conn, "Analyst table converted to view",
          name: data_source.name,
          id: data_source.id,
          data_source: data_source.name,
          analyst_table_name: analyst_table.name,
          analyst_table_owner: analyst_table.user.name,
          analyst_table_id: analyst_table.id,
          view_id: view.id
        )

        UserChannel.broadcast_analyst_selectables_change(analyst_table.user, data_source)

        conn
        |> put_flash(:info, ~s(Analyst table "#{analyst_table.name}" has been converted to a view.))
        |> redirect(to: admin_data_source_path(conn, :show, conn.assigns.data_source.name))

      {:error, error} ->
        Logger.error("Error converting analyst table to view: #{inspect(error)}")

        conn
        |> put_flash(
          :error,
          ~s(Failed converting analyst table "#{analyst_table.name}" to view. Please try again later.)
        )
        |> redirect(to: admin_data_source_path(conn, :show, conn.assigns.data_source.name))
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_data_source(conn, _) do
    case DataSource.by_name(conn.params["id"]) do
      nil -> not_found(conn)
      data_source -> assign(conn, :data_source, data_source)
    end
  end

  defp load_analyst_table(conn, _) do
    analyst_table =
      Air.Schemas.AnalystTable
      |> Repo.get(conn.params["table_id"])
      |> Repo.preload(:user)

    case analyst_table do
      nil -> not_found(conn)
      analyst_table -> assign(conn, :analyst_table, analyst_table)
    end
  end

  defp highest_severity_class_map(data_sources),
    do:
      data_sources
      |> Enum.map(&{&1.id, highest_severity_class(&1)})
      |> Enum.into(%{})

  defp highest_severity_class(data_source),
    do:
      data_source
      |> Warnings.problems_for_resource()
      |> Warnings.highest_severity_class()

  defp data_source_audit_log(conn, data_source, text),
    do: audit_log(conn, text, name: data_source.name, id: data_source.id, data_source: data_source.name)
end
