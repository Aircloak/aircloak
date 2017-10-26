defmodule Air.Admin.DataSourceController do
  @moduledoc """
  Controller for administrators to manage data sources.
  """

  use Air.Web, :admin_controller

  alias Air.Service.{DataSource, User, Warnings}

  plug :load_data_source when action in [:show, :edit, :update, :delete]


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
    data_sources =
      DataSource.all()
      |> Enum.sort_by(&{not DataSource.available?(&1.name), &1.name})

    render(conn, "index.html", data_sources: data_sources, users_count: DataSource.users_count(),
      data_source_problem_severity: highest_severity_class_map(data_sources))
  end

  def edit(conn, _params) do
    data_source = conn.assigns.data_source
    render(conn, "edit.html", changeset: DataSource.to_changeset(data_source), chosen_groups: data_source.groups)
  end

  def update(conn, params) do
    case DataSource.update(conn.assigns.data_source, params["data_source"]) do
      {:ok, data_source} ->
        audit_log(conn, "Altered data source", name: data_source.name, data_source: data_source.name)
        conn
        |> put_flash(:info, "Data source updated")
        |> redirect(to: admin_data_source_path(conn, :index))
      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset, chosen_groups: changeset.data.groups)
    end
  end

  def show(conn, _params) do
    data_source = conn.assigns.data_source

    render(conn, "show.html",
      data_source: data_source,
      conn: conn,
      users: User.data_source_users(data_source),
      problems: Warnings.problems_for_resource(data_source)
    )
  end

  def delete(conn, _params) do
    data_source = conn.assigns.data_source
    DataSource.delete!(data_source)
    audit_log(conn, "Removed data source", name: data_source.name,
      id: data_source.id, data_source: data_source.name)
    conn
    |> put_flash(:info, "Data source deleted")
    |> redirect(to: admin_data_source_path(conn, :index))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_data_source(conn, _) do
    case DataSource.by_name(conn.params["id"]) do
      nil ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> put_view(Air.ErrorView)
        |> render("404.html")
        |> halt()
      data_source ->
        assign(conn, :data_source, data_source)
    end
  end

  defp highest_severity_class_map(data_sources), do:
    data_sources
    |> Enum.map(&({&1.id, highest_severity_class(&1)}))
    |> Enum.into(%{})

  defp highest_severity_class(data_source), do:
    data_source
    |> Warnings.problems_for_resource()
    |> Warnings.highest_severity_class()
end
