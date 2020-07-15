defmodule AirWeb.Admin.ExplorerController do
  use Air.Web, :admin_controller

  alias Air.Service.Explorer
  alias Air.Service.Group

  plug(:check_if_enabled)
  plug(:prepare_debug when action in [:show, :update])

  def permissions do
    %{
      admin: :all
    }
  end

  def index(conn, _params) do
    group = Explorer.group()

    render(conn, "index.html",
      data_sources: Explorer.statistics(),
      changeset: Group.to_changeset(group),
      all_data_sources: Enum.map(Air.Service.DataSource.all(), &{{&1, selected_tables(&1)}, &1.id})
    )
  end

  def create(conn, params) do
    case Explorer.change_permitted_data_sources(params["group"]) do
      {:ok, group} ->
        audit_log(conn, "Altered Diffix Explorer permissions", group: group.name, admin: group.admin)

        conn
        |> put_flash(:info, "Diffix Explorer settings updated. It can take some time before you see new results.")
        |> redirect(to: admin_explorer_path(conn, :index))

      {:error, changeset} ->
        render(conn, "show.html",
          group: Explorer.group(),
          changeset: changeset,
          all_data_sources: Enum.map(Air.Service.DataSource.all(), &{{&1.name, &1.description}, &1.id})
        )
    end
  end

  def show(conn, _params) do
    render(conn, "show.html", analyses: Explorer.results_for_datasource(conn.assigns.data_source))
  end

  def update(conn, _params) do
    data_source = conn.assigns.data_source
    Explorer.reanalyze_datasource(data_source)
    redirect(conn, to: admin_explorer_path(conn, :index))
  end

  def reanalyze_all(conn, _params) do
    Air.Service.DataSource.all()
    |> Enum.filter(&Explorer.data_source_enabled?/1)
    |> Enum.each(&Explorer.reanalyze_datasource/1)

    conn
    |> put_flash(:info, "Reanalyzing all data sources.")
    |> redirect(to: admin_explorer_path(conn, :index))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp selected_tables(data_source) do
    if Explorer.data_source_enabled?(data_source) do
      Enum.map(Explorer.results_for_datasource(data_source), & &1.table_name)
    else
      Explorer.elligible_tables_for_datasource(data_source)
    end
  end

  defp check_if_enabled(conn, _opts) do
    if Explorer.enabled?() do
      conn
    else
      render(conn, "disabled.html")
    end
  end

  defp prepare_debug(conn, _opts) do
    data_source_name = Map.fetch!(conn.params, "id")

    case Air.Service.DataSource.fetch_as_user(
           {:name, data_source_name},
           Explorer.user()
         ) do
      {:ok, data_source} ->
        if Explorer.data_source_enabled?(data_source) do
          conn
          |> assign(:data_source, data_source)
        else
          conn
          |> put_flash(:error, "Data source '#{conn.assigns.data_source.name}' is not enabled with Diffix Explorer.")
          |> redirect(to: admin_explorer_path(conn, :index))
        end

      _ ->
        conn
        |> put_flash(:error, "Data source not found.")
        |> redirect(to: admin_explorer_path(conn, :index))
    end
  end
end
