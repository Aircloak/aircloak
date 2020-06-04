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
      all_data_sources: Enum.map(Air.Service.DataSource.all(), &{{&1.name, &1.description}, &1.id})
    )
  end

  def create(conn, params) do
    group = Explorer.group()

    case Group.update(group, params["group"]) do
      {:ok, group} ->
        audit_log(conn, "Altered group", group: group.name, admin: group.admin)

        Explorer.reanalyze_all()

        conn
        |> put_flash(:info, "Diffix Explorer settings updated. It can take some time before you see new results.")
        |> redirect(to: admin_explorer_path(conn, :index))

      {:error, changeset} ->
        render(conn, "show.html",
          group: group,
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
    redirect(conn, to: admin_explorer_path(conn, :show, data_source.name))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp check_if_enabled(conn, _opts) do
    if Explorer.enabled?() do
      conn
    else
      render(conn, "disabled.html")
    end
  end

  defp prepare_debug(conn, _opts) do
    data_source_name = Map.fetch!(conn.params, "id")

    if conn.assigns.current_user.debug_mode_enabled do
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
    else
      conn
      |> put_flash(:error, "Debug mode not enabeld")
      |> redirect(to: admin_explorer_path(conn, :index))
    end
  end
end
