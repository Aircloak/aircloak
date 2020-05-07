defmodule AirWeb.ExplorerAnalysisController do
  alias Air.Service.Explorer

  use Air.Web, :controller

  plug(:check_if_enabled)
  plug(:load_data_source)
  plug(:check_if_supported_for_data_source)

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions, do: %{user: :all}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html", analyses: Explorer.results_for_datasource(conn.assigns.data_source))
  end

  def create(conn, _params) do
    data_source = conn.assigns.data_source
    Explorer.reanalyze_datasource(data_source)
    redirect(conn, to: data_source_explorer_analysis_path(conn, :index, data_source.name))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp check_if_enabled(conn, _opts) do
    if Explorer.enabled?() do
      conn
    else
      render(conn, "not_enabled.html")
    end
  end

  defp load_data_source(conn, _opts) do
    data_source_name = Map.fetch!(conn.params, "data_source_id")

    case Air.Service.DataSource.fetch_as_user(
           {:name, data_source_name},
           conn.assigns.current_user
         ) do
      {:ok, data_source} ->
        conn
        |> assign(:data_source, data_source)

      _ ->
        conn
        |> put_flash(:error, "Data source not found.")
        |> redirect(to: data_source_path(conn, :index))
    end
  end

  defp check_if_supported_for_data_source(conn, _opts) do
    if Explorer.data_source_supported?(conn.assigns.data_source) do
      conn
    else
      conn
      |> put_flash(:error, "Data source '#{conn.assigns.data_source.name}' is not enabled with Diffix Explorer.")
      |> redirect(to: data_source_path(conn, :index))
    end
  end
end
