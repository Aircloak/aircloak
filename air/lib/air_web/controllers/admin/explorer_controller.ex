defmodule AirWeb.Admin.ExplorerController do
  use Air.Web, :admin_controller

  alias Air.Service.Explorer
  import Phoenix.LiveView.Controller

  plug(:check_if_enabled)
  plug(:prepare_debug when action in [:show])
  plug(:disable_layout_flash when action in [:index])

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    live_render(conn, AirWeb.Admin.ExplorerLive.Index)
  end

  def show(conn, _params) do
    render(conn, "show.html", analyses: Explorer.results_for_datasource(conn.assigns.data_source))
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

  defp disable_layout_flash(conn, _opts), do: assign(conn, :hide_flash_messages, true)
end
