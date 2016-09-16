defmodule Air.DataSourceController do
  @moduledoc """
  Controller that allows users to select and query a datasource
  """

  use Air.Web, :controller

  alias Air.{DataSource, Query, DataSourceManager, AuditLog}
  alias Plug.CSRFProtection


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: [:index, :show],
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    data_sources = Repo.all(DataSource)
    {available_data_sources, unavailable_data_sources} = Enum.partition(data_sources,
      &(DataSourceManager.available?(&1.global_id)))
    render(conn, "index.html",
      available_data_sources: available_data_sources,
      unavailable_data_sources: unavailable_data_sources,
      conn: conn,
    )
  end

  def show(conn, %{"id" => id}) do
    data_source = Repo.get!(DataSource, id)
    last_query = case Query.load_recent_queries(conn.assigns.current_user, data_source, 1) do
      [query] -> query
      _ -> nil
    end

    conn
    |> put_layout("raw.html")
    |> render("show.html",
      data_source: data_source,
      guardian_token: Guardian.Plug.current_token(conn),
      csrf_token: CSRFProtection.get_csrf_token(),
      last_query: last_query,
    )
  end

  def delete(conn, %{"id" => id}) do
    data_source = Repo.get!(DataSource, id)
    Repo.delete!(data_source)
    AuditLog.log(conn, "Removed data source", name: data_source.name, global_id: data_source.global_id)
    conn
    |> put_flash(:info, "Data source deleted")
    |> redirect(to: data_source_path(conn, :index))
  end
end
