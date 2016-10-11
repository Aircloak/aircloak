defmodule Air.DataSourceController do
  @moduledoc """
  Controller that allows users to select and query a datasource
  """

  use Air.Web, :controller

  alias Air.{DataSource, Query, DataSourceManager}
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
    data_sources = DataSource
    |> DataSource.for_user(conn.assigns.current_user)
    |> Repo.all()
    {available_data_sources, unavailable_data_sources} = Enum.partition(data_sources,
      &(DataSourceManager.available?(&1.global_id)))
    data_source_count = Repo.aggregate(DataSource, :count, :id)
    render(conn, "index.html",
      available_data_sources: available_data_sources,
      unavailable_data_sources: unavailable_data_sources,
      data_source_count: data_source_count,
      conn: conn,
    )
  end

  def show(conn, %{"id" => id}) do
    if DataSource.available_to_user?(id, conn.assigns.current_user) do
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
    else
      conn
      |> put_flash(:error, "Not permitted to access data source")
      |> redirect(to: data_source_path(conn, :index))
      |> halt()
    end
  end
end
