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
      user: :all,
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    data_sources = Repo.all(DataSource)
    {available_data_sources, unavailable_data_sources} = Enum.partition(data_sources,
      &(DataSourceManager.available?(&1.unique_id)))
    render(conn, "index.html",
      available_data_sources: available_data_sources,
      unavailable_data_sources: unavailable_data_sources
    )
  end

  def show(conn, %{"data_source_id" => id}) do
    data_source = Repo.get!(DataSource, id)
    last_query = case Query.load_recent_queries(conn.assigns.current_user, data_source, 1) do
      [query] -> query
      _ -> nil
    end

    render(conn, "show.html",
      data_source: data_source,
      guardian_token: Guardian.Plug.current_token(conn),
      csrf_token: CSRFProtection.get_csrf_token(),
      last_query: last_query,
    )
  end
end
