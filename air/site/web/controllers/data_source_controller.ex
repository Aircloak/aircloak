defmodule Air.DataSourceController do
  @moduledoc """
  Controller that allows users to select and query a datasource
  """

  use Air.Web, :controller

  alias Air.{DataSource, Query}
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
    data_sources = Repo.all(DataSource) |> Repo.preload(:cloak)
    render(conn, "index.html", data_sources: data_sources)
  end

  def show(conn, %{"data_source_id" => id}) do
    data_source = Repo.get!(DataSource, id) |> Repo.preload(:cloak)
    last_query = case Query.load_recent_queries(conn.assigns.current_user, data_source, 1) do
      [query] -> query
      _ -> nil
    end

    render(conn, "show.html",
      data_source: data_source,
      guardian_token: Guardian.Plug.current_token(conn),
      csrf_token: CSRFProtection.get_csrf_token(),
      last_query: Poison.encode!(last_query),
    )
  end
end
