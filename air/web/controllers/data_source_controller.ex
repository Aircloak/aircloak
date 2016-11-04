defmodule Air.DataSourceController do
  @moduledoc """
  Controller that allows users to select and query a datasource
  """

  use Air.Web, :controller

  alias Air.{DataSourceManager, Service.DataSource}
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
    data_sources = DataSource.for_user(conn.assigns.current_user)
    {available_data_sources, unavailable_data_sources} = Enum.partition(data_sources,
      &(DataSourceManager.available?(&1.global_id)))
    render(conn, "index.html",
      available_data_sources: available_data_sources,
      unavailable_data_sources: unavailable_data_sources,
      data_source_count: DataSource.count(),
      conn: conn,
    )
  end

  def show(conn, %{"id" => id}) do
    with {:ok, data_source} <- DataSource.fetch_as_user({:id, id}, conn.assigns.current_user),
         {:ok, last_query} <- DataSource.last_query({:id, id}, conn.assigns.current_user)
    do
      conn
      |> put_layout("raw.html")
      |> render(
        "show.html",
        data_source: data_source,
        guardian_token: Guardian.Plug.current_token(conn),
        csrf_token: CSRFProtection.get_csrf_token(),
        last_query: last_query,
        session_id: Ecto.UUID.generate()
      )
    else
      _ ->
        conn
        |> put_flash(:error, "Not permitted to access data source")
        |> redirect(to: data_source_path(conn, :index))
        |> halt()
    end
  end
end
