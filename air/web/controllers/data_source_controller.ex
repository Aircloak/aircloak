defmodule Air.DataSourceController do
  @moduledoc """
  Controller that allows users to select and query a datasource
  """

  use Air.Web, :controller

  alias Air.{Service.Query, Service.DataSource}
  alias Air.Service.DataSource
  alias Plug.CSRFProtection


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: [:index, :show, :redirect_to_last_used],
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    user_data_sources = DataSource.for_user(conn.assigns.current_user)
    render(conn, "index.html",
      user_data_sources: user_data_sources,
      data_source_count: DataSource.count(),
      conn: conn,
    )
  end

  def show(conn, %{"id" => id}) do
    with {:ok, data_source} <- DataSource.fetch_as_user({:id, id}, conn.assigns.current_user),
         {:ok, last_query} <- DataSource.last_query({:id, id}, conn.assigns.current_user)
    do
      pending_queries = Air.Service.Query.currently_running(conn.assigns.current_user, data_source)
      |> Enum.map(&Air.Schemas.Query.for_display/1)

      conn
      |> put_layout("raw.html")
      |> render(
        "show.html",
        data_source: data_source,
        views: Air.Service.View.all(conn.assigns.current_user, data_source),
        pending_queries: pending_queries,
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

  def redirect_to_last_used(conn, _params) do
    conn.assigns.current_user
    |> Query.last_for_user()
    |> case do
      %{data_source_id: data_source_id} when data_source_id != nil ->
        redirect(conn, to: data_source_path(conn, :show, data_source_id))
      _ ->
        redirect(conn, to: data_source_path(conn, :index))
    end
  end
end
