defmodule AirWeb.DataSourceController do
  @moduledoc """
  Controller that allows users to select and query a datasource
  """

  use Air.Web, :controller

  alias Air.{Service.Query, Service.DataSource}
  alias Air.Service.DataSource
  alias Plug.CSRFProtection

  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
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

    render(
      conn,
      "index.html",
      user_data_sources: user_data_sources,
      data_source_count: DataSource.count(),
      conn: conn
    )
  end

  def show(conn, %{"id" => name}) do
    with {:ok, data_source} <- DataSource.fetch_as_user({:name, name}, conn.assigns.current_user),
         {:ok, last_query} <- DataSource.last_query({:name, name}, conn.assigns.current_user, :http) do
      pending_queries =
        Air.Service.Query.currently_running(conn.assigns.current_user, data_source, :http)
        |> Enum.map(&Air.Schemas.Query.for_display/1)

      conn
      |> put_layout("raw.html")
      |> render(
        "show.html",
        data_source: data_source,
        pending_queries: pending_queries,
        guardian_token: Air.Guardian.Plug.current_token(conn),
        csrf_token: CSRFProtection.get_csrf_token(),
        last_query: if(last_query != nil, do: Air.Schemas.Query.for_display(last_query)),
        session_id: Ecto.UUID.generate(),
        number_format: Air.Service.User.number_format_settings(conn.assigns.current_user),
        debug_mode_enabled: conn.assigns.current_user.debug_mode_enabled
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
    |> Query.last_for_user(:http)
    |> case do
      nil -> redirect(conn, to: data_source_path(conn, :index))
      query -> redirect(conn, to: data_source_path(conn, :show, query.data_source.name))
    end
  end
end
