defmodule Air.Admin.QueryController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  require Logger
  alias Air.Schemas.Query
  alias Plug.CSRFProtection


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def show(conn, %{"id" => query_id}) do
    case Air.Service.Query.get_as_user(conn.assigns.current_user, query_id, load_result: true) do
      {:ok, query} ->
        render(conn, %{
          query: Query.for_display(query),
          guardian_token: Guardian.Plug.current_token(conn),
          csrf_token: CSRFProtection.get_csrf_token(),
        })
      {:error, _} ->
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> render(Air.ErrorView, "404.html")
        |> halt()
    end
  end

  def failed(conn, params) do
    page = params["page"] || 1
    failed_queries = Air.Service.Query.paginated_failed_queries(page)
    render(conn, "failed.html", failed_queries: failed_queries)
  end
end
