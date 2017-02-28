defmodule Air.Admin.QueryController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  require Logger
  alias Air.{Schemas.Query, Repo}


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
    case Air.Service.Query.get(query_id) do
      {:ok, query} ->
        render(conn, %{
          query: Query.for_display(query),
          guardian_token: Guardian.Plug.current_token(conn),
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
    failed_queries = Repo.paginate(Query.failed(), page: page)
    render(conn, "failed.html", failed_queries: failed_queries)
  end
end
