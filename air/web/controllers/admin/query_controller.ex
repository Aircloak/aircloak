defmodule Air.Admin.QueryController do
  @moduledoc false
  use Air.Web, :admin_controller
  use Timex

  require Logger
  alias Air.{Query, Repo}


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

  def failed(conn, params) do
    page = params["page"] || 1
    failed_queries = Repo.paginate(Query.failed(), page: page)
    render(conn, "failed.html", failed_queries: failed_queries)
  end
end
