defmodule Air.QueriesController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(conn, "index.html",
      guardian_token: Guardian.Plug.current_token(conn),
      csrf_token: Plug.CSRFProtection.get_csrf_token(),
      recent_tasks: "{}"
    )
  end
end
