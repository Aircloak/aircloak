defmodule Air.QueriesController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  alias Air.Task
  alias Air.Repo

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
      recent_results: Poison.encode!(recent_tasks(conn.assigns.current_user))
    )
  end

  defp recent_tasks(user) do
    user
    |> Task.for_user
    |> Task.recent(_recent_count = 5)
    |> Repo.all
    |> Enum.map(&encode_task/1)
  end

  defp encode_task(task) do
    %{query: task.query}
  end
end
