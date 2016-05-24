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
      recent_results: Poison.encode!(recent_tasks(conn.assigns.current_user)),
      data_sources: Poison.encode!(data_sources(conn))
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

  defp data_sources(conn) do
      for cloak <- Air.CloakInfo.all(conn.assigns.current_user.organisation),
          data_source <- cloak.data_sources
      do
        %{
          id: data_source.id,
          display: "#{data_source.id} (#{cloak.name})",
          tables: data_source.tables,
          cloak: cloak,
          token: data_source_token(cloak.id, data_source.id)
        }
      end
  end

  defp data_source_token(nil, nil), do: nil
  defp data_source_token(cloak_id, data_source) do
    Phoenix.Token.sign(Air.Endpoint, "data_source_token", {cloak_id, data_source})
  end
end
