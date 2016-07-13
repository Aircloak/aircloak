defmodule Air.Monitoring.FailedQueries do
  @moduledoc "Logs failed queries as JSON with details."

  @doc false
  @spec start_link() :: {:ok, pid}
  def start_link, do: Task.start_link(&work/0)

  alias Air.{Repo, Query, QueryEvents}
  import Ecto.Query


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp work do
    for {:result, result} <- QueryEvents.stream do
      if result["error"], do: log_error(result)
    end
  end

  defp log_error(%{"error" => error, "query_id" => query_id}) do
    import Logger, warn: false

    db_query = from q in Query,
      where: q.id == ^query_id,
      preload: [{:user, :organisation}],
      select: q

    query = Repo.one!(db_query)
    user = query.user
    organisation = user.organisation

    message = %{type: "failed_query", message: error}
      |> Map.merge(Map.take(query, [:statement, :cloak_id, :data_source]))
      |> Map.merge(%{user_email: user.email, user_id: user.id})
      |> Map.merge(%{organisation_name: organisation.name, organisation_id: organisation.id})
      |> Poison.encode!()

    Logger.error("JSON_LOG #{message}")
  end
end
