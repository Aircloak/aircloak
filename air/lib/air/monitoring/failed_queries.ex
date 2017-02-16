defmodule Air.Monitoring.FailedQueries do
  @moduledoc "Logs failed queries as JSON with details."

  @doc false
  @spec start_link() :: {:ok, pid}
  def start_link, do: Task.start_link(&work/0)

  alias Air.{Repo, Schemas.Query, QueryEvents.Results}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp work do
    for {:result, result} <- Results.stream do
      if result["error"], do: log_error(result)
    end
  end

  defp log_error(%{"error" => error, "query_id" => query_id}) do
    import Logger, warn: false

    query = Repo.get(Query, query_id) |> Repo.preload([:user])
    user = query.user

    message = %{
      type: "failed_query",
      message: error,
      statement: query.statement,
      data_source_id: query.data_source_id,
      user_id: user.id,
      user_email: user.email
    }

    Logger.error("JSON_LOG #{Poison.encode!(message)}")
  end
end
