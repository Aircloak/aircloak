defmodule Air.Monitoring.FailedQueries do
  @moduledoc "Logs failed queries as JSON with details."

  @doc false
  @spec start_link() :: {:ok, pid}
  def start_link, do: Task.start_link(&work/0)

  alias Air.{Repo, Query, QueryEvents}


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

    message =
      Repo.get!(Query, query_id)
      |> Map.take([:statement, :cloak_id, :data_source])
      |> Map.merge(%{type: "failed_query", message: error})
      |> Poison.encode!()

    Logger.error("JSON_LOG #{message}")
  end
end
