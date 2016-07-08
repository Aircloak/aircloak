defmodule Air.Monitoring.FailedQueries do
  def start_link, do: Task.start_link(&work/0)

  defp work do
    for {:result, result} <- Air.QueryEvents.stream do
      if result["error"], do: log_error(result)
    end
  end

  defp log_error(%{"error" => error, "query_id" => query_id}) do
    import Logger, warn: false

    message =
      Air.Repo.get!(Air.Query, query_id)
      |> Map.take([:statement, :cloak_id, :data_source])
      |> Map.merge(%{type: "failed_query", message: error})
      |> Poison.encode!()

    Logger.error("JSON_LOG #{message}")
  end
end
