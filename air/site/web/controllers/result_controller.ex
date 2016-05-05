defmodule Air.ResultController do
  @moduledoc false
  use Air.Web, :controller

  require Logger
  alias Air.Result

  import Air.TaskController, only: [load_task_and_validate_ownership: 2]
  plug :load_task_and_validate_ownership, "task_id" when action in [:index, :show]


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    # Preload and unpack the task results.
    task = conn.assigns.task |> Repo.preload(results: (from r in Result, order_by: [desc: r.id]))
    results = for result <- task.results, do: Result.unpack(result)

    # Create the list of unique bucket names.
    headers = (for %{buckets: buckets} <- results, {label, value, _count} <- buckets, do: {label, value}) |> Enum.uniq

    # Map the counts to the unique bucket name list.
    default_counts = List.duplicate("", length(headers))
    results = for result = %{buckets: buckets} <- results do
      counts = List.foldl(buckets, default_counts, fn({label, value, count}, counts) ->
            index = Enum.find_index(headers, &[&1 === {label, value}])
            List.replace_at(counts, index, count)
          end)
      Map.put(result, :counts, counts)
    end

    render(conn, "index.html", task: task, headers: headers, results: results)
  end

  def show(conn, params) do
    task = conn.assigns.task
    result = Result |> Repo.get!(params["id"]) |> Result.unpack()
    render(conn, "show.html", task: task, result: result)
  end
end
