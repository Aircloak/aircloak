defmodule Air.ResultController do
  @moduledoc false
  use Air.Web, :controller

  require Logger
  alias Air.Result

  plug :load_task_and_validate_ownership when action in [:index, :show]


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
    headers =
        List.foldl(results, [], fn (%{buckets: buckets}, headers) ->
          List.foldl(buckets, headers, fn ({label, value, _count}, headers) ->
                [{label, value} | headers]
              end)
        end)
        |> Enum.uniq
        |> Enum.reverse

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


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_task_and_validate_ownership(conn, opts), do: Air.TaskController.load_task_and_validate_ownership(conn, opts)
end
