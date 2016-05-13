defmodule Air.ResultController do
  @moduledoc false
  use Air.Web, :controller

  require Logger
  alias Air.Result

  import Air.TaskController, only: [load_task_and_validate_ownership: 2]
  plug :load_task_and_validate_ownership, "task_id" when action in [:index, :show, :delete]


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
    results = for result <- task.results, do: Result.unpack(result, 10)

    # Create the list of unique bucket names.
    headers = (for %{buckets: buckets} <- results, {label, value, _count} <- buckets, do: {label, value}) |> Enum.uniq

    # Map the counts to the unique bucket name list.
    results = for result = %{buckets: buckets} <- results do
      counts = for header <- headers do
        Enum.find_value(buckets, "", fn ({label, value, count}) -> {label, value} === header && count end)
      end
      Map.put(result, :counts, counts)
    end

    render(conn, "index.html", task: task, headers: headers, results: results)
  end

  def show(conn, params) do
    task = conn.assigns.task
    result = Result |> Repo.get!(params["id"]) |> Result.unpack(100)
    render(conn, "show.html", task: task, result: result)
  end

  def delete(conn, %{"id" => "all"}) do
    task = conn.assigns.task
    Result.delete_all!(task)
    conn
    |> put_flash(:info, "Task results deleted successfully.")
    |> redirect(to: task_result_path(conn, :index, task.id))
  end
  def delete(conn, %{"id" => id}) do
    task = conn.assigns.task
    Result.delete!(id)
    conn
    |> put_flash(:info, "Task result deleted successfully.")
    |> redirect(to: task_result_path(conn, :index, task.id))
  end
end
