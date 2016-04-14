defmodule Air.TaskController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Task

  plug :scrub_params, "task" when action in [:create, :update]


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
    tasks = Repo.preload(current_user(conn), :tasks).tasks
    render(conn, "index.html", tasks: tasks)
  end

  def new(conn, _params) do
    # We create a new task immediately, persist it to the database,
    # and then redirect to the edit page. This way we operate on an
    # existing task from the get go, and can easily update it in place.
    task_params = %{
      query: "-- Add your query code here",
      name: "Untitled query"
    }
    changeset = build_assoc(current_user(conn), :tasks)
    changeset = Task.changeset(changeset, task_params)
    case Repo.insert(changeset) do
      {:ok, task} ->
        redirect(conn, to: task_path(conn, :edit, task))
      {:error, _} ->
        conn
        |> put_flash(:error, "Unfortunately the task could not be created")
        |> redirect(to: task_path(conn, :index))
    end
  end

  def edit(conn, %{"id" => id}) do
    with_task(conn, id, fn(%{id: id, name: name, query: query}) ->
          task_map = %{
            id: id,
            name: name,
            query: query
          }
          render(conn, "editor.html", task_json: Poison.encode!(task_map))
        end)
  end

  def update(conn, %{"id" => id, "task" => task_params}) do
    with_task(conn, id, fn(task) ->
          changeset = Task.changeset(task, task_params)
          Repo.update!(changeset)
          json(conn, %{success: true})
        end)
  end

  def delete(conn, %{"id" => id}) do
    with_task(conn, id, fn(task) ->
          Repo.delete!(task)
          conn
          |> put_flash(:info, "Task deleted successfully.")
          |> redirect(to: task_path(conn, :index))
        end)
  end

  def run_task(conn, %{"id" => id, "task" => _task_params}) do
    with_task(conn, id, fn(_task) ->
          # TODO: Schedule task running here...
          json(conn, %{success: true})
        end)
  end


  # -------------------------------------------------------------------
  # Private methods
  # -------------------------------------------------------------------

  defp current_user(conn) do
    Guardian.Plug.current_resource(conn)
  end

  defp with_task(conn, id, fun) do
    task = Repo.get!(Task, id)
    if task.user_id == current_user(conn).id do
      fun.(task)
    else
      # Raise a 404 if the user isn't the right one.
      # This way we avoid leaking information if someone
      # is trying to enumerate all tasks.
      conn
      |> put_status(:not_found)
      |> render(Air.ErrorView, "404.html")
    end
  end
end
