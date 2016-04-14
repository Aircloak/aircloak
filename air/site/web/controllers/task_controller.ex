defmodule Air.TaskController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.Task

  plug :scrub_params, "task" when action in [:create, :update]
  plug :load_task_and_validate_ownership when action in [:edit, :update, :delete, :run_task]


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

  def edit(conn, _params) do
    task = conn.assigns.task
    task_map = %{
      id: task.id,
      name: task.name,
      query: task.query
    }
    render(conn, "editor.html", task_json: Poison.encode!(task_map))
  end

  def update(conn, %{"task" => task_params}) do
    changeset = Task.changeset(conn.assigns.task, task_params)
    Repo.update!(changeset)
    json(conn, %{success: true})
  end

  def delete(conn, _params) do
    Repo.delete!(conn.assigns.task)
    conn
    |> put_flash(:info, "Task deleted successfully.")
    |> redirect(to: task_path(conn, :index))
  end

  def run_task(conn, %{"task" => _task_params}) do
    # TODO: Schedule task running here...
    #       But make sure you run the task with the parameters
    #       passed in with the request, rather than those of
    #       the `conn.assigns.task` record. This is quite important
    #       as the task might not have been saved.
    json(conn, %{success: true})
  end


  # -------------------------------------------------------------------
  # Private methods
  # -------------------------------------------------------------------

  defp current_user(conn) do
    Guardian.Plug.current_resource(conn)
  end

  defp load_task_and_validate_ownership(conn, _) do
    %{"id" => id} = conn.params
    task = Repo.get!(Task, id)
    if task.user_id != current_user(conn).id do
      # Raise a 404 if the user isn't the right one.
      # This way we avoid leaking information if someone
      # is trying to enumerate all tasks.
      conn
      |> put_status(:not_found)
      |> render(Air.ErrorView, "404.html")
      |> halt
    else
      assign(conn, :task, task)
    end
  end
end
