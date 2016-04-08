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
    changeset = Task.changeset(%Task{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"task" => task_params}) do
    changeset = build_assoc(current_user(conn), :tasks)
    changeset = Task.changeset(changeset, task_params)

    case Repo.insert(changeset) do
      {:ok, _task} ->
        conn
        |> put_flash(:info, "Task created successfully.")
        |> redirect(to: task_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def edit(conn, %{"id" => id}) do
    with_task(conn, id, fn(task) ->
          changeset = Task.changeset(task)
          render(conn, "edit.html", task: task, changeset: changeset)
        end)
  end

  def update(conn, %{"id" => id, "task" => task_params}) do
    with_task(conn, id, fn(task) ->
          changeset = Task.changeset(task, task_params)
          case Repo.update(changeset) do
            {:ok, _task} ->
              conn
              |> put_flash(:info, "Task updated successfully.")
              |> redirect(to: task_path(conn, :index))
            {:error, changeset} ->
              render(conn, "edit.html", task: task, changeset: changeset)
          end
        end)
  end

  def delete(conn, %{"id" => id}) do
    with_task(conn, id, fn(task) ->
          # Here we use delete! (with a bang) because we expect
          # it to always work (and if it does not, it will raise).
          Repo.delete!(task)

          conn
          |> put_flash(:info, "Task deleted successfully.")
          |> redirect(to: task_path(conn, :index))
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
