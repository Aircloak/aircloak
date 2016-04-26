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
    tasks =
      Task
      |> Task.permanent
      |> Task.for_user(current_user(conn))
      |> Repo.all
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
    task_map = Map.merge(
          %{
            id: task.id,
            name: task.name,
            query: task.query,
            cloak_id: task.cloak_id,
            tables: task.tables
          },
          data_sources(conn, task)
        )
    render(conn, "editor.html", token: Guardian.Plug.current_token(conn),
        task_json: Poison.encode!(task_map))
  end

  def update(conn, %{"task" => task_params}) do
    changeset = Task.changeset(conn.assigns.task,
        # Tasks are temporary until they are explicitly saved by the user for the first time.
        # Only tasks that are permanent are shown in the interface.
        Map.merge(parse_task_params(task_params), %{"permanent" => true}))
    Repo.update!(changeset)
    json(conn, %{success: true})
  end

  def delete(conn, _params) do
    Repo.delete!(conn.assigns.task)
    conn
    |> put_flash(:info, "Task deleted successfully.")
    |> redirect(to: task_path(conn, :index))
  end

  def run_task(conn, %{"task" => task_params}) do
    task =
      Task.changeset(conn.assigns.task, parse_task_params(task_params))
      |> Ecto.Changeset.apply_changes()

    Air.Socket.Cloak.MainChannel.run_task(task.cloak_id, Task.to_cloak_query(task))

    json(conn, %{success: true})
  end


  # -------------------------------------------------------------------
  # Internal functions
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

  defp data_sources(conn, task) do
    # flatten data sources so it's easier to handle in the task editor
    data_sources =
      for cloak <- Air.CloakInfo.all(current_user(conn).organisation),
          data_source <- cloak.data_sources
      do
        %{
          id: data_source.id,
          display: "#{data_source.id} (#{cloak.name})",
          tables: data_source.tables,
          cloak: cloak,
          token: data_source_token(cloak.id, data_source.id)
        }
      end

    # We need to find the token of the task data source. Tokens have some randomness, so here we
    # ensure that we return the token which exists in the data source list.
    selected_token =
      case Enum.find(
            data_sources,
            fn(data_source) -> data_source.id == task.data_source && data_source.cloak.id == task.cloak_id end
          ) do
        %{} = data_source -> data_source.token
        nil ->
          # Can't find the data source because the cloak is not connected, or data source is not
          # assigned. We'll just encode what we currently have in the task.
          data_source_token(task.cloak_id, task.data_source)
      end

      %{data_sources: data_sources, data_source_token: selected_token}
  end

  defp parse_task_params(task_params) do
    {cloak_id, data_source} = decode_data_source_token(task_params["data_source_token"])
    Map.merge(task_params, %{"cloak_id" => cloak_id, "data_source" => data_source})
  end

  defp data_source_token(nil, nil), do: nil
  defp data_source_token(cloak_id, data_source) do
    Phoenix.Token.sign(Air.Endpoint, "data_source_token", {cloak_id, data_source})
  end

  defp decode_data_source_token(nil), do: {nil, nil}
  defp decode_data_source_token(data_source_token) do
    {:ok, {cloak_id, data_source}} = Phoenix.Token.verify(Air.Endpoint, "data_source_token", data_source_token)
    {cloak_id, data_source}
  end
end
