defmodule Air.API.TaskController do
  @moduledoc false
  use Air.Web, :controller

  require Logger
  alias Air.Task


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

  @doc """
  Executes a task sent in by the API user, reusing the standard task execution pipeline.

  The following parameters are required:

  - query: the lua task to be executed
  - cloak: the name of the node on which the task should execute. At this time no checks are performed
      to ensure that the cloak belongs to the user's organisation.
  - data_source: the name of the data source in the selected cloak
  - tables: a list of tables that should be made available to the task

  You can test this endpoint in your local development environment using the following curl command:
  curl --data '{"query":"report_property(\"Hello\", \"world\")", "cloak":"nonode@nohost", \
      "data_source":"local", "tables":["test"]}' \
      -H "auth-token: <valid auth token>" \
      -H "content-type: application/json" -k -XPOST \
      https://insights.air-local:20000/api/task
  """
  def run_task(conn, task_payload) do
    case validate_params(task_payload) do
      {:ok, validated_params} -> save_and_run_task(conn, validated_params)
      {:error, response} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(response)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_params(payload) do
    final_data = %{params: %{}, errors: [], payload: payload}
    |> add_if_exists("query", "should contain the task code you want to execute, for example: " <>
      "\"query\":\"report_property(\"hello\", \"world\")\"")
    # FIXME(#76): Once cloak's belong to a particular organisation, we should start attaching the
    # current users organisation to the cloak here, along with checking whether the cloak actually
    # exists for the particular user. Until then, all cloak's are hardcoded to `unknown_org`
    |> add_if_exists("cloak", "should contain the node-name of the cloak that will execute the task. " <>
      "Given a cloak with the node-name 'my-cloak', you would write: \"cloak\":\"my-cloak\"",
      fn(value) -> %{"cloak_id" => "unknown_org/#{value}"} end)
    |> add_if_exists("data_source", "should contain the name of the data source you want to use. " <>
      "A data source mostly corresponds to a database. Your 'data_source' parameter could for example " <>
      "look like: \"data_source\":\"my-db\"")
    |> add_if_exists("tables", "should consist of a list the tables contained within your data source " <>
      "that you want made available to your task. For example: \"tables\":[\"my-table\"]")

    case final_data.errors do
      [] -> {:ok, final_data.params}
      errors ->
        response = %{
          success: false,
          description: "Task is invalid. Please consult the API documentation.",
          errors: errors
        }
        {:error, response}
    end
  end

  defp add_if_exists(data, what, error_description, success_fn \\ nil) do
    case data.payload[what] do
      nil -> %{data | errors: ["The '#{what}' parameter #{error_description}" | data.errors]}
      value ->
        success_fn = case success_fn do
          nil -> fn(value) -> Map.put(%{}, what, value) end
          _ -> success_fn
        end
        %{data | params: Map.merge(data.params, success_fn.(value))}
    end
  end

  defp save_and_run_task(conn, params) do
    changeset = build_assoc(conn.assigns.current_user, :tasks)
    |> Task.changeset(params)

    case Repo.insert(changeset) do
      {:ok, task} ->
        execute_task(conn, task)
      {:error, changeset} ->
        # The task was validated, but didn't save. Let's crash and report
        Logger.error([
          "Error saving task: #{inspect(changeset)}\n",
          Exception.format_stacktrace(System.stacktrace())
        ])

        send_resp(conn, Plug.Conn.Status.code(:internal_server_error), "")
    end
  end

  defp execute_task(conn, task) do
    try do
      # We have to subscribe to task results before we schedule the task, otherwise
      # there is a race condition, where the task result is returned before we have
      # time to notice, and then end up waiting indefinitely for a result that has
      # already been sent.
      Air.Endpoint.subscribe(self(), "task:#{task.id}")
      case Air.Socket.Cloak.MainChannel.run_task(task.cloak_id, Task.to_cloak_query(task)) do
        :ok ->
          await_response(send_chunked(conn, 200), task)
        {:error, :not_connected} ->
          json(conn, %{success: false, reason: "the cloak is not connected"})
        {:error, reason} ->
          Logger.error("Task start error: #{reason}")
          json(conn, %{success: false, reason: reason})
      end
    catch type, error ->
      # We'll make a nice error log report and return 500
      Logger.error([
        "Error starting a task: #{inspect(type)}:#{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])

      send_resp(conn, Plug.Conn.Status.code(:internal_server_error), "")
    end
  end

  defp await_response(conn, task) do
    receive do
      %Phoenix.Socket.Broadcast{event: "result", payload: payload} ->
        {:ok, conn} = chunk(conn, Poison.encode!(Map.merge(%{success: true}, payload)))
        conn
      other ->
        Logger.warn("Received unexpected response while waiting for query to complete: #{inspect(other)}")
        keep_connection_alive(conn, task)
    after
      10_000 -> keep_connection_alive(conn, task)
    end
  end

  defp keep_connection_alive(conn, task) do
    {:ok, conn} = chunk(conn, " ")
    await_response(conn, task)
  end
end
