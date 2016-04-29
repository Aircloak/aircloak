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
  The requester will receive an error should the task not finish executing within 10 minutes.
  This does not mean that the task doesn't infact still run.

  The following parameters are required:

  - query: the lua task to be executed
  - cloak_id: the ID of the cloak on which the task should execute.
      No checks are performed to ensure that the cloak belongs to the user's organisation.
      The cloak_id takes the format `<organization-name>/<node-name>`
  - data_source: the name of the data source in the selected cloak
  - tables: a list of tables that should be made available to the task

  You can test this endpoint in your local development environment using the following curl command:
  curl --data '{"query":"report_property(\"Hello\", \"world\")", "cloak_id":"unknown_org/nonode@nohost", \
      "data_source":"local", "tables":["test"]}' \
      -H "auth-token: <valid auth token>" \
      -H "content-type: application/json" -k -XPOST \
      https://insights.air-local:20000/api/task
  """
  def run_task(conn, task_payload) do
    case validate_params(conn, task_payload) do
      {:ok, validated_params} -> save_and_run_task(conn, validated_params)
      {:error, conn} -> conn
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp param_validations() do
    [
      %{name: "query", description: "should contain the task code you want to execute, for example: " <>
          "\"query\":\"report_property(\"hello\", \"world\")\""},
      %{name: "cloak_id", description: "should contain the id of the cloak you have installed. " <>
          "The id usually takes the form '<organization-name>/<node-name>', so you could for " <>
          "example include the following statement: \"cloak_id\":\"my-org/hostname\""},
      %{name: "data_source", description: "should contain the name of the data source you want to use. " <>
          "A data source mostly corresponds to a database. Your 'data_source' parameter could for example " <>
          "look like: \"data_source\":\"my-db\""},
      %{name: "tables", description: "should consist of a list the tables contained within your data source " <>
          "that you want made available to your task. For example: \"tables\":[\"my-table\"]", validator: fn(tables) ->
            case is_list(tables) do
              true -> :ok
              false -> {:error, "should be a list of tables declared as strings, for example \"tables\":" <>
                  "[\"my-table1\", \"my-table2\"]"}
            end
          end}
    ]
  end

  defp validate_params(conn, payload) do
    validation_fun = fn(%{name: name, description: description} = req,
        %{params: params, errors: existing_errors} = acc) ->
          add_error = fn(error) -> %{acc| errors: [%{parameter: name, problem: error}|existing_errors]} end
          add_value = fn(value) -> %{acc| params: Map.put(params, name, value)} end
          case payload[name] do
            nil -> add_error.(description)
            value ->
              case req[:validator] do
                nil -> add_value.(value)
                validator -> case validator.(value) do
                  :ok -> add_value.(value)
                  {:error, error} -> add_error.(error)
                end
              end
          end
        end
    %{params: params, errors: errors} = List.foldl(param_validations(), %{params: %{}, errors: []}, validation_fun)
    case errors do
      [] -> {:ok, params}
      _ ->
        response = %{
          success: false,
          description: "Your request needs some attention - your task was not executed. Please consult the API " <>
              "documentation for further information on how to use this API endpoint. " <>
              "The specific errors are described in more defail in the 'errors' field",
          errors: Enum.map(errors, fn(error) -> "The '#{error.parameter}' parameter #{error.problem}" end)
        }

        {:error, conn
            |> put_status(:unprocessable_entity)
            |> json(response)}
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
          await_response(send_chunked(conn, 200), task, 0)
        {:error, :not_connected} ->
          json(conn, %{success: false, reason: "the cloak is not connected"})
        {:error, other} ->
          Logger.error("Task start error: #{other}")
          json(conn, %{success: false, reason: "an error has occurred"})
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

  @ten_minutes_in_ms 600000

  defp await_response(conn, _task, time_waited) when time_waited > @ten_minutes_in_ms do
    response = %{
      success: false,
      description: "The task execution timed out. In the future you will also be able to run " <>
          "tasks asynchronously using this API end-point. Until then, please run your tasks via " <>
          "the web interface at https://insights.aircloak.com"
    }
    {:ok, conn} = chunk(conn, Poison.encode!(response))
    conn
  end
  defp await_response(conn, task, time_waited) do
    receive do
      %Phoenix.Socket.Broadcast{event: "result", payload: payload} ->
        {:ok, conn} = chunk(conn, Poison.encode!(Map.merge(%{success: true}, payload)))
        conn
      other ->
        Logger.warn("Received unexpected response while waiting for query to complete: #{inspect(other)}")
        keep_connection_alive(conn, task, time_waited)
    after
      10_000 -> keep_connection_alive(conn, task, time_waited + 10_000)
    end
  end

  defp keep_connection_alive(conn, task, time_waited) do
    {:ok, conn} = chunk(conn, " ")
    await_response(conn, task, time_waited)
  end
end
