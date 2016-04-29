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
          "\"query\":\"report_property(\"hello\", \"world\")\"", check_map: fn(query) -> {:ok, %{query: query}} end},
      %{name: "cloak", description: "should contain the node-name of the cloak that will execute the task. " <>
          "Given a cloak with the node-name 'my-cloak', you would write: \"cloak\":\"my-cloak\"",
          check_map: fn(cloak) ->
            # FIXME(#76): Once cloak's belong to a particular organisation, we should start attaching the
            # current users organisation to the cloak here, along with checking whether the cloak actually
            # exists for the particular user. Until then, all cloak's are hardcoded to `unknown_org`
            {:ok, %{cloak_id: "unknown_org/#{cloak}"}}
          end},
      %{name: "data_source", description: "should contain the name of the data source you want to use. " <>
          "A data source mostly corresponds to a database. Your 'data_source' parameter could for example " <>
          "look like: \"data_source\":\"my-db\"", check_map: fn(ds) -> {:ok, %{data_source: ds}} end},
      %{name: "tables", description: "should consist of a list the tables contained within your data source " <>
          "that you want made available to your task. For example: \"tables\":[\"my-table\"]", check_map: fn(tables) ->
            case is_list(tables) do
              true -> {:ok, %{tables: tables}}
              false -> {:error, "should be a list of tables declared as strings, for example \"tables\":" <>
                  "[\"my-table1\", \"my-table2\"]"}
            end
          end}
    ]
  end

  defp validate_params(conn, payload) do
    validation_fun = fn(%{name: name, description: description} = req,
        %{params: params, errors: existing_errors} = acc) ->
          add_error = fn(error) -> %{acc | errors: [%{parameter: name, problem: error} | existing_errors]} end
          add_value = fn(value) -> %{acc | params: Map.merge(params, value)} end
          case payload[name] do
            nil -> add_error.(description)
            value ->
              case req[:check_map].(value) do
                {:ok, updated_value} -> add_value.(updated_value)
                {:error, error} -> add_error.(error)
              end
          end
        end
    %{params: params, errors: errors} = List.foldl(param_validations(), %{params: %{}, errors: []}, validation_fun)
    case errors do
      [] -> {:ok, params}
      _ ->
        response = %{
          success: false,
          description: "Task is invalid. Please consult the API documentation.",
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
          await_response(send_chunked(conn, 200), task)
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
