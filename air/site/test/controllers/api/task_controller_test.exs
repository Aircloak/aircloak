defmodule Air.API.TaskControllerTest do
  use Air.ConnCase

  import Air.{TestAuthHelper, TestRepoHelper}
  alias Air.{Task, TestSocketHelper}

  test "requires parameters", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    token = create_token(conn, create_user!())

    %{"success" => success, "errors" => errors} = conn
        |> put_req_header("auth-token", token)
        |> post("/api/task")
        |> response(422)
        |> Poison.decode!

    assert errors
        |> Enum.map(fn(str) -> Regex.run(~r"The '(\w+)' parameter", str, capture: :all_but_first) end)
        |> Enum.map(&hd(&1))
        |> Enum.sort == ["cloak_id", "data_source", "query", "tables"]

    refute success
  end

  test "executes task", %{conn: conn} do
    conn = add_auth_to_conn(conn)
    user = create_user!()
    token = create_token(conn, user)

    # Open the cloak mock socket so we can schedule a task execution against it
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main", %{"data_sources" => [
        %{"id" => "local", "tables" => [%{"columns" => [
          %{"name" => "item", "type" => "text"},
          %{"name" => "price", "type" => "number"}
        ], "id" => "test"}]}], "name" => "nonode@nohost"})

    query_params = %{
      query: "report_property(\"Hello\", \"world\")",
      cloak_id: "unknown_org/nonode@nohost",
      data_source: "local",
      tables: ["test"]
    }

    # Since the call scheduling the task is blocking, we need to perform it
    # in a secondary thread.
    me = self()
    spawn_link(fn ->
          response = conn
              |> put_req_header("auth-token", token)
              |> put_req_header("content-type", "application/json")
              |> post("/api/task", query_params)
              |> response(200)
              |> Poison.decode!
          send(me, {:response, response})
        end)

    # We block waiting for the task to have been created by the request,
    # otherwise we don't know which ID we should send our fake results to
    task_id = wait_for_task_id(query_params.query)

    # Simulate that the cloak has accepted the task
    TestSocketHelper.respond_to_start_task_request!(socket, task_id, "ok")

    # Simulate that the cloak broadcasts the response.
    Air.Endpoint.broadcast_from!(self(), "task:#{task_id}", "result", %{bucket: "value"})

    # Verify the cloak response
    assert_receive {:response, %{"success" => status, "bucket" => value}}
    assert status
    assert value == "value"
  end

  defp wait_for_task_id(query) do
    case Repo.one(from task in Task, where: task.query == ^query, select: task) do
      nil ->
        :timer.sleep(100)
        wait_for_task_id(query)
      task ->
        task.id
    end
  end
end
