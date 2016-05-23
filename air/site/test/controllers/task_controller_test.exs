defmodule Air.TaskControllerTest do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.TestSocketHelper

  alias Air.Task
  @valid_attrs %{name: "name", query: "query content", permanent: true}
  @invalid_attrs %{name: ""}
  @query_data_params %{task: %{query: "Query code", name: "Query name"}}

  defp create_task(user, params \\ @valid_attrs) do
    changeset = build_assoc(user, :tasks)
    changeset = Task.changeset(changeset, params)
    Repo.insert!(changeset)
  end

  test "tasks pages require an authenticated user" do
    user = create_user!()
    task = create_task(user)

    assert "/auth" == conn() |> get("/tasks") |> redirected_to()
    assert "/auth" == conn() |> get("/tasks/new") |> redirected_to()
    assert "/auth" == conn() |> get("/tasks/#{task.id}/edit") |> redirected_to()
    conn = conn()
    assert "/auth" == delete(conn, task_path(conn, :delete, task)) |> redirected_to()
  end

  test "index only shows tasks owned by the user" do
    user = create_user!()
    user_task = create_task(user)
    other_user = create_user!()
    other_user_task = create_task(other_user)

    index_html = login(user) |> get("/tasks") |> response(200)
    assert index_html =~ user_task.id
    refute index_html =~ other_user_task.id
  end

  test "editing and deleting tasks of other users is forbidden" do
    user = create_user!()
    other_user = create_user!()
    other_user_task = create_task(other_user)

    not_found_html = login(user) |> get("/tasks/#{other_user_task.id}/edit") |> response(404)
    assert not_found_html =~ "not found"

    not_found_html = login(user) |> post("/tasks/#{other_user_task.id}/edit", task: @valid_attrs) |> response(404)
    assert not_found_html =~ "not found"

    conn = conn()
    not_found_html = login(user) |> delete(task_path(conn, :delete, other_user_task)) |> response(404)
    assert not_found_html =~ "not found"
  end

  test "renders form for editing chosen resource", %{conn: conn} do
    user = create_user!()
    task = create_task(user)

    html_response = login(user) |> get(task_path(conn, :edit, task)) |> response(200)
    assert html_response =~ "editor"
  end

  test "updates chosen resource and returns a JSON response", %{conn: conn} do
    user = create_user!()
    task = create_task(user)
    response_json = login(user) |> put(task_path(conn, :update, task), task: @valid_attrs) |> response(200)
    %{"success" => success} = Poison.decode!(response_json)
    assert success
  end

  test "deletes chosen resource", %{conn: conn} do
    user = create_user!()
    task = create_task(user)

    assert "/tasks" == login(user) |> delete(task_path(conn, :delete, task)) |> redirected_to()
    refute Repo.get(Task, task.id)
  end

  test "running task of other user returns a 404" do
    user = create_user!()
    other_user = create_user!()
    other_user_task = create_task(other_user)

    response_html = login(user) |> post("/tasks/#{other_user_task.id}/run", @query_data_params) |> response(404)
    assert response_html =~ "Page not found"
  end

  test "can run a new task" do
    user = create_user!()

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main", %{name: "cloak_1", data_sources: []})

    # Run the task in parallel since it's blocking on waiting a response from the socket
    me = self()
    spawn_link(fn ->
      run_params = put_in(@query_data_params, [:task, :data_source_token],
          Phoenix.Token.sign(Air.Endpoint, "data_source_token",{"unknown_org/cloak_1", nil}))
      response_json = login(user) |> post("/tasks/run", run_params) |> response(200)
      send(me, {:response_json, response_json})
    end)

    # Cloak responds to the request from the POST controller
    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    # Verify the cloak response
    assert_receive {:response_json, response_json}
    %{"success" => status} = Poison.decode!(response_json)
    assert status
  end
end
