defmodule Air.API.QueryController.Test do
  # Despite using Ecto 2.0 with it's transactional DB sandbox model,
  # we have to run these tests sequentially.
  # The problem causing the sequential execution is that the database pool
  # is used from a process distinct from the test one:
  #
  # Ecto provides two options:
  #
  # - explicitly allowing a third process to access a sandbox pool
  # - sharing the test pool with the world
  #
  # Explicitly allowing the DataSourceManager access doesn't work as
  # we would have to concurrently give it access to multiple test pools,
  # which then in turns means it wouldn't know which to check out a connection from.
  #
  # Using distinct servers per test doesn't work either, since we don't
  # control the calling site.
  #
  # The sharing option is the one we are using, but since any process can access
  # the pool, we cannot run tests concurrently.
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, DataSource}
  alias Poison, as: JSON

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "can run a query" do
    organisation = create_organisation!()
    token = create_token!(create_user!(organisation))

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1", cloak_organisation: organisation.name})
    TestSocketHelper.join!(socket, "main", %{data_sources: [%{"id" => "data_source", "tables" => []}]})

    query_data_params = %{
      query: %{query: "Query code", name: "Query name", data_source_id: Repo.one(DataSource).id}
    }
    task = Task.async(fn -> api_conn(token) |> post("/api/queries", query_data_params) |> response(200) end)

    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    assert %{"success" => true} = JSON.decode!(Task.await(task))
  end

  test "show a query of the token's user" do
    user = create_user!()
    token = create_token!(user)
    query = create_query!(user, %{statement: "text of the query"})

    result = api_conn(token) |> get("/api/queries/#{query.id}") |> response(200)

    assert %{"query" => %{"statement" => "text of the query"}} = JSON.decode!(result)
  end

  test "show a query of another user fails" do
    user = create_user!()
    token = create_token!(user)
    query = create_query!(_another_user = create_user!(), %{statement: "text of the query"})

    result = api_conn(token) |> get("/api/queries/#{query.id}") |> response(404)

    assert %{"error" => _} = JSON.decode!(result)
  end
end
