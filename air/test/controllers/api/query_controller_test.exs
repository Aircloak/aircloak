defmodule Air.API.QueryController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, DataSource, User}
  alias Poison, as: JSON

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "can run a query" do
    group = create_group!()
    user = create_user!()
    |> User.changeset(%{groups: [group.id]})
    |> Repo.update!()
    token = create_token!(user)

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main", %{data_sources: [%{"global_id" => "data_source", "tables" => []}]})

    data_source = Repo.one(DataSource)
    |> Repo.preload([:groups])
    |> DataSource.changeset(%{groups: [group.id]})
    |> Repo.update!()

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: data_source.id}
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
