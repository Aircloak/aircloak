defmodule Air.API.QueryController.Test do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, DataSource}
  alias Poison, as: JSON

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
