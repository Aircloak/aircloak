defmodule Air.QueryControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, DataSource}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    params = %{
      "global_id" => "data_source_global_id",
      "name" => "data source name",
      "tables" => "[]",
    }
    data_source = %DataSource{}
    |> DataSource.changeset(params)
    |> Repo.insert!()
    {:ok, data_source: data_source}
  end

  test "can run a query", context do
    organisation = create_organisation!()
    user = create_user!(organisation)

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1", cloak_organisation: organisation.name})
    TestSocketHelper.join!(socket, "main",
      %{data_sources: [%{"global_id" => "data_source_global_id", "tables" => []}]})

    query_data_params = %{
      query: %{query: "Query code", name: "Query name", data_source_id: context[:data_source].id}
    }
    task = Task.async(fn -> login(user) |> post("/queries", query_data_params) |> response(200) end)

    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    assert %{"success" => true} = Poison.decode!(Task.await(task))
  end

  test "returns error when data source unavailable", context do
    organisation = create_organisation!()
    user = create_user!(organisation)

    query_data_params = %{
      query: %{query: "Query code", name: "Query name", data_source_id: context[:data_source].id}
    }
    login(user) |> post("/queries", query_data_params) |> response(503)
  end

  test "failed queries", context do
    user = create_user!(create_organisation!())

    insert_query(user, context[:data_source], "query 1", %{error: "some error"})
    insert_query(user, context[:data_source], "query 2", %{error: "some error"})
    insert_query(user, context[:data_source], "query 3", %{})

    admin = create_user!(admin_organisation())
    response = login(admin) |> get("/queries/failed") |> response(200)

    assert response =~ "query 1"
    assert response =~ "query 2"
    refute response =~ "query 3"
  end

  test "user can't fetch failed queries" do
    assert "/" ==
      create_organisation!()
      |> create_user!()
      |> login()
      |> get("/queries/failed")
      |> redirected_to()
  end

  defp insert_query(user, data_source, statement, result) do
    create_query!(user, %{
      statement: statement,
      data_source_id: data_source.id,
      result: Poison.encode!(result)
    })
  end
end
