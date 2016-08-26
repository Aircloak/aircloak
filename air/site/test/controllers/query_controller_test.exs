defmodule Air.QueryControllerTest do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, DataSource}

  test "can run a query" do
    organisation = create_organisation!()
    user = create_user!(organisation)

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1", cloak_organisation: organisation.name})
    TestSocketHelper.join!(socket, "main",
      %{data_sources: [%{"id" => "unique_name", "tables" => []}]})

    data_source = Repo.one(DataSource)
    query_data_params = %{
      query: %{query: "Query code", name: "Query name", data_source_id: data_source.id}
    }
    task = Task.async(fn -> login(user) |> post("/queries", query_data_params) |> response(200) end)

    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    assert %{"success" => true} = Poison.decode!(Task.await(task))
  end

  test "failed queries" do
    user = create_user!(create_organisation!())
    params = %{
      unique_id: "data_source_id",
      name: "data source name",
      tables: "[]",
    }
    data_source = %DataSource{}
    |> DataSource.changeset(params)
    |> Repo.insert!()

    insert_query(user, data_source, "query 1", %{error: "some error"})
    insert_query(user, data_source, "query 2", %{error: "some error"})
    insert_query(user, data_source, "query 3", %{})

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
