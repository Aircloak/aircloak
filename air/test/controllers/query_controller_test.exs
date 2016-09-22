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
    user = create_user!()

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
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
    user = create_user!()

    query_data_params = %{
      query: %{query: "Query code", name: "Query name", data_source_id: context[:data_source].id}
    }
    login(user) |> post("/queries", query_data_params) |> response(503)
  end
end
