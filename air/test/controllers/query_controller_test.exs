defmodule Air.QueryControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, DataSource, User}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    group = create_group!()

    user = create_user!()
    |> User.changeset(%{groups: [group.id]})
    |> Repo.update!()

    params = %{
      "global_id" => "data_source_global_id",
      "name" => "data source name",
      "tables" => "[]",
      "groups" => [group.id],
    }
    data_source = %DataSource{}
    |> DataSource.changeset(params)
    |> Repo.insert!()

    {:ok, data_source: data_source, user: user}
  end

  test "can run a query", context do
    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main",
      %{data_sources: [%{"global_id" => "data_source_global_id", "tables" => []}]})

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id}
    }
    task = Task.async(fn -> login(context[:user]) |> post("/queries", query_data_params) |> response(200) end)

    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    assert %{"success" => true} = Poison.decode!(Task.await(task))
  end

  test "returns unauthorized when not authorized to query data source", context do
    user = create_user!()

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id}
    }
    assert login(user) |> post("/queries", query_data_params) |> response(401)
  end

  test "returns error when data source unavailable", context do
    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id}
    }
    login(context[:user]) |> post("/queries", query_data_params) |> response(503)
  end
end
