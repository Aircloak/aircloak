defmodule Air.QueryControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo}
  alias Phoenix.Channels.GenSocketClient.TestSocket

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    group = create_group!()

    user = create_user!(%{groups: [group.id]})

    params = %{
      "global_id" => "data_source_global_id",
      "name" => "data source name",
      "tables" => "[]",
      "groups" => [group.id],
    }
    data_source = Air.Service.DataSource.create!(params)
    {:ok, data_source: data_source, user: user}
  end

  test "can run a query", context do
    socket = open_cloak_mock_socket(context.data_source)

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id}
    }
    task = Task.async(fn -> login(context[:user]) |> post("/queries", query_data_params) |> response(200) end)

    TestSocketHelper.respond_to_start_task_request!(socket, :ok)

    assert %{"success" => true} = Poison.decode!(Task.await(task))
  end

  test "can cancel a query", context do
    socket = open_cloak_mock_socket(context.data_source)
    query = create_query!(context.user, %{data_source_id: context.data_source.id})

    Task.start_link(fn -> login(context[:user]) |> post("/queries/#{query.id}/cancel") |> response(200) end)

    query_id = query.id
    assert {:ok, {"main", "air_call", %{event: "stop_query", payload: ^query_id}}} =
      TestSocket.await_message(socket)
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

  test "returns more rows", context do
    query = create_query!(
      context.user,
      %{statement: "text of the query", query_state: :started, data_source_id: context.data_source.id}
    )

    send_query_result(
      query.id,
      %{columns: ["col"]},
      [%{occurrences: 60, row: [1]}, %{occurrences: 50, row: [2]}]
    )

    assert buckets(context, query.id, 0) ==
      [%{"occurrences" => 60, "row" => [1]}, %{"occurrences" => 40, "row" => [2]}]
    assert buckets(context, query.id, 100) == [%{"occurrences" => 10, "row" => [2]}]
    assert buckets(context, query.id, 110) == []
  end

  defp open_cloak_mock_socket(data_source) do
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main",
      %{data_sources: [%{name: data_source.name, global_id: data_source.global_id, tables: []}]})
    socket
  end

  defp buckets(context, query_id, from), do:
    login(context.user)
    |> get(query_path(context.conn, :buckets, query_id, from: from))
    |> response(200)
    |> Poison.decode!()
end
