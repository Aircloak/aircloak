defmodule AirWeb.QueryController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

  import Aircloak.AssertionHelper
  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo}
  alias Phoenix.Channels.GenSocketClient.TestSocket

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})

    group = create_group!()

    user = create_user!(%{groups: [group.id]})

    tables = [
      %{
        id: "table",
        columns: [
          %{user_id: true, type: :string, name: "uid"},
          %{user_id: false, type: :integer, name: "age"}
        ]
      }
    ]

    params = %{
      "name" => "data source name",
      "tables" => Jason.encode!(tables),
      "groups" => [group.id]
    }

    data_source = Air.Service.DataSource.create!(params)
    on_exit(&Air.Service.DataSource.QueryScheduler.sync/0)
    {:ok, data_source: data_source, user: user}
  end

  test "can run a query without providing query id", context do
    socket = open_cloak_mock_socket(context.data_source)

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id}
    }

    task =
      Task.async(fn ->
        login(context[:user]) |> post("/queries", query_data_params) |> response(200)
      end)

    TestSocketHelper.respond_to_start_task_request!(socket, :ok)
    Air.Service.DataSource.QueryScheduler.sync()

    assert %{"success" => true} = Jason.decode!(Task.await(task))
  end

  test "can run a query providing the desired query id", context do
    socket = open_cloak_mock_socket(context.data_source)

    query_id = "d44aecdb-b87b-43bd-a35b-3fd4a5be0bcd"

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id, id: query_id}
    }

    task =
      Task.async(fn ->
        login(context[:user]) |> post("/queries", query_data_params) |> response(200)
      end)

    TestSocketHelper.respond_to_start_task_request!(socket, :ok)
    Air.Service.DataSource.QueryScheduler.sync()

    assert %{"success" => true, "query_id" => ^query_id} = Jason.decode!(Task.await(task))
  end

  test "starting a query with an already used id fails", context do
    socket = open_cloak_mock_socket(context.data_source)

    query_id = "d44aecdb-b87b-43bd-a35b-3fd4a5be0bcd"

    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id, id: query_id}
    }

    task =
      Task.async(fn ->
        login(context[:user]) |> post("/queries", query_data_params) |> response(200)
      end)

    TestSocketHelper.respond_to_start_task_request!(socket, :ok)

    assert %{"success" => true, "query_id" => ^query_id} = Jason.decode!(Task.await(task))

    rerun_task =
      Task.async(fn ->
        login(context[:user]) |> post("/queries", query_data_params) |> response(200)
      end)

    assert %{"success" => false, "reason" => "unable_to_create_query"} = Jason.decode!(Task.await(rerun_task))
  end

  test "can cancel a query", context do
    socket = open_cloak_mock_socket(context.data_source)
    query = create_query!(context.user, %{data_source_id: context.data_source.id})

    Task.start_link(fn ->
      login(context[:user]) |> post("/queries/#{query.id}/cancel") |> response(200)
    end)

    query_id = query.id

    assert {:ok, {"main", "air_call", %{event: "stop_query", payload: ^query_id}}} = TestSocket.await_message(socket)
    assert_soon is_nil(Air.Service.Query.Lifecycle.whereis(query_id))
  end

  describe "deleting a finished query" do
    test "can delete a finished query", context do
      query = create_query!(context.user, %{data_source_id: context.data_source.id})

      send_query_result(
        query.id,
        %{columns: ["col"]},
        Enum.map(1..10, &%{occurrences: 1, row: [&1]})
      )

      assert login(context.user) |> delete("/queries/#{query.id}") |> response(200)
      assert login(context.user) |> get("/queries/#{query.id}") |> response(404)
    end

    test "cannot delete an in-progress query", context do
      query = create_query!(context.user, %{data_source_id: context.data_source.id})
      assert login(context.user) |> delete("/queries/#{query.id}") |> response(409)
    end
  end

  describe "query notes" do
    test "updating query note", context do
      query = create_query!(context.user, %{data_source_id: context.data_source.id})
      assert login(context.user) |> patch("/queries/#{query.id}/note", note: "my query note") |> response(200)

      assert {:ok, updated_query} = get_query(query.id)
      assert updated_query.note == "my query note"
    end

    test "returns not found for invalid query", context do
      assert login(context.user) |> patch("/queries/invalid-query-id/note", note: "my query note") |> response(404)
    end

    test "cannot update note of another user", context do
      query = create_query!(context.user, %{data_source_id: context.data_source.id})

      other_user = create_user!()
      assert login(other_user) |> patch("/queries/#{query.id}/note", note: "my query note") |> response(404)

      assert {:ok, %{note: nil}} = get_query(query.id)
    end
  end

  test "returns unauthorized when not authorized to query data source", context do
    query_data_params = %{
      query: %{statement: "Query code", data_source_id: context[:data_source].id}
    }

    assert login(create_user!()) |> post("/queries", query_data_params) |> response(401)
  end

  test "fetching desired chunk", context do
    query =
      create_query!(context.user, %{
        statement: "text of the query",
        query_state: :started,
        data_source_id: context.data_source.id
      })

    send_query_result(
      query.id,
      %{columns: ["col"]},
      Enum.map(1..1100, &%{occurrences: 1, row: [&1]})
    )

    assert chunk_values(context, query.id, 0) == Enum.to_list(1..1000)
    assert chunk_values(context, query.id, 1) == Enum.to_list(1001..1100)
    assert chunk_values(context, query.id, 2) == []
  end

  test "fetching all chunks", context do
    query =
      create_query!(context.user, %{
        statement: "text of the query",
        query_state: :started,
        data_source_id: context.data_source.id
      })

    send_query_result(
      query.id,
      %{columns: ["col"]},
      Enum.map(1..1100, &%{occurrences: 1, row: [&1]})
    )

    assert chunk_values(context, query.id, "all") == Enum.to_list(1..1100)
  end

  describe "debug export" do
    test "contains query itself", context do
      statement = "SELECT foo FROM bar"
      assert get_query_export(context, %{statement: statement}) =~ statement
    end

    test("contains the query results", context, do: assert(get_query_export(context) =~ " | 1 "))

    test "contains the available views", context do
      view = create_view!(context.user, context.data_source)
      assert get_query_export(context) =~ view.sql
    end

    test "contains the available tables", context do
      query_export = get_query_export(context)
      assert query_export =~ ~r/uid.+string.+user id column/
      assert query_export =~ ~r/age.+integer/
    end
  end

  describe ".query" do
    test "it renders the query for a public token" do
      query = create_query!(create_user!())
      token = Air.Service.Token.public_query_token(query)

      conn = build_conn()
      assert conn |> get(public_permalink_path(conn, :permalink_show, token)) |> response(200) =~ query.statement
    end

    test "it renders the query for a private token" do
      query = create_query!(create_user!())

      group = create_group!(%{data_sources: [query.data_source_id]})
      user = create_user!(%{groups: [group.id]})
      token = Air.Service.Token.private_query_token(query)

      assert login(user) |> get(private_permalink_path(build_conn(), :permalink_show, token)) |> response(200) =~
               query.statement
    end

    test "it renders not found for invalid public tokens" do
      assert build_conn() |> get(public_permalink_path(build_conn(), :permalink_show, "invalid")) |> response(404)
    end

    test "it renders not found for invalid private tokens" do
      assert login(create_user!())
             |> get(private_permalink_path(build_conn(), :permalink_show, "invalid"))
             |> response(404)
    end

    test "the result does not include permalinks if query is accessed via a public permalink" do
      query = create_query!(create_user!())
      token = Air.Service.Token.public_query_token(query)
      result = build_conn() |> get(public_permalink_path(build_conn(), :permalink_show, token)) |> response(200)

      refute result =~ ~r[/permalink/public/query/.+/query]
      refute result =~ ~r[/permalink/private/query/.+/query]
    end

    test "the result does not include permalinks if query is accessed via a private permalink" do
      query = create_query!(create_user!())
      group = create_group!(%{data_sources: [query.data_source_id]})
      user = create_user!(%{groups: [group.id]})
      token = Air.Service.Token.private_query_token(query)
      result = login(user) |> get(private_permalink_path(build_conn(), :permalink_show, token)) |> response(200)

      refute result =~ ~r[/permalink/public/query/.+/query]
      refute result =~ ~r[/permalink/private/query/.+/query]
    end
  end

  defp get_query_export(context, params \\ %{}) do
    default_params = %{
      statement: "SELECT count(*) FROM table",
      query_state: :started,
      data_source_id: context.data_source.id
    }

    query =
      create_query!(
        context.user,
        Map.merge(default_params, params)
      )

    send_query_result(
      query.id,
      %{columns: ["col"]},
      Enum.map(1..10, &%{occurrences: 1, row: [&1]})
    )

    login(context.user) |> get("/queries/#{query.id}/debug_export") |> response(200)
  end

  defp open_cloak_mock_socket(data_source) do
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})

    TestSocketHelper.join!(socket, "main", %{
      data_sources: [%{name: data_source.name, tables: []}]
    })

    socket
  end

  defp chunk_values(context, query_id, chunk),
    do:
      login(context.user)
      |> get(query_path(context.conn, :buckets, query_id, chunk: chunk))
      |> response(200)
      |> Jason.decode!()
      |> Enum.map(&(&1 |> Map.fetch!("row") |> hd()))
end
