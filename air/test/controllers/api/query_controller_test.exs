defmodule Air.API.QueryController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Repo, Schemas.DataSource, Schemas.User}
  alias Poison, as: JSON

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  describe "running a query" do
    setup [:with_group, :with_user, :with_token, :with_socket]

    test "can run a query", context do
      query_data_params = %{
        query: %{statement: "Query code", data_source_name: context.data_source.name}
      }
      task = Task.async(fn -> api_conn(context.token) |> post("/api/queries", query_data_params) |> response(200) end)

      TestSocketHelper.respond_to_start_task_request!(context.socket, "ok")

      assert %{"success" => true} = JSON.decode!(Task.await(task))
    end
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

  defp with_group(_context), do: {:ok, group: create_group!()}

  defp with_user(%{group: group}) do
    user =
      create_user!()
      |> User.changeset(%{groups: [group.id]})
      |> Repo.update!()
    {:ok, user: user}
  end

  defp with_token(%{user: user}), do: %{token: create_token!(user)}

  defp with_socket(%{group: group}) do
    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main", %{data_sources: [
      %{"name" => "data_source", "global_id" => "data_source", "tables" => []}]})

    data_source =
      Repo.one(DataSource)
      |> Repo.preload([:groups])
      |> DataSource.changeset(%{groups: [group.id]})
      |> Repo.update!()

    {:ok, socket: socket, data_source: data_source}
  end
end
