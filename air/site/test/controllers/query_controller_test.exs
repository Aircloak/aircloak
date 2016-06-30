defmodule Air.QueryControllerTest do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Token}

  @query_data_params %{query: %{query: "Query code", name: "Query name"}}

  test "can run a query" do
    organisation = create_organisation!()
    user = create_user!(organisation)

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1", cloak_organisation: organisation.name})
    TestSocketHelper.join!(socket, "main", %{data_sources: []})

    task = Task.async(fn ->
      data_source_token = Token.data_source_token("#{organisation.name}/cloak_1", nil)
      run_params = put_in(@query_data_params, [:query, :data_source_token], data_source_token)

      login(user) |> post("/queries", run_params) |> response(200)
    end)

    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    assert %{"success" => true} = Poison.decode!(Task.await(task))
  end
end
