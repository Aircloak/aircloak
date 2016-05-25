defmodule Air.QueriesControllerTest do
  use Air.ConnCase

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.TestSocketHelper

  @query_data_params %{query: %{query: "Query code", name: "Query name"}}

  test "can run a query" do
    user = create_user!()

    # Open the cloak mock socket
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})
    TestSocketHelper.join!(socket, "main", %{name: "cloak_1", data_sources: []})

    # Run the task in parallel since it's blocking on waiting a response from the socket
    me = self()
    spawn_link(fn ->
      run_params = put_in(@query_data_params, [:query, :data_source_token],
          Phoenix.Token.sign(Air.Endpoint, "data_source_token",{"unknown_org/cloak_1", nil}))
      response_json = login(user) |> post("/queries", run_params) |> response(200)
      send(me, {:response_json, response_json})
    end)

    # Cloak responds to the request from the POST controller
    TestSocketHelper.respond_to_start_task_request!(socket, "ok")

    # Verify the cloak response
    assert_receive {:response_json, response_json}
    %{"success" => status} = Poison.decode!(response_json)
    assert status
  end
end
