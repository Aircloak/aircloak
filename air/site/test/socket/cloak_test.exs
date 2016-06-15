defmodule Air.Socket.CloakTest do
  use ExUnit.Case, async: true

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Air.Socket.Cloak.MainChannel
  alias Air.{CloakInfo, TestSocketHelper}


  test "invalid authentication" do
    assert {{:disconnected, {403, "Forbidden"}}, _} = TestSocketHelper.connect(%{})
  end

  test "unmatched topic" do
    socket = TestSocketHelper.connect!()
    assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    socket = TestSocketHelper.connect!()
    assert {:ok, %{}} == join_main_channel(socket)
  end

  test "starting a query" do
    socket = TestSocketHelper.connect!()
    assert {:ok, %{}} == join_main_channel(socket)

    me = self()
    spawn(fn ->
      start_query_result = MainChannel.run_query("unknown_org/cloak_1", %{id: 42, code: ""})
      send(me, {:start_query_result, start_query_result})
    end)
    assert {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, 100)
    assert %{"event" => "run_query", "payload" => %{"id" => 42}, "request_id" => request_id} = request

    TestSocket.push(socket, "main", "call_response", %{request_id: request_id, status: "ok"})
    assert_receive {:start_query_result, :ok}
  end

  test "receiving a query result" do
    query = Air.TestRepoHelper.create_organisation!()
    |> Air.TestRepoHelper.create_user!()
    |> Air.TestRepoHelper.create_query!()

    nil = Air.Repo.get!(Air.Query, query.id).result

    socket = TestSocketHelper.connect!()
    join_main_channel!(socket)

    request = %{
      request_id: "foobar", event: "query_result",
      payload: %{query_id: query.id, rows: [], columns: []}
    }

    Air.Endpoint.subscribe(self(), "user:#{query.user_id}")
    TestSocket.push(socket, "main", "cloak_call", request)

    assert {:ok, {"main", "call_response", response}} = TestSocket.await_message(socket, 100)
    assert %{"request_id" => "foobar", "status" => "ok"} = response

    query_id = query.id
    assert_receive %Phoenix.Socket.Broadcast{event: "result", payload: %{completed: true, id: ^query_id}}

    assert Air.Repo.get!(Air.Query, query.id).result != nil
  end

  test "getting data for connected cloaks" do
    socket1 = TestSocketHelper.connect!(%{cloak_name: "cloak_3"})
    join_main_channel!(socket1, "cloak_3")
    assert [%Air.CloakInfo{name: "cloak_3"} = cloak_3] = CloakInfo.all(Air.TestRepoHelper.admin_organisation())
    assert cloak_3 == CloakInfo.get(cloak_3.id)
    assert nil == CloakInfo.get("non-existing cloak")

    socket2 = TestSocketHelper.connect!(%{cloak_name: "cloak_4"})
    join_main_channel!(socket2, "cloak_4")

    # admin org fetches all cloaks
    all_cloaks = CloakInfo.all(Air.TestRepoHelper.admin_organisation())
    assert [%Air.CloakInfo{name: "cloak_3"}, %Air.CloakInfo{name: "cloak_4"} = cloak_4] = Enum.sort(all_cloaks)
    assert cloak_4 == CloakInfo.get(cloak_4.id)

    # owner org fetches all owned cloaks
    all_cloaks = CloakInfo.all(%Air.Organisation{name: "unknown_org"})
    assert [%Air.CloakInfo{name: "cloak_3"}, %Air.CloakInfo{name: "cloak_4"}] = Enum.sort(all_cloaks)

    # cloak data disappears when it leaves the main channel
    mref = Process.monitor(CloakInfo.main_channel_pid("unknown_org/cloak_3"))
    TestSocket.leave(socket1, "main")
    assert_receive {:DOWN, ^mref, _, _, _}
    assert [%Air.CloakInfo{name: "cloak_4"}] = CloakInfo.all(Air.TestRepoHelper.admin_organisation())
    assert nil == CloakInfo.get(cloak_3.id)
  end

  defp join_main_channel!(socket, cloak_name \\ "cloak_1", data_sources \\ []) do
    {:ok, %{}} = join_main_channel(socket, cloak_name, data_sources)
  end

  defp join_main_channel(socket, cloak_name \\ "cloak_1", data_sources \\ []) do
    TestSocketHelper.join!(socket, "main", %{name: cloak_name, data_sources: data_sources})
  end
end
