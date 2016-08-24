defmodule Air.Socket.CloakTest do
  use ExUnit.Case, async: true

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Air.Socket.Cloak.MainChannel
  alias Air.{Organisation, TestSocketHelper, Cloak, Repo}

  import Air.TestRepoHelper
  import Ecto.Query, only: [from: 2]

  setup do
    Repo.delete_all(Cloak)
    :ok
  end

  test "cloak name must be provided" do
    params = %{cloak_name: "", cloak_organisation: "some_organisation"}
    assert {{:disconnected, {403, "Forbidden"}}, _} = TestSocketHelper.connect(params)
  end

  test "cloak org must be provided" do
    params = %{cloak_name: "some_name", cloak_organisation: ""}
    assert {{:disconnected, {403, "Forbidden"}}, _} = TestSocketHelper.connect(params)
  end

  test "unmatched topic" do
    socket = connect!()
    assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    socket = connect!()
    assert {:ok, %{}} == join_main_channel(socket)
  end

  test "starting a query" do
    socket = connect!()
    join_main_channel!(socket)

    async_query(
      cloak_pid(),
      %Organisation{name: "some_organisation"},
      %{id: 42, code: ""}
    )

    request = next_cloak_request(socket)
    assert %{"event" => "run_query", "payload" => %{"id" => 42}} = request

    respond_to_cloak(socket, request, %{status: "ok"})
    assert :ok == query_response!()
  end

  test "receiving a query result" do
    query = Air.TestRepoHelper.create_organisation!()
    |> Air.TestRepoHelper.create_user!()
    |> Air.TestRepoHelper.create_query!()

    nil = Air.Repo.get!(Air.Query, query.id).result

    socket = connect!()
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
    socket1 = connect!(%{cloak_name: "cloak_3"})
    join_main_channel!(socket1)
    assert [cloak(cloak_name: "cloak_3").id] == Enum.map(Repo.all(Cloak), &(&1.id))

    socket2 = connect!(%{cloak_name: "cloak_4"})
    join_main_channel!(socket2)

    # admin org fetches all cloaks
    assert [cloak(cloak_name: "cloak_3"), cloak(cloak_name: "cloak_4")] == Enum.sort(Repo.all(Cloak))

    # cloak data disappears when it leaves the main channel
    mref = Process.monitor(Cloak.channel_pid(cloak(cloak_name: "cloak_3")))

    TestSocket.leave(socket1, "main")
    assert_receive {:DOWN, ^mref, _, _, _}
    cloak3 = cloak(cloak_name: "cloak_3")
    refute Cloak.online?(cloak3)
  end

  test "can't run a query on a cloak from another organisation" do
    socket = connect!()
    join_main_channel!(socket)

    async_query(
      cloak_pid(),
      %Organisation{name: "another_organisation"},
      %{id: 42, code: ""}
    )

    assert {:error, :forbidden} == query_response!()
  end

  test "admins can run a query on a cloak from another organisation" do
    socket = connect!()
    join_main_channel!(socket)

    async_query(
      cloak_pid(),
      admin_organisation(),
      %{id: 42, code: ""}
    )

    request = next_cloak_request(socket)
    respond_to_cloak(socket, request, %{status: "ok"})

    assert :ok == query_response!()
  end

  defp connect!(params \\ %{}) do
    default_params = %{
      cloak_name: "cloak_1",
      cloak_organisation: "some_organisation",
    }
    TestSocketHelper.connect!(Map.merge(default_params, params))
  end

  defp cloak_pid do
    c = cloak()
    assert Cloak.online?(c)
    Cloak.channel_pid(c)
  end

  defp cloak(params \\ []) do
    org_name = Keyword.get(params, :organisation) || "some_organisation"
    cloak_name = Keyword.get(params, :cloak_name) || "cloak_1"
    query = from c in Cloak,
      where: c.name == ^"#{org_name}/#{cloak_name}",
      select: c
    Repo.one!(query)
  end

  defp join_main_channel!(socket, data_sources \\ []) do
    {:ok, %{}} = join_main_channel(socket, data_sources)
  end

  defp join_main_channel(socket, data_sources \\ []) do
    TestSocketHelper.join!(socket, "main", %{data_sources: data_sources})
  end

  defp async_query(cloak_id, user_organisation, payload) do
    caller = self()
    spawn(fn ->
      start_query_result = MainChannel.run_query(cloak_id, user_organisation, payload)
      send(caller, {:query_response, start_query_result})
    end)
  end

  defp query_response!(timeout \\ 100) do
    receive do
      {:query_response, response} -> response
    after timeout ->
      raise("timeout")
    end
  end

  defp next_cloak_request(socket, timeout \\ 100) do
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    request
  end

  defp respond_to_cloak(socket, %{"request_id" => request_id}, response) do
    TestSocket.push(socket, "main", "call_response", Map.put(response, :request_id, request_id))
  end
end
