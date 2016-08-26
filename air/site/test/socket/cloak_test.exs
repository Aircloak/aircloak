defmodule Air.Socket.CloakTest do
  use ExUnit.Case, async: false

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Air.Socket.Cloak.MainChannel
  alias Air.{Organisation, TestSocketHelper, DataSourceManager}

  setup do
    DataSourceManager.start_link()
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
      channel_pid(),
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

  test "can't run a query on a cloak from another organisation" do
    socket = connect!()
    join_main_channel!(socket)

    async_query(
      channel_pid(),
      %Organisation{name: "another_organisation"},
      %{id: 42, code: ""}
    )

    assert {:error, :forbidden} == query_response!()
  end

  defp connect!(params \\ %{}) do
    default_params = %{
      cloak_name: "cloak_1",
      cloak_organisation: "some_organisation",
    }
    TestSocketHelper.connect!(Map.merge(default_params, params))
  end

  defp channel_pid do
    [pid | _] = DataSourceManager.channel_pids("data_source_id")
    pid
  end

  defp join_main_channel!(socket, data_sources \\ default_data_sources()) do
    {:ok, %{}} = join_main_channel(socket, data_sources)
  end

  defp join_main_channel(socket, data_sources \\ default_data_sources()) do
    TestSocketHelper.join!(socket, "main", %{data_sources: data_sources})
  end

  defp default_data_sources() do
    [%{"id" => "data_source_id", "name" => "data source name", "tables" => []}]
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
