defmodule Air.Service.DataSource.QueryScheduler.StarterTest do
  use Air.SchemaCase, async: false

  alias Air.{TestRepoHelper, TestSocketHelper}
  alias Air.Service.{DataSource.QueryScheduler.Starter, Query}
  import Aircloak.AssertionHelper

  test "query starting" do
    user = create_user!()
    data_source = create_data_source!(user)
    queries = Enum.map(1..10, fn _ -> create_query!(user, data_source) end)
    cloak_name = start_cloak(data_source, List.duplicate(:ok, length(queries)))
    Starter.run()

    assert Query.awaiting_start() == []
    assert query_cloaks(queries) == [cloak_name]
  end

  test "fair query distribution" do
    user = create_user!()
    data_source = create_data_source!(user)
    queries = Enum.map(1..10, fn _ -> create_query!(user, data_source) end)
    cloak_names = Enum.map(queries, fn _ -> start_cloak(data_source, [:ok]) end)
    Starter.run()

    assert Query.awaiting_start() == []
    assert Enum.sort(query_cloaks(queries)) == Enum.sort(cloak_names)
  end

  test "starting on proper cloak" do
    user = create_user!()

    ds1 = create_data_source!(user)
    queries1 = Enum.map(1..10, fn _ -> create_query!(user, ds1) end)

    ds2 = create_data_source!(user)
    queries2 = Enum.map(1..10, fn _ -> create_query!(user, ds2) end)

    cloak1 = start_cloak(ds1, List.duplicate(:ok, length(queries1)))
    cloak2 = start_cloak(ds2, List.duplicate(:ok, length(queries2)))

    Starter.run()

    assert Query.awaiting_start() == []
    assert query_cloaks(queries1) == [cloak1]
    assert query_cloaks(queries2) == [cloak2]
  end

  test "handling maximum cloak capacity" do
    user = create_user!()
    data_source = create_data_source!(user)
    queries = Enum.map(1..10, fn _ -> create_query!(user, data_source) end)

    _cloak1 = start_cloak(data_source, [{:error, :too_many_queries}])
    cloak2 = start_cloak(data_source, List.duplicate(:ok, length(queries)))

    Starter.run()

    assert Query.awaiting_start() == []
    assert query_cloaks(queries) == [cloak2]
  end

  test "query is left in the awaiting state if there's no available cloak" do
    user = create_user!()
    data_source = create_data_source!(user)
    query = create_query!(user, data_source)
    Starter.run()

    assert same_queries?(Query.awaiting_start(), [query])
  end

  test "query is left in the awaiting state if all cloaks are on maximum capacity" do
    user = create_user!()
    data_source = create_data_source!(user)
    query = create_query!(user, data_source)
    Enum.each(1..10, fn _ -> start_cloak(data_source, [{:error, :too_many_queries}]) end)
    Starter.run()

    assert same_queries?(Query.awaiting_start(), [query])
  end

  test "timeout error is immediately reported" do
    user = create_user!()
    data_source = create_data_source!(user)
    %{id: query_id} = create_query!(user, data_source)
    Air.Service.Query.Events.subscribe(query_id)
    start_cloak(data_source, [{:error, :timeout}])
    Starter.run()

    assert_receive {:query_state_change, %{query_id: ^query_id, state: :query_died}}
    assert soon(Air.Repo.get!(Air.Schemas.Query, query_id).query_state == :error)
    assert Query.awaiting_start() == []
    assert soon(is_nil(Air.Service.Query.Lifecycle.whereis(query_id)))
  end

  test "scheduler ignores the cloak which times out" do
    user = create_user!()
    data_source = create_data_source!(user)
    [%{id: query_id} | other_queries] = Enum.map(1..10, fn _ -> create_query!(user, data_source) end)
    Air.Service.Query.Events.subscribe(query_id)
    start_cloak(data_source, [{:error, :timeout}])
    Starter.run()

    assert_receive {:query_state_change, %{query_id: ^query_id, state: :query_died}}
    assert soon(Air.Repo.get!(Air.Schemas.Query, query_id).query_state == :error)

    assert Air.Repo.get!(Air.Schemas.Query, query_id).result["error"] ==
             "The query could not be started due to a communication timeout."

    assert same_queries?(Query.awaiting_start(), other_queries)
    assert soon(is_nil(Air.Service.Query.Lifecycle.whereis(query_id)))
  end

  test "reporting an error if a query has not been succesfully started for too long" do
    user = create_user!()
    data_source = create_data_source!(user)
    two_days_ago = NaiveDateTime.add(NaiveDateTime.utc_now(), -:timer.hours(48), :millisecond)

    %{id: query_id} =
      user
      |> create_query!(data_source)
      |> Ecto.Changeset.change(inserted_at: two_days_ago)
      |> Air.Repo.update!()

    Air.Service.Query.Events.subscribe(query_id)
    Starter.run()

    assert_receive {:query_state_change, %{query_id: ^query_id, state: :query_died}}
    assert soon(Air.Repo.get!(Air.Schemas.Query, query_id).query_state == :error)

    assert Air.Repo.get!(Air.Schemas.Query, query_id).result["error"] ==
             "The query could not be started because there was no cloak available."

    assert Query.awaiting_start() == []
    assert soon(is_nil(Air.Service.Query.Lifecycle.whereis(query_id)))
  end

  defp create_user!(), do: TestRepoHelper.create_user!(%{groups: [TestRepoHelper.create_group!().id]})

  defp create_data_source!(user) do
    Air.Service.DataSource.create!(%{
      "name" => "data_source_#{System.unique_integer([:positive, :monotonic])}",
      "tables" => Jason.encode!([]),
      "groups" => [hd(user.groups).id]
    })
  end

  defp create_query!(user, data_source), do: TestRepoHelper.create_query!(user, %{data_source_id: data_source.id})

  defp start_cloak(data_source, expected_responses) do
    cloak_fun = fn ->
      cloak_name = "cloak_#{System.unique_integer([:positive, :monotonic])}"
      socket = TestSocketHelper.connect!(%{cloak_name: cloak_name})
      TestSocketHelper.join!(socket, "main", %{data_sources: [%{name: data_source.name, tables: []}]})
      :proc_lib.init_ack({:ok, cloak_name})

      expected_responses
      |> Stream.map(&with :ok <- &1, do: {:ok, nil})
      |> Enum.each(fn {status, result} -> TestSocketHelper.respond_to_start_task_request!(socket, status, result) end)

      Process.exit(socket, :shutdown)
    end

    {:ok, cloak_name} = :proc_lib.start_link(Kernel, :apply, [cloak_fun, []])
    cloak_name
  end

  defp query_cloaks(queries), do: queries |> reload_queries() |> Stream.map(& &1.cloak_id) |> Enum.dedup()

  defp reload_queries(queries), do: Enum.map(queries, &Air.Repo.get!(&1.__struct__, &1.id))

  defp same_queries?(queries_1, queries_2) do
    ids1 = Enum.map(queries_1, & &1.id)
    ids2 = Enum.map(queries_2, & &1.id)
    Enum.sort(ids1) == Enum.sort(ids2)
  end
end
