defmodule Air.Service.DataSourceTest do
  use Air.ModelCase, async: false # because of shared mode

  alias Air.{Service.DataSource, TestRepoHelper}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})

    g1 = TestRepoHelper.create_group!()
    g2 = TestRepoHelper.create_group!()
    g3 = TestRepoHelper.create_group!()

    user1 = TestRepoHelper.create_user!(%{groups: [g1.id, g2.id]})
    user2 = TestRepoHelper.create_user!(%{groups: [g2.id, g3.id]})
    user3 = TestRepoHelper.create_user!(%{groups: [g3.id]})

    ds1 = TestRepoHelper.create_data_source!(%{groups: [g1.id]})
    ds2 = TestRepoHelper.create_data_source!(%{groups: [g2.id]})

    %{user1: user1, user2: user2, user3: user3, ds1: ds1, ds2: ds2}
  end

  test "count" do
    initial_count = DataSource.count()

    Enum.each(1..5, fn(_) -> TestRepoHelper.create_data_source!() end)
    assert DataSource.count() == initial_count + 5
  end

  test "fetching user's data sources", context do
    assert Enum.map(DataSource.for_user(context.user1), &(&1.id)) == [context.ds1.id, context.ds2.id]
    assert Enum.map(DataSource.for_user(context.user2), &(&1.id)) == [context.ds2.id]
    assert Enum.map(DataSource.for_user(context.user3), &(&1.id)) == []
  end

  test "fetching available data source", context do
    assert {:ok, data_source} = DataSource.fetch_as_user(context.ds1.id, context.user1)
    assert data_source.id == context.ds1.id
  end

  test "fetching unavailable data source", context, do:
    assert {:error, :unauthorized} == DataSource.fetch_as_user(context.ds1.id, context.user3)

  test "fetching user's history", context do
    queries = Enum.map(1..5, fn(_) -> create_query(context.user1, context.ds2) end)
    _other_user_queries = Enum.map(1..5, fn(_) -> create_query(context.user2, context.ds2) end)

    for count <- 1..6 do
      assert {:ok, history} = DataSource.history(context.ds2.id, context.user1, count)
      assert Enum.map(history, &(&1.id)) ==
        queries |> Enum.reverse() |> Enum.take(min(5, count)) |> Enum.map(&(&1.id))
    end
  end

  test "fetching history for the unavailable data source", context, do:
    assert {:error, :unauthorized} == DataSource.history(context.ds2.id, context.user3, 1)

  test "fetching last query", context do
    queries = Enum.map(1..5, fn(_) -> create_query(context.user1, context.ds2) end)
    _other_user_queries = Enum.map(1..5, fn(_) -> create_query(context.user2, context.ds2) end)

    assert {:ok, query} = DataSource.last_query(context.ds2.id, context.user1)
    assert query.id == List.last(queries).id
  end

  test "fetching last query when there are no queries", context, do:
    assert {:ok, nil} == DataSource.last_query(context.ds2.id, context.user1)

  test "fetching last query for the unavailable data source", context, do:
    assert {:error, :unauthorized} == DataSource.last_query(context.ds2.id, context.user3)

  defp create_query(user, data_source), do:
    TestRepoHelper.create_query!(user, %{statement: "query content", data_source_id: data_source.id})
end
