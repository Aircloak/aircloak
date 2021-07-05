defmodule Air.Service.Cleanup.Test do
  use ExUnit.Case, async: false

  alias Air.{Settings, Repo, TestRepoHelper, TestSocketHelper, Schemas.Query, Service.Cleanup, Service.Logs}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    :ok = Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    Repo.delete_all(Query)
    :ok
  end

  describe "cleanup_old_queries" do
    test "does nothing with unlimited retention" do
      query = create_query!()
      Cleanup.cleanup_old_queries(%Settings{query_retention_days: :unlimited})
      assert Repo.all(Query) |> Enum.map(& &1.id) == [query.id]
    end

    test "removes queries older than retention days" do
      create_query!()
      Cleanup.cleanup_old_queries(%Settings{query_retention_days: 10}, _now = in_days(11))
      assert Repo.all(Query) == []
    end

    test "leaves queries not older than retention days" do
      query = create_query!()
      Cleanup.cleanup_old_queries(%Settings{query_retention_days: 10}, _now = in_days(9))
      assert Repo.all(Query) |> Enum.map(& &1.id) == [query.id]
    end
  end

  describe "cleanup_dead_queries" do
    test "erroring a query not found in connected cloaks" do
      query = create_query!(%{query_state: :started})
      Cleanup.cleanup_dead_queries()
      :timer.sleep(100)

      assert %Query{query_state: :error, result: %{"error" => "Query died."}} = Repo.get!(Query, query.id)
    end

    test "not erroring a query found in connected cloaks" do
      query = create_query!()

      {:connected, cloak} = TestSocketHelper.connect(%{cloak_name: "cloak"})
      TestSocketHelper.join!(cloak, "main", %{data_sources: [%{name: "ds", tables: []}]})

      task = Task.async(fn -> Cleanup.cleanup_dead_queries() end)
      TestSocketHelper.respond_to_running_queries!(cloak, [query.id])
      Task.await(task)
      :timer.sleep(100)

      assert %Query{query_state: :created, result: nil} = Repo.get!(Query, query.id)
    end

    test "query which is not yet started is not cleaned up" do
      query = create_query!()
      Cleanup.cleanup_dead_queries()
      :timer.sleep(100)

      assert %Query{query_state: :created} = Repo.get!(Query, query.id)
    end
  end

  test "removes logs older than retention days" do
    :ok = Logs.save(in_days(0), :air, "host", :info, "recent")
    :ok = Logs.save(in_days(-2), :air, "host", :info, "old")
    Cleanup.cleanup_old_logs()
    assert [%{message: "recent"}] = Logs.tail(in_days(-10), 10)
  end

  defp in_days(days) do
    Timex.shift(NaiveDateTime.utc_now(), days: days)
  end

  defp create_query!(additional_data \\ %{}) do
    TestRepoHelper.create_query!(
      TestRepoHelper.create_user!(),
      Map.merge(%{data_source_id: TestRepoHelper.create_data_source!().id}, additional_data)
    )
  end
end
