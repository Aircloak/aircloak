defmodule Air.Service.Cleanup.Test do
  use ExUnit.Case, async: false

  alias Air.{Settings, Repo, TestRepoHelper, Schemas.Query, Service.Cleanup}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Repo.delete_all(Query)
    :ok
  end

  describe "cleanup_old_queries" do
    test "does nothing with unlimited retention" do
      query = TestRepoHelper.create_query!(TestRepoHelper.create_user!())
      Cleanup.cleanup_old_queries(%Settings{query_retention_days: :unlimited})
      assert Repo.all(Query) |> Enum.map(&(&1.id)) == [query.id]
    end

    test "removes queries older than retention days" do
      TestRepoHelper.create_query!(TestRepoHelper.create_user!())
      Cleanup.cleanup_old_queries(%Settings{query_retention_days: 10}, _now = in_days(11))
      assert Repo.all(Query) == []
    end

    test "leaves queries not older than retention days" do
      query = TestRepoHelper.create_query!(TestRepoHelper.create_user!())
      Cleanup.cleanup_old_queries(%Settings{query_retention_days: 10}, _now = in_days(9))
      assert Repo.all(Query) |> Enum.map(&(&1.id)) == [query.id]
    end
  end

  defp in_days(days) do
    Timex.shift(NaiveDateTime.utc_now(), days: days)
  end
end
