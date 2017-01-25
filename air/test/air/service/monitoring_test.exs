defmodule Air.Service.Monitoring.Test do
  use ExUnit.Case, async: false

  alias Air.{Repo, Service.Monitoring, TestRepoHelper}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "assemble_info" do
    test "uptime" do
      uptime1 = Monitoring.assemble_info().uptime
      uptime2 = Monitoring.assemble_info().uptime

      assert uptime2 > uptime1
    end

    test "list of group names" do
      group = TestRepoHelper.create_group!()
      assert group.name in Monitoring.assemble_info().groups
    end

    test "list of users with group names" do
      group = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group.id]})

      assert %{email: user.email, name: user.name, groups: [group.name]} in Monitoring.assemble_info().users
    end

    test "list of cloaks" do
      cloak_info = cloak_info()
      :ok = Air.DataSourceManager.register_cloak(cloak_info, [])

      assert %{name: cloak_info.name} in Monitoring.assemble_info().cloaks
    end

    test "list of datasources" do
      data_source = TestRepoHelper.create_data_source!()
      TestRepoHelper.create_query!(TestRepoHelper.create_user!(), %{data_source_id: data_source.id})

      assert %{
        name: data_source.name,
        queries: %{
          last_5_minutes: 0,
          last_15_minutes: 0,
          last_30_minutes: 0,
          last_1_hour: 1,
          last_1_day: 1,
        }
      } in Monitoring.assemble_info(in_minutes(45)).data_sources
    end
  end

  defp in_minutes(minutes), do: NaiveDateTime.utc_now() |> Timex.shift(minutes: minutes)

  defp cloak_info() do
    %{
      id: "cloak_id_#{:erlang.unique_integer()}",
      name: "cloak_name",
      online_since: Timex.now()
    }
  end
end
