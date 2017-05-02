defmodule Air.Service.Monitoring.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  alias Air.{Service.Monitoring, TestRepoHelper, Schemas.Group}

  @seconds_in_minute 60

  describe "assemble_info" do
    test "has air version", do:
      refute is_nil(Monitoring.assemble_info().version)

    test "uptime", do:
      assert is_integer(Monitoring.assemble_info().uptime)

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
      :ok = Air.Service.Cloak.register(cloak_info,
        [%{"name" => "data_source_name", "global_id" => "a very global id", "tables" => []}])
      memory_reading = %{free_memory: 100}
      Air.Service.Cloak.record_memory(memory_reading)
      TestRepoHelper.create_query!(TestRepoHelper.create_user!(), %{cloak_id: cloak_info.id})

      cloak_name = cloak_info.name
      assert [%{
        name: ^cloak_name,
        version: "17.1.0",
        data_sources: ["data_source_name"],
        uptime: uptime,
        queries: %{
          last_5_minutes: 0,
          last_15_minutes: 0,
          last_30_minutes: 1,
          last_1_hour: 1,
          last_1_day: 1,
        },
        memory: ^memory_reading,
      }] = Monitoring.assemble_info(in_minutes(20)).cloaks
      assert uptime >= 20 * @seconds_in_minute
      assert uptime <= 21 * @seconds_in_minute
    end

    test "list of datasources" do
      data_source = TestRepoHelper.create_data_source!()
      group = create_group_for_data_source!(data_source)
      TestRepoHelper.create_query!(TestRepoHelper.create_user!(), %{data_source_id: data_source.id})

      assert %{
        id: data_source.id,
        name: data_source.name,
        queries: %{
          last_5_minutes: 0,
          last_15_minutes: 0,
          last_30_minutes: 0,
          last_1_hour: 1,
          last_1_day: 1,
        },
        groups: [group.name],
        errors: [],
      } in Monitoring.assemble_info(in_minutes(45)).data_sources
    end
  end

  defp in_minutes(minutes), do: NaiveDateTime.utc_now() |> Timex.shift(minutes: minutes)

  defp cloak_info() do
    %{
      id: "cloak_id_#{:erlang.unique_integer()}",
      version: "17.1.0",
      name: "cloak_name",
      online_since: Timex.now()
    }
  end

  defp create_group_for_data_source!(data_source) do
    TestRepoHelper.create_group!()
    |> Repo.preload(:data_sources)
    |> Group.changeset()
    |> put_assoc(:data_sources, [data_source])
    |> Repo.update!()
  end
end
