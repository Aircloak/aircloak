defmodule Air.Service.WarningsTest do
  use Air.SchemaCase, async: false

  alias Air.Service.{Warnings, Cloak}
  alias Air.{Repo, Schemas.Group, Schemas.DataSource}

  @data_source_name "name"
  @data_sources [%{"name" => @data_source_name, "global_id" => "global_id", "tables" => []}]
  @data_sources_with_errors [%{"name" => @data_source_name, "global_id" => "global_id", "tables" => [],
    "errors" => ["broken"]}]

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "offline data source produce warnings" do
    spawn_monitor(fn() -> Cloak.register(cloak_info(), @data_sources) end)
    receive do
      {:DOWN, _, _, _, _} -> :ok
    end

    assert Warnings.known_problems?()
    assert hd(Warnings.problems()).resource.name == @data_source_name
    assert problem_with_description(~r/No cloaks .+ are online/)
  end

  test "broken data source produce warnings" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources_with_errors)

    assert Warnings.known_problems?()
    assert hd(Warnings.problems()).resource.name == @data_source_name
    assert problem_with_description(~r/broken/)
  end

  test "no warning when data source is online and no errors" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources)
    add_group(@data_sources)
    refute Warnings.known_problems?()
  end

  test "warning when data source has no groups" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources)
    assert Warnings.known_problems?()
    assert problem_with_description(~r/no groups/i)
  end

  defp problem_with_description(pattern), do:
    Warnings.problems()
    |> Enum.map(&(&1.description))
    |> Enum.any?(&(&1 =~ pattern))

  defp cloak_info() do
    %{
      id: "cloak_id_#{:erlang.unique_integer()}",
      name: "cloak_name",
      online_since: Timex.now()
    }
  end

  defp add_group([%{"name" => name}]) do
    data_source = Repo.get_by!(DataSource, name: name)
    params = %{
      name: "group_#{data_source.name}",
      admin: false,
      data_sources: [data_source.id],
    }
    %Group{}
    |> Group.changeset(params)
    |> Repo.insert!()
  end

  defp start_cloak_channel(cloak_info, data_sources) do
    parent = self()
    ref = make_ref()

    pid = spawn_link(fn ->
      registration_result = Cloak.register(cloak_info, data_sources)
      send(parent, {ref, registration_result})
      :timer.sleep(:infinity)
    end)

    receive do
      {^ref, registration_result} -> {registration_result, pid}
    end
  end
end
