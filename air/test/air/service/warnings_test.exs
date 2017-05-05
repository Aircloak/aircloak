defmodule Air.Service.WarningsTest do
  use Air.SchemaCase, async: false

  alias Air.Service.{Warnings, Cloak}
  alias Air.Schemas.{Group, DataSource, User}
  alias Air.Repo

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
    assert problem_with_description(~r/No cloaks .+ are online/).resource.name == @data_source_name
  end

  test "broken data source produce warnings" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources_with_errors)

    assert Warnings.known_problems?()
    assert hd(Warnings.problems()).resource.name == @data_source_name
    assert problem_with_description(~r/broken/).resource.name == @data_source_name
  end

  test "no warning when data source is online and no errors" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources)
    @data_sources
    |> add_group()
    |> add_user()
    refute Warnings.known_problems?()
  end

  test "warning when data source has no groups" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources)
    assert Warnings.known_problems?()
    assert problem_with_description(~r/no groups/i)
  end

  test "warning when data source has no users despite having a group" do
    {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources)
    add_group(@data_sources)
    assert Warnings.known_problems?()
    assert problem_with_description(~r/no users/i)
  end

  describe("problems_for_resource") do
    test "data source" do
      {:ok, _pid} = start_cloak_channel(cloak_info(), @data_sources_with_errors)
      [%{"name" => name}] = @data_sources_with_errors
      data_source = Repo.get_by!(DataSource, name: name)
      assert problem_with_description(Warnings.problems_for_resource(data_source), ~r/broken/).resource.name == name
    end
  end

  defp problem_with_description(problems \\ Warnings.problems(), pattern) do
    filtered_problems = problems
    |> Enum.filter(&(&1.description =~ pattern))
    case filtered_problems do
      [] -> raise "Expected a problem matching the pattern #{inspect pattern}, but there was none"
      [problem] -> problem
      problems ->
        raise "Expected exactly one problem matching the pattern #{inspect pattern}, " ++
          "but there were #{length(problems)}."
    end
  end

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

  defp add_user(group) do
    params = %{
      name: "user_#{:erlang.unique_integer()}",
      email: "random_#{:erlang.unique_integer}@example.com",
      groups: [group.id]
    }
    %User{}
    |> User.changeset(params)
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
