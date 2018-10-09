defmodule Air.Service.WarningsTest do
  use Air.SchemaCase, async: false

  alias Air.Service.{Warnings, Cloak}
  alias Air.Schemas.DataSource
  alias Air.{Repo, TestRepoHelper}

  @data_source_name "name"
  @data_sources [%{name: @data_source_name, tables: []}]
  @data_sources_with_errors [%{name: @data_source_name, tables: [], errors: ["broken"]}]
  @data_sources_with_failed_isolators [
    %{
      name: @data_source_name,
      tables: [%{id: "failed_table", columns: [%{name: "failed_isolator", shadow_table: :ok, isolated: :failed}]}],
      errors: []
    }
  ]
  @data_sources_with_failed_shadows [
    %{
      name: @data_source_name,
      tables: [%{id: "failed_table", columns: [%{name: "failed_shadow", shadow_table: :failed, isolated: true}]}],
      errors: []
    }
  ]

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    TestRepoHelper.create_privacy_policy!()
    :ok
  end

  test "offline data source produce warnings" do
    spawn_monitor(fn -> Cloak.register(TestRepoHelper.cloak_info(), @data_sources) end)

    receive do
      {:DOWN, _, _, _, _} -> :ok
    end

    assert problem_with_description(~r/No cloaks .+ are online/).resource.name == @data_source_name
  end

  test "broken data source produce warnings" do
    start_cloak_channel(@data_sources_with_errors)

    assert hd(data_source_problems()).resource.name == @data_source_name
    assert problem_with_description(~r/broken/).resource.name == @data_source_name
  end

  test "no warning when data source is online and no errors" do
    start_cloak_channel(@data_sources)

    @data_sources
    |> add_group()
    |> add_user()

    assert data_source_problems() == []
  end

  test "warning when data source has no groups" do
    start_cloak_channel(@data_sources)
    assert problem_with_description(~r/no groups/i)
  end

  test "warning when data source has no users despite having a group" do
    start_cloak_channel(@data_sources)
    add_group(@data_sources)
    assert problem_with_description(~r/no users/i)
  end

  test "warning when data source has failed isolators" do
    start_cloak_channel(@data_sources_with_failed_isolators)
    assert problem_with_description(~r/Cloak could not compute if columns `failed_table.failed_isolator` are isolating/)
  end

  test "warning when data source has failed shadows" do
    start_cloak_channel(@data_sources_with_failed_shadows)
    assert problem_with_description(~r/could not compute frequent values from columns `failed_table.failed_shadow`/)
  end

  describe("problems_for_resource") do
    test "data source" do
      start_cloak_channel(@data_sources_with_errors)
      [%{name: name}] = @data_sources_with_errors
      data_source = Repo.get_by!(DataSource, name: name)

      assert problem_with_description(Warnings.problems_for_resource(data_source), ~r/broken/).resource.name == name
    end
  end

  describe("highest_severity_class") do
    test "high is highest",
      do:
        assert(
          Warnings.highest_severity_class([
            %{severity: :high},
            %{severity: :medium},
            %{severity: :low}
          ]) == :high
        )

    test "medium is higher than low",
      do: assert(Warnings.highest_severity_class([%{severity: :medium}, %{severity: :low}]) == :medium)
  end

  defp problem_with_description(problems \\ Warnings.problems(), pattern) do
    filtered_problems =
      problems
      |> Enum.filter(&(&1.description =~ pattern))

    case filtered_problems do
      [] ->
        raise "Expected a problem matching the pattern #{inspect(pattern)}, but there was none"

      [problem] ->
        problem

      problems ->
        raise "Expected exactly one problem matching the pattern #{inspect(pattern)}, " ++
                "but there were #{length(problems)}."
    end
  end

  defp add_group([%{name: name}]) do
    data_source = Repo.get_by!(DataSource, name: name)

    TestRepoHelper.create_group!(%{
      name: "group_#{data_source.name}",
      admin: false,
      data_sources: [data_source.id]
    })
  end

  defp add_user(group) do
    TestRepoHelper.create_user!(%{
      name: "user_#{:erlang.unique_integer()}",
      login: "random_#{:erlang.unique_integer()}@example.com",
      groups: [group.id]
    })
  end

  defp start_cloak_channel(data_sources) do
    parent = self()
    ref = make_ref()

    pid =
      spawn_link(fn ->
        Cloak.register(TestRepoHelper.cloak_info(), data_sources)
        send(parent, ref)
        :timer.sleep(:infinity)
      end)

    receive do
      ^ref -> pid
    end
  end

  defp data_source_problems(), do: Warnings.problems() |> Enum.filter(&match?(%{resource: %DataSource{}}, &1))
end
