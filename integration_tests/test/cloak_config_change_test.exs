defmodule IntegrationTest.CloakConfigChangeTest do
  use ExUnit.Case, async: false

  @data_source_name "test_data_source_name"

  setup do
    :erlang.trace(Process.whereis(Cloak.DataSource), true, [:call])
    :erlang.trace_pattern({Cloak.DataSource, :_, :_}, [{:_, [], []}])

    # stop tracing on exit, since there can be only one tracer for the given target process
    on_exit(fn ->
      Cloak.DataSource.reinitialize_all_data_sources()
      :erlang.trace(Process.whereis(Cloak.DataSource), false, [:call])
    end)
  end

  test "new data source definition files are detected" do
    assert :error == Cloak.DataSource.fetch(@data_source_name)
    create_data_source_with_cleanup()
    assert {:ok, _} = Cloak.DataSource.fetch(@data_source_name)
  end

  test "updates to existing data source definitions are detecetd" do
    create_data_source_with_cleanup()
    {:ok, original_data_source} = Cloak.DataSource.fetch(@data_source_name)

    data_source_content()
    |> Map.put("marker", original_data_source.marker <> "_altered")
    |> write_data_source()
    wait_for_data_source()

    assert {:ok, updated_data_source} = Cloak.DataSource.fetch(@data_source_name)
    assert original_data_source.marker != updated_data_source.marker
  end

  test "data source definition removal are detected" do
    create_data_source()
    {:ok, _} = Cloak.DataSource.fetch(@data_source_name)
    File.rm!(data_source_path())
    wait_for_data_source()
    assert :error = Cloak.DataSource.fetch(@data_source_name)
  end

  defp create_data_source() do
    write_data_source()
    wait_for_data_source()
  end

  defp create_data_source_with_cleanup() do
    create_data_source()
    on_exit(fn -> File.rm!(data_source_path()) end)
  end

  defp data_source_content() do
    %{
      driver: "postgresql",
      marker: "native",
      name: @data_source_name,
      parameters: %{
        hostname: "localhost",
        username: "postgres",
        database: "cloaktest1"
      },
      tables: %{},
    }
  end

  defp wait_for_data_source(), do:
    assert_receive {:trace, _, :call, {Cloak.DataSource, :handle_cast, _}}, :timer.seconds(1)

  defp write_data_source(content \\ data_source_content()), do:
    File.write!(data_source_path(), Poison.encode!(content))

  defp data_source_path(), do:
    path_for_config(@data_source_name <> ".json")

  defp path_for_config(name), do:
    Path.join([File.cwd!(), "../cloak/priv/config/integration_tests", name])
end
