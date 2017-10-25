defmodule IntegrationTest.CloakConfigChangeTest do
  use ExUnit.Case, async: false

  @data_source_name "test_data_source_name"
  import Aircloak.AssertionHelper

  setup_all do
    on_exit(fn() -> Cloak.DataSource.reinitialize_all_data_sources() end)
    {:ok, original_data_sources: Cloak.DataSource.all()}
  end

  setup [:reset]

  test "new data source definition files are detected" do
    assert soon(fn() -> :error == Cloak.DataSource.fetch(@data_source_name) end)
    create_data_source_with_cleanup()
    assert soon(fn() -> {:ok, _} = Cloak.DataSource.fetch(@data_source_name) end)
  end

  test "updates to existing data source definitions are detecetd" do
    create_data_source_with_cleanup()
    assert soon(fn() -> {:ok, _} = Cloak.DataSource.fetch(@data_source_name) end)

    data_source_content()
    |> Map.put("marker", data_source_content().marker <> "_altered")
    |> write_data_source()

    assert soon(fn() ->
      {:ok, updated_data_source} = Cloak.DataSource.fetch(@data_source_name)
      data_source_content().marker != updated_data_source.marker
    end)
  end

  test "data source definition removal are detected" do
    create_data_source()
    assert soon(fn() -> {:ok, _} = Cloak.DataSource.fetch(@data_source_name) end)
    File.rm!(data_source_path())
    assert soon(fn() -> :error = Cloak.DataSource.fetch(@data_source_name) end)
  end


  # -------------------------------------------------------------------
  # Test helper functions
  # -------------------------------------------------------------------

  defp reset(context) do
    Cloak.DataSource.reinitialize_all_data_sources()
    assert soon(fn() -> data_sources_reset?(context) end, 1000)
    :ok
  end

  defp data_sources_reset?(%{original_data_sources: original_data_sources}), do:
    Enum.sort(original_data_sources) == Enum.sort(Cloak.DataSource.all())

  defp create_data_source(), do:
    write_data_source()

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

  defp write_data_source(content \\ data_source_content()), do:
    File.write!(data_source_path(), Poison.encode!(content))

  defp data_source_path(), do:
    path_for_config(@data_source_name <> ".json")

  defp path_for_config(name), do:
    Path.join([Aircloak.File.config_dir_path(:cloak), "integration_tests", name])
end
