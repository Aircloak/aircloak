defmodule IntegrationTest.CloakConfigChangeTest do
  use ExUnit.Case, async: false

  import Aircloak.AssertionHelper

  @data_source_name "test_data_source_name"

  setup_all do
    original_data_sources = Cloak.DataSource.all()
    on_exit(fn -> Cloak.DataSource.replace_all_data_source_configs(original_data_sources) end)
  end

  setup do
    Cloak.DataSource.reinitialize_all_data_sources()
  end

  test "new data source definition files are detected" do
    assert :error = Cloak.DataSource.fetch(@data_source_name)
    create_data_source_with_cleanup()
    assert_soon {:ok, _} = Cloak.DataSource.fetch(@data_source_name), soon_opts()
  end

  test "updates to existing data source definitions are detected" do
    create_data_source_with_cleanup()
    assert_soon {:ok, _} = Cloak.DataSource.fetch(@data_source_name), soon_opts()

    data_source_content()
    |> put_in([:parameters, :dummy_field], "dummy_value")
    |> write_data_source()

    soon soon_opts() do
      assert {:ok, updated_data_source} = Cloak.DataSource.fetch(@data_source_name)
      assert data_source_content().parameters[:dummy_field] != updated_data_source.parameters[:dummy_field]
    end
  end

  test "data source definition removal is detected" do
    create_data_source_with_cleanup()
    assert_soon {:ok, _} = Cloak.DataSource.fetch(@data_source_name), soon_opts()
    File.rm!(data_source_path())
    assert_soon :error = Cloak.DataSource.fetch(@data_source_name), soon_opts()
  end

  # -------------------------------------------------------------------
  # Test helper functions
  # -------------------------------------------------------------------

  defp soon_opts(), do: [attempts: 5, timeout: :timer.seconds(1)]

  defp create_data_source_with_cleanup() do
    write_data_source()
    on_exit(fn -> File.rm(data_source_path()) end)
  end

  defp data_source_content() do
    %{
      driver: "postgresql",
      name: @data_source_name,
      parameters: %{
        hostname: "localhost",
        username: "postgres",
        database: "cloaktest1"
      },
      tables: %{}
    }
  end

  defp write_data_source(content \\ data_source_content()), do: File.write!(data_source_path(), Jason.encode!(content))

  defp data_source_path(), do: path_for_config(@data_source_name <> ".json")

  defp path_for_config(name),
    do: Path.join([Aircloak.File.config_dir_path(:cloak), Aircloak.DeployConfig.fetch!(:cloak, "data_sources"), name])
end
