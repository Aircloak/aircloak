defmodule Air.Service.DataSource.Column.Test do
  use Air.SchemaCase
  alias Air.Service.DataSource.Column

  setup do
    {:ok, data_source: data_source()}
  end

  test ".isolators_computed?", %{data_source: data_source} do
    assert Column.isolators_computed?(column("all_ok", data_source))
    refute Column.isolators_computed?(column("isolated_failed", data_source))
    assert Column.isolators_computed?(column("shadow_failed", data_source))
    refute Column.isolators_computed?(column("pending", data_source))
  end

  test ".isolators_failed?", %{data_source: data_source} do
    refute Column.isolators_failed?(column("all_ok", data_source))
    assert Column.isolators_failed?(column("isolated_failed", data_source))
    refute Column.isolators_failed?(column("shadow_failed", data_source))
    refute Column.isolators_failed?(column("pending", data_source))
  end

  test ".shadow_computed?", %{data_source: data_source} do
    assert Column.shadow_computed?(column("all_ok", data_source))
    assert Column.shadow_computed?(column("isolated_failed", data_source))
    refute Column.shadow_computed?(column("shadow_failed", data_source))
    refute Column.shadow_computed?(column("pending", data_source))
  end

  test ".shadow_failed?", %{data_source: data_source} do
    refute Column.shadow_failed?(column("all_ok", data_source))
    refute Column.shadow_failed?(column("isolated_failed", data_source))
    assert Column.shadow_failed?(column("shadow_failed", data_source))
    refute Column.shadow_failed?(column("pending", data_source))
  end

  test ".analyzed?", %{data_source: data_source} do
    assert Column.analyzed?(column("all_ok", data_source))
    refute Column.analyzed?(column("isolated_failed", data_source))
    refute Column.analyzed?(column("shadow_failed", data_source))
    refute Column.analyzed?(column("pending", data_source))
  end

  test ".analysis_failed?", %{data_source: data_source} do
    refute Column.analysis_failed?(column("all_ok", data_source))
    assert Column.analysis_failed?(column("isolated_failed", data_source))
    assert Column.analysis_failed?(column("shadow_failed", data_source))
    refute Column.analysis_failed?(column("pending", data_source))
  end

  defp column(name, data_source) do
    hd(Air.Schemas.DataSource.tables(data_source))["columns"]
    |> Enum.find(&(&1["name"] == name))
  end

  defp data_source() do
    tables = [
      %{
        id: "table_id",
        columns: [
          %{name: "all_ok", shadow_table: :ok, isolated: true},
          %{name: "isolated_failed", shadow_table: :ok, isolated: :failed},
          %{name: "shadow_failed", shadow_table: :failed, isolated: true},
          %{name: "pending", shadow_table: :pending, isolated: :pending}
        ]
      }
    ]

    Air.Service.DataSource.create_or_update_data_source("new_name", tables, [])
  end
end
