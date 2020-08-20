defmodule Air.Service.DataSource.Column.Test do
  use Air.SchemaCase
  alias Air.Service.DataSource.Column

  @tables [
    "all_ok",
    "all_pending",
    "all_failed",
    "isolated_failed",
    "isolated_pending",
    "shadow_failed",
    "shadow_pending",
    "bounds_failed",
    "bounds_pending"
  ]

  defmacrop true_for(func_name, table_names) do
    quote do
      ds = var!(context).data_source
      assert unquote("all_ok" in table_names) == Column.unquote(func_name)(column("all_ok", ds))
      assert unquote("all_pending" in table_names) == Column.unquote(func_name)(column("all_pending", ds))
      assert unquote("all_failed" in table_names) == Column.unquote(func_name)(column("all_failed", ds))
      assert unquote("isolated_failed" in table_names) == Column.unquote(func_name)(column("isolated_failed", ds))
      assert unquote("isolated_pending" in table_names) == Column.unquote(func_name)(column("isolated_pending", ds))
      assert unquote("shadow_failed" in table_names) == Column.unquote(func_name)(column("shadow_failed", ds))
      assert unquote("shadow_pending" in table_names) == Column.unquote(func_name)(column("shadow_pending", ds))
      assert unquote("bounds_failed" in table_names) == Column.unquote(func_name)(column("bounds_failed", ds))
      assert unquote("bounds_pending" in table_names) == Column.unquote(func_name)(column("bounds_pending", ds))
    end
  end

  defmacrop false_for(func_name, false_tables) do
    quote do
      true_for(unquote(func_name), unquote(@tables -- false_tables))
    end
  end

  setup do
    {:ok, data_source: data_source()}
  end

  test ".isolators_computed?", context do
    :isolators_computed?
    |> false_for(["all_pending", "all_failed", "isolated_pending", "isolated_failed"])
  end

  test ".isolators_pending?", context do
    :isolators_pending?
    |> true_for(["all_pending", "isolated_pending"])
  end

  test ".isolators_failed?", context do
    :isolators_failed?
    |> true_for(["all_failed", "isolated_failed"])
  end

  test ".shadow_computed?", context do
    :shadow_computed?
    |> false_for(["all_pending", "all_failed", "shadow_pending", "shadow_failed"])
  end

  test ".shadow_pending?", context do
    :shadow_pending?
    |> true_for(["all_pending", "shadow_pending"])
  end

  test ".shadow_failed?", context do
    :shadow_failed?
    |> true_for(["all_failed", "shadow_failed"])
  end

  test ".bounds_computed?", context do
    :bounds_computed?
    |> false_for(["all_pending", "all_failed", "bounds_pending", "bounds_failed"])
  end

  test ".bounds_pending?", context do
    :bounds_pending?
    |> true_for(["all_pending", "bounds_pending"])
  end

  test ".bounds_failed?", context do
    :bounds_failed?
    |> true_for(["all_failed", "bounds_failed"])
  end

  test ".analyzed_successfully?", context do
    :analyzed_successfully?
    |> true_for(["all_ok"])
  end

  test ".analysis_pending?", context do
    :analysis_pending?
    |> true_for(["all_pending", "isolated_pending", "shadow_pending", "bounds_pending"])
  end

  test ".analysis_failed?", context do
    :analysis_failed?
    |> true_for(["all_failed", "isolated_failed", "shadow_failed", "bounds_failed"])
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
          %{name: "all_ok", shadow_table: :ok, isolated: true, bounds: :ok},
          %{name: "all_pending", shadow_table: :pending, isolated: :pending, bounds: :pending},
          %{name: "all_failed", shadow_table: :failed, isolated: :failed, bounds: :failed},
          %{name: "isolated_pending", shadow_table: :ok, isolated: :pending, bounds: :ok},
          %{name: "isolated_failed", shadow_table: :ok, isolated: :failed, bounds: :ok},
          %{name: "shadow_pending", shadow_table: :pending, isolated: true, bounds: :ok},
          %{name: "shadow_failed", shadow_table: :failed, isolated: true, bounds: :ok},
          %{name: "bounds_pending", shadow_table: :ok, isolated: true, bounds: :pending},
          %{name: "bounds_failed", shadow_table: :ok, isolated: true, bounds: :failed}
        ]
      }
    ]

    Air.Service.DataSource.create_or_update_data_source(%{name: "new_name", tables: tables})
  end
end
