defmodule Cloak.DataSource.Isolators.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Isolators
  alias Cloak.TestIsolatorsCache

  test "delegates to cache when column not included in isolating_columns" do
    data_source = %{
      name: "isolators_test_data_source",
      tables: %{table: %{name: "table", isolating_columns: %{}, auto_isolating_column_classification: true}}
    }

    TestIsolatorsCache.register_isolator(data_source, "table", "column")

    assert Isolators.isolates_users?(data_source, "table", "column")
    assert {:ok, true} = Isolators.cache_lookup(data_source, "table", "column")
  end

  test "returns immediately when column included in isolating_columns" do
    data_source = %{
      name: "isolators_test_data_source",
      tables: %{
        table: %{name: "table", isolating_columns: %{"column" => false}, auto_isolating_column_classification: true}
      }
    }

    TestIsolatorsCache.register_isolator(data_source, "table", "column")

    refute Isolators.isolates_users?(data_source, "table", "column")
    assert {:ok, false} = Isolators.cache_lookup(data_source, "table", "column")
  end

  test "treats as isolating when auto_isolating_column_classification is false" do
    data_source = %{
      name: "isolators_test_data_source",
      tables: %{table: %{name: "table", isolating_columns: %{}, auto_isolating_column_classification: false}}
    }

    assert Isolators.isolates_users?(data_source, "table", "other_column")
    assert {:ok, true} = Isolators.cache_lookup(data_source, "table", "other_column")
  end
end
