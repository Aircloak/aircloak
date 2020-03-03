defmodule Cloak.DataSource.Bounds.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Bounds
  alias Cloak.TestBoundsCache

  test "returns the cached values if present" do
    data_source = %{
      name: "bounds_test_data_source",
      tables: %{table: %{name: "table"}}
    }

    TestBoundsCache.set(data_source, "table", "column", {100, 200})

    assert {100, 200} = Bounds.bounds(data_source, "table", "column")
    assert {:ok, {100, 200}} = Bounds.cache_lookup(data_source, "table", "column")
  end

  test "returns :unknown if absent" do
    data_source = %{
      name: "bounds_test_data_source",
      tables: %{table: %{name: "table"}}
    }

    assert :unknown = Bounds.bounds(data_source, "table", "other_column")
    assert {:error, _} = Bounds.cache_lookup(data_source, "table", "other_column")
  end
end
