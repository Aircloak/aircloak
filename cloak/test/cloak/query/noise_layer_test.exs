defmodule Cloak.Query.NoiseLayerTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("noise_layers", "number REAL")
  end

  setup do
    Cloak.Test.DB.clear_table("noise_layers")

    anonymizer_config = Application.get_env(:cloak, :anonymizer)
    Application.put_env(:cloak, :anonymizer, Keyword.put(anonymizer_config, :outliers_count, {4, 1}))
    on_exit(fn() -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)

    :ok
  end

  test "count(*) uses a different noise layer than count(column)" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [6])
    :ok = insert_rows(_user_ids = 1..4, "noise_layers", ["number"], [3])

    assert_query "select count(*), count(number) from noise_layers where number <> 0",
      %{rows: [%{row: [value1, value2]}]}
    assert value1 != value2
  end
end
