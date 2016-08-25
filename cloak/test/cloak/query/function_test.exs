defmodule Cloak.Query.FunctionTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    Cloak.Test.DB.setup()
    Cloak.Test.DB.create_test_schema()
    :ok = Cloak.Test.DB.create_table("heights", "height INTEGER, name TEXT")
    Cloak.Test.DB.clear_table("heights")
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
  end

  defmacrop assert_function(expression, expected) do
    quote do
      assert_query(
        "select (#{unquote(expression)}) as elixir_res from heights",
        %{columns: ["elixir_res"], rows: [%{row: [unquote(expected)], occurrences: 100}]}
      )

      assert_query(
        "select sql_res from (select user_id, (#{unquote(expression)}) as sql_res from heights) alias",
        %{columns: ["sql_res"], rows: [%{row: [unquote(expected)], occurrences: 100}]}
      )
    end
  end

  test "+", do: assert_function("height + 1", 181)
  test "-", do: assert_function("height - 1", 179)
  test "*", do: assert_function("height * 2", 360)
  test "/", do: assert_function("height / 2", 90.0)
  test "^", do: assert_function("height ^ 2", 32_400.0)
  test "trunc/1", do: assert_function("trunc(height + 0.6)", 180)
  test "trunc/2", do: assert_function("trunc(height + 0.126, 2)", 180.12)
end
