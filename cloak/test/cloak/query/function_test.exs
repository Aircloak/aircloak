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

  defmacrop assert_function(expression, expected_match) do
    quote do
      assert_query(
        "select (#{unquote(expression)}) as elixir_res from heights",
        unquote(expected_match)
      )

      assert_query(
        "select sql_res from (select user_id, (#{unquote(expression)}) as sql_res from heights) alias",
        unquote(expected_match)
      )
    end
  end

  defmacrop assert_function_success(expression, expected_result) do
    quote do
      assert_function(unquote(expression), %{rows: [%{row: [unquote(expected_result)], occurrences: 100}]})
    end
  end

  defmacrop assert_function_error(expression, expected_error) do
    quote do
      assert_function(unquote(expression), %{error: unquote(expected_error)})
    end
  end

  test "+", do: assert_function_success("height + 1", 181)
  test "-", do: assert_function_success("height - 1", 179)
  test "*", do: assert_function_success("height * 2", 360)
  test "/", do: assert_function_success("height / 2", 90.0)
  test "^", do: assert_function_success("height ^ 2", 32_400.0)
  test "trunc/1", do: assert_function_success("trunc(height + 0.6)", 180)
  test "trunc/2", do: assert_function_success("trunc(height + 0.126, 2)", 180.12)
  test "cast as integer", do: assert_function_success("cast('42' AS integer)", 42)
  test "cast as real", do: assert_function_success("cast('42' AS real)", 42.0)
  test "cast as text", do: assert_function_success("cast(42 AS text)", "42")

  test "type errors", do: assert_function_error("trunc('foobar')", "Function `trunc` requires arguments of type" <> _)
end
