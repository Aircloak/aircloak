defmodule Cloak.Query.FunctionTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights_ft", "height INTEGER, name TEXT")
    Cloak.Test.DB.clear_table("heights_ft")
    :ok = insert_rows(_user_ids = 1..100, "heights_ft", ["height"], [180])
  end

  defmacrop assert_top_level_function(expression, expected_match) do
    quote do
      assert_query(
        "select (#{unquote(expression)}) as elixir_res from heights_ft",
        unquote(expected_match)
      )
    end
  end

  defmacrop assert_subquery_function(expression, subquery_postfix \\ "", expected_match) do
    quote do
      assert_query(
        "select sql_res from (
          select user_id, (#{unquote(expression)}) as sql_res from heights_ft #{unquote(subquery_postfix)}
        ) alias",
        unquote(expected_match)
      )
    end
  end

  defmacrop assert_function_success(expression, expected_result) do
    quote do
      assert_top_level_function(unquote(expression), %{rows: [%{row: [unquote(expected_result)]}]})
      assert_subquery_function(unquote(expression), %{rows: [%{row: [unquote(expected_result)]}]})
    end
  end

  defmacrop assert_function_error(expression, expected_error) do
    quote do
      assert_top_level_function(unquote(expression), %{error: unquote(expected_error)})
      assert_subquery_function(unquote(expression), %{error: unquote(expected_error)})
    end
  end

  defmacrop assert_subquery_aggregate(expression, expected_result) do
    quote do
      assert_subquery_function(
        unquote(expression),
        "group by user_id", %{rows: [%{row: [unquote(expected_result)]}]}
      )
    end
  end

  test "detect missing group by in subqueries" do
    assert_subquery_function(
      "count(*)",
      %{error: "Column `user_id` from table `heights_ft` needs to appear in the `group by`" <> _}
    )
  end

  test "min(height)", do: assert_subquery_aggregate("min(height)", 180)
  test "max(height)", do: assert_subquery_aggregate("max(height)", 180)
  test "avg(height)", do: assert_subquery_aggregate("avg(height)", 180.0)
  test "sum(height)", do: assert_subquery_aggregate("sum(height)", 180)
  test "count(*)", do: assert_subquery_aggregate("count(*)", 1)
  test "count(height)", do: assert_subquery_aggregate("count(height)", 1)
  test "count(distinct height)", do: assert_subquery_aggregate("count(distinct height)", 1)

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
