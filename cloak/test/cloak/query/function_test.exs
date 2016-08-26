defmodule Cloak.Query.FunctionTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    Cloak.Test.DB.create_table("heights_ft", "height INTEGER, name TEXT")
    insert_rows(_user_ids = 1..100, "heights_ft", ["height"], [180])

    Cloak.Test.DB.create_table("datetimes_ft", "datetime TIMESTAMP, date_only DATE, time_only TIME")
    insert_rows(_user_ids = 1..10, "datetimes_ft", ["datetime"],
      [%Postgrex.Timestamp{year: 2015, month: 1, day: 2, hour: 3, min: 4, sec: 5}])
  end

  defmacrop assert_top_level_function(expression, table, expected_match) do
    quote do
      assert_query(
        "select (#{unquote(expression)}) as elixir_res from #{unquote(table)}",
        unquote(expected_match)
      )
    end
  end

  defmacrop assert_subquery_function(expression, table, subquery_postfix \\ "", expected_match) do
    quote do
      assert_query(
        "select sql_res from (
          select user_id, (#{unquote(expression)}) as sql_res from #{unquote(table)} #{unquote(subquery_postfix)}
        ) alias",
        unquote(expected_match)
      )
    end
  end

  defmacrop assert_function_success(expression, table, expected_result) do
    quote do
      assert_top_level_function(unquote(expression), unquote(table), %{rows: [%{row: [unquote(expected_result)]}]})
      assert_subquery_function(unquote(expression), unquote(table), %{rows: [%{row: [unquote(expected_result)]}]})
    end
  end

  defmacrop assert_function_error(expression, table, expected_error) do
    quote do
      assert_top_level_function(unquote(expression), unquote(table), %{error: unquote(expected_error)})
      assert_subquery_function(unquote(expression), unquote(table), %{error: unquote(expected_error)})
    end
  end

  defmacrop assert_subquery_aggregate(expression, table, expected_result) do
    quote do
      assert_subquery_function(
        unquote(expression),
        unquote(table),
        "group by user_id", %{rows: [%{row: [unquote(expected_result)]}]}
      )
    end
  end

  test "detect missing group by in subqueries" do
    assert_subquery_function(
      "count(*)",
      "heights_ft",
      %{error: "Column `user_id` from table `heights_ft` needs to appear in the `group by`" <> _}
    )
  end

  test "min(height)", do: assert_subquery_aggregate("min(height)", "heights_ft", 180)
  test "max(height)", do: assert_subquery_aggregate("max(height)", "heights_ft", 180)
  test "avg(height)", do: assert_subquery_aggregate("avg(height)", "heights_ft", 180.0)
  test "sum(height)", do: assert_subquery_aggregate("sum(height)", "heights_ft", 180)
  test "count(*)", do: assert_subquery_aggregate("count(*)", "heights_ft", 1)
  test "count(height)", do: assert_subquery_aggregate("count(height)", "heights_ft", 1)
  test "count(distinct height)", do: assert_subquery_aggregate("count(distinct height)", "heights_ft", 1)

  test "+", do: assert_function_success("height + 1", "heights_ft", 181)
  test "-", do: assert_function_success("height - 1", "heights_ft", 179)
  test "*", do: assert_function_success("height * 2", "heights_ft", 360)
  test "/", do: assert_function_success("height / 2", "heights_ft", 90.0)
  test "^", do: assert_function_success("height ^ 2", "heights_ft", 32_400.0)

  test "trunc/1", do: assert_function_success("trunc(height + 0.6)", "heights_ft", 180)
  test "trunc/2", do: assert_function_success("trunc(height + 0.126, 2)", "heights_ft", 180.12)
  test "round/1", do: assert_function_success("round(height + 0.6)", "heights_ft", 181)
  test "round/2", do: assert_function_success("round(height + 0.126, 2)", "heights_ft", 180.13)
  test "abs", do: assert_function_success("abs(height * -1)", "heights_ft", 180)
  test "sqrt", do: assert_function_success("sqrt(height/20)", "heights_ft", 3.0)

  test "cast as integer", do: assert_function_success("cast('42' AS integer)", "heights_ft", 42)
  test "cast as real", do: assert_function_success("cast('42' AS real)", "heights_ft", 42.0)
  test "cast as text", do: assert_function_success("cast(42 AS text)", "heights_ft", "42")

  test "year", do: assert_function_success("year(datetime)", "datetimes_ft", 2015)
  test "month", do: assert_function_success("month(datetime)", "datetimes_ft", 1)
  test "day", do: assert_function_success("day(datetime)", "datetimes_ft", 2)
  test "hour", do: assert_function_success("hour(datetime)", "datetimes_ft", 3)
  test "minute", do: assert_function_success("minute(datetime)", "datetimes_ft", 4)
  test "second", do: assert_function_success("second(datetime)", "datetimes_ft", 5)

  test "extract(year)", do: assert_function_success("extract(year from datetime)", "datetimes_ft", 2015)
  test "extract(month)", do: assert_function_success("extract(month from datetime)", "datetimes_ft", 1)
  test "extract(day)", do: assert_function_success("extract(day from datetime)", "datetimes_ft", 2)
  test "extract(hour)", do: assert_function_success("extract(hour from datetime)", "datetimes_ft", 3)
  test "extract(minute)", do: assert_function_success("extract(minute from datetime)", "datetimes_ft", 4)
  test "extract(second)", do: assert_function_success("extract(second from datetime)", "datetimes_ft", 5)

  test "type errors", do: assert_function_error(
    "trunc('foobar')",
    "heights_ft",
    "Function `trunc` requires arguments of type" <> _
  )
end
