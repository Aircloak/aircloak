defmodule Cloak.Query.FunctionTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    Cloak.Test.DB.create_table("heights_ft", "height INTEGER, name TEXT")
    insert_rows(_user_ids = 1..100, "heights_ft", ["height"], [180])

    Cloak.Test.DB.create_table("datetimes_ft", "datetime TIMESTAMP, date_only DATE, time_only TIME")
    insert_rows(_user_ids = 1..10, "datetimes_ft", ["datetime"], [~N[2015-01-02 03:04:05.000000]])
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
      %{error: "Column `user_id` from table `heights_ft` needs to appear in the `GROUP BY`" <> _}
    )
  end

  test "min(height)", do: assert_subquery_aggregate("min(height)", "heights_ft", 180)
  test "max(height)", do: assert_subquery_aggregate("max(height)", "heights_ft", 180)
  test "min(datetime)", do: assert_subquery_aggregate("min(datetime)", "datetimes_ft", ~N[2015-01-02 03:04:05.000000])
  test "max(datetime)", do: assert_subquery_aggregate("max(datetime)", "datetimes_ft", ~N[2015-01-02 03:04:05.000000])
  test "avg(height)", do: assert_subquery_aggregate("avg(height)", "heights_ft", 180.0)
  test "sum(height)", do: assert_subquery_aggregate("sum(height)", "heights_ft", 180)
  test "count(*)", do: assert_subquery_aggregate("count(*)", "heights_ft", 1)
  test "count(height)", do: assert_subquery_aggregate("count(height)", "heights_ft", 1)
  test "count(distinct height)", do: assert_subquery_aggregate("count(distinct height)", "heights_ft", 1)

  test "+", do: assert 181 == apply_function("height + 1", "heights_ft")
  test "-", do: assert 179 == apply_function("height - 1", "heights_ft")
  test "*", do: assert 360 == apply_function("height * 2", "heights_ft")
  test "/", do: assert 22.5 == apply_function("height / 8", "heights_ft")
  test "^", do: assert 32_400.0 == apply_function("height ^ 2", "heights_ft")

  test "trunc/1", do: assert 180 == apply_function("trunc(height + 0.6)", "heights_ft")
  test "trunc/2", do: assert 180.12 == apply_function("trunc(height + 0.126, 2)", "heights_ft")
  test "round/1", do: assert 181 == apply_function("round(height + 0.6)", "heights_ft")
  test "round/2", do: assert 180.13 == apply_function("round(height + 0.126, 2)", "heights_ft")
  test "abs", do: assert 180 == apply_function("abs(height * -1)", "heights_ft")
  test "sqrt", do: assert 3.0 == apply_function("sqrt(height/20)", "heights_ft")
  test "div", do: assert 1 == apply_function("div(height, 100)", "heights_ft")
  test "mod", do: assert 80 == apply_function("mod(height, 100)", "heights_ft")
  test "pow", do: assert 32_400.0 == apply_function("pow(height, 2)", "heights_ft")
  test "floor", do: assert 180 == apply_function("floor(height + 0.9)", "heights_ft")
  test "ceil", do: assert 181 == apply_function("ceil(height + 0.1)", "heights_ft")
  test "ceiling", do: assert 181 == apply_function("ceiling(height + 0.1)", "heights_ft")
  test "bucket", do: assert 150 == apply_function("bucket(height, 50)", "heights_ft")

  test "cast as integer", do: assert 42 == apply_function("cast('42' AS integer)", "heights_ft")
  test "cast as real", do: assert 42.0 == apply_function("cast('42' AS real)", "heights_ft")
  test "cast as text", do: assert "42" == apply_function("cast(42 AS text)", "heights_ft")

  test "year", do: assert 2015 == apply_function("year(datetime)", "datetimes_ft")
  test "month", do: assert 1 == apply_function("month(datetime)", "datetimes_ft")
  test "day", do: assert 2 == apply_function("day(datetime)", "datetimes_ft")
  test "hour", do: assert 3 == apply_function("hour(datetime)", "datetimes_ft")
  test "minute", do: assert 4 == apply_function("minute(datetime)", "datetimes_ft")
  test "second", do: assert 5 == apply_function("second(datetime)", "datetimes_ft")

  test "extract(year)", do: assert 2015 == apply_function("extract(year from datetime)", "datetimes_ft")
  test "extract(month)", do: assert 1 == apply_function("extract(month from datetime)", "datetimes_ft")
  test "extract(day)", do: assert 2 == apply_function("extract(day from datetime)", "datetimes_ft")
  test "extract(hour)", do: assert 3 == apply_function("extract(hour from datetime)", "datetimes_ft")
  test "extract(minute)", do: assert 4 == apply_function("extract(minute from datetime)", "datetimes_ft")
  test "extract(second)", do: assert 5 == apply_function("extract(second from datetime)", "datetimes_ft")

  test "length", do: assert 3 == apply_function("length(cast(height as text))", "heights_ft")
  test "lower", do: assert "abc" == apply_function("lower('AbC')", "heights_ft")
  test "lcase", do: assert "abc" == apply_function("lcase('AbC')", "heights_ft")
  test "upper", do: assert "ABC" == apply_function("upper('AbC')", "heights_ft")
  test "ucase", do: assert "ABC" == apply_function("ucase('AbC')", "heights_ft")
  test "left", do: assert "A" == apply_function("left('AbC', 1)", "heights_ft")
  test "right", do: assert "C" == apply_function("right('AbC', 1)", "heights_ft")
  test "ltrim/1", do: assert "AbC" == apply_function("ltrim(' AbC')", "heights_ft")
  test "ltrim/2", do: assert "bC" == apply_function("ltrim('AbC', 'A')", "heights_ft")
  test "rtrim/1", do: assert "AbC" == apply_function("rtrim('AbC ')", "heights_ft")
  test "rtrim/2", do: assert "Ab" == apply_function("rtrim('AbC', 'C')", "heights_ft")
  test "btrim/1", do: assert "AbC" == apply_function("btrim(' AbC ')", "heights_ft")
  test "btrim/2", do: assert "b" == apply_function("btrim('AbC', 'AC')", "heights_ft")
  test "substring from", do: assert "bC" == apply_function("substring('AbC' from 2)", "heights_ft")
  test "substring for", do: assert "Ab" == apply_function("substring('AbC' for 2)", "heights_ft")
  test "substring from for", do: assert "b" == apply_function("substring('AbC' from 2 for 1)", "heights_ft")
  test "concat", do: assert "Ab" == apply_function("concat('A', 'b')", "heights_ft")
  test "||", do: assert "Abc" == apply_function("'A' || 'b' || 'c'", "heights_ft")

  defp apply_function(sql_fragment, table_name) do
    assert_query(
      "select (#{sql_fragment}) as elixir_res from #{table_name}",
      %{rows: [%{row: [result_simple_query]}]}
    )

    assert_query(
      "select sql_res from (select user_id, (#{sql_fragment}) as sql_res from #{table_name}) alias",
      %{rows: [%{row: [result_subquery]}]}
    )

    assert result_simple_query == result_subquery
    result_simple_query
  end
end
