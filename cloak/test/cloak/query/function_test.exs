defmodule Cloak.Query.FunctionTest do
  # These tests avoid constant expressions (like 2 + 3), because they are normalized away (to 5 in this case) -
  # see lib/cloak/sql/compiler/normalization.ex

  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    Cloak.Test.DB.create_table("heights_ft", "height INTEGER, name TEXT, string_number TEXT")
    insert_rows(_user_ids = 1..50, "heights_ft", ["height", "string_number", "name"], [180, "42", "first second third"])
    insert_rows(_user_ids = 1..50, "heights_ft", ["height", "string_number", "name"], [180, "42", "first second third"])

    Cloak.Test.DB.create_table("datetimes_ft", "datetime TIMESTAMP, date_only DATE, time_only TIME, empty text")
    insert_rows(_user_ids = 1..10, "datetimes_ft", ["datetime"], [~N[2015-02-03 04:05:06.000000]])

    Cloak.Test.DB.create_table("types_ft", "fixed DECIMAL(10, 5), frac REAL, num INTEGER, string TEXT, string2 TEXT")
  end

  setup do
    Cloak.Test.DB.clear_table("types_ft")
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

  test "detect unknown function" do
    assert_query(
      "select foo(height) from heights_ft",
      %{error: "Unknown function `foo`."}
    )
  end

  test "detect unknown function in subqueries" do
    assert_subquery_function(
      "foo(height)",
      "heights_ft",
      %{error: "Unknown function `foo`."}
    )
  end

  describe "extract_words" do
    test "basic usage" do
      assert_query("SELECT extract_words(name), count(*) FROM heights_ft GROUP BY 1", %{rows: [
        %{row: ["first", 100]},
        %{row: ["second", 100]},
        %{row: ["third", 100]},
      ]})
    end

    test "forbidden in subquery" do
      assert_query("SELECT count(*) FROM (SELECT user_id, extract_words(name) FROM heights_ft) x",
        %{error: "Function `extract_words` is not allowed in subqueries."})
    end

    test "extract_words surrounded by aggregate function" do
      assert_query(
        "SELECT avg(length(extract_words(name))) FROM heights_ft",
        %{rows: [%{occurrences: 1, row: [average]}]}
      )
      assert_in_delta 5.3333, average, 0.0001
    end

    test "extract_words alone splits words" do
      assert_query(
        "SELECT extract_words(name) as word FROM heights_ft",
        %{rows: [
          %{row: ["first"], occurrences: 100},
          %{row: ["second"], occurrences: 100},
          %{row: ["third"], occurrences: 100},
        ]}
      )
    end

    test "extract_words can have functions inside it" do
      assert_query(
        "SELECT extract_words(cast(height as text)) as word FROM heights_ft",
        %{rows: [
          %{row: ["180"], occurrences: 100},
        ]}
      )
    end

    test "extract_words can have functions outside it" do
      assert_query(
        "SELECT length(extract_words(name)) as word_length FROM heights_ft",
        %{rows: [
          %{row: [5], occurrences: 200},
          %{row: [6], occurrences: 100},
        ]}
      )
    end

    test "extract_words can be used multiple times at the same level" do
      assert_query("""
        SELECT
          concat(
            extract_words(word1),
            extract_words(word2)
          ) as pairs
        FROM (SELECT user_id, name AS word1, name AS word2 FROM heights_ft) AS t
        """,
        %{rows: [
          %{row: ["firstfirst"], occurrences: 100},
          %{row: ["firstsecond"], occurrences: 100},
          %{row: ["firstthird"], occurrences: 100},
          %{row: ["secondfirst"], occurrences: 100},
          %{row: ["secondsecond"], occurrences: 100},
          %{row: ["secondthird"], occurrences: 100},
          %{row: ["thirdfirst"], occurrences: 100},
          %{row: ["thirdsecond"], occurrences: 100},
          %{row: ["thirdthird"], occurrences: 100},
        ]}
      )
    end

    test "extract_words can be used multiple times in the same query" do
      assert_query("""
        SELECT
          extract_words(word1),
          extract_words(word2)
        FROM (SELECT user_id, name AS word1, name AS word2 FROM heights_ft) AS t
        """,
        %{rows: [
          %{row: ["first", "first"], occurrences: 100},
          %{row: ["first", "second"], occurrences: 100},
          %{row: ["first", "third"], occurrences: 100},
          %{row: ["second", "first"], occurrences: 100},
          %{row: ["second", "second"], occurrences: 100},
          %{row: ["second", "third"], occurrences: 100},
          %{row: ["third", "first"], occurrences: 100},
          %{row: ["third", "second"], occurrences: 100},
          %{row: ["third", "third"], occurrences: 100},
        ]}
      )
    end

    test "extract_words in where clause" do
      assert_query("""
        SELECT extract_words(name)
        FROM heights_ft
        WHERE extract_words(name) IN ('first', 'third')
        """,
        %{rows: [
          %{row: ["first"], occurrences: 100},
          %{row: ["third"], occurrences: 100},
        ]}
      )
    end

    test "invalid extract_words usage in where clause" do
      assert_query("SELECT extract_words(name) FROM heights_ft WHERE extract_words(string_number) = 'first'",
        %{error: "Row splitter functions used in the `WHERE`-clause"
          <> " have to be used identically in the `SELECT`-clause first."})
    end

    test "invalid extract_words argument" do
      assert_query("SELECT extract_words('constant') FROM heights_ft",
        %{error: "A constant is not allowed as the first argument of the function `extract_words`."})
    end
  end

  test "min(height)", do: assert_subquery_aggregate("min(height)", "heights_ft", 180)
  test "max(height)", do: assert_subquery_aggregate("max(height)", "heights_ft", 180)
  test "min(datetime)", do: assert_subquery_aggregate("min(datetime)", "datetimes_ft", "2015-02-03T04:05:06.000000")
  test "max(datetime)", do: assert_subquery_aggregate("max(datetime)", "datetimes_ft", "2015-02-03T04:05:06.000000")
  test "avg(height)", do: assert_subquery_aggregate("avg(height)", "heights_ft", 180.0)
  test "sum(height)", do: assert_subquery_aggregate("sum(height)", "heights_ft", 360)
  test "median(height)", do: assert_subquery_aggregate("median(height)", "heights_ft", 180)
  test "stddev(height)", do: assert_subquery_aggregate("stddev(height)", "heights_ft", 0.0)
  test "count(*)", do: assert_subquery_aggregate("count(*)", "heights_ft", 2)
  test "count(height)", do: assert_subquery_aggregate("count(height)", "heights_ft", 2)
  test "count(distinct height)", do: assert_subquery_aggregate("count(distinct height)", "heights_ft", 1)

  test "+", do: assert 181 == apply_function("height + 1", "heights_ft")
  test "-", do: assert 179 == apply_function("height - 1", "heights_ft")
  test "*", do: assert 360 == apply_function("height * 2", "heights_ft")
  test "/", do: assert 1 == apply_function("height / height", "heights_ft")
  test "^", do: assert 32_400.0 == apply_function("height ^ 2", "heights_ft")
  test "%", do: assert 3 == apply_function("10 % 7", "heights_ft")

  test "trunc/1", do: assert 180 == apply_function("trunc", ["frac"], [180.6], "types_ft")
  test "trunc/2", do: assert 180.12 == apply_function("trunc", ["frac", "num"], [180.126, 2], "types_ft")
  test "trunc/2 on DECIMAL", do: assert 180.12 == apply_function("trunc", ["fixed", "num"], [180.126, 2], "types_ft")
  test "round/1", do: assert 181 == apply_function("round", ["frac"], [180.6], "types_ft")
  test "round/2", do: assert 180.13 == apply_function("round", ["frac", "num"], [180.126, 2], "types_ft")
  test "round/2 on DECIMAL", do: assert 180.13 == apply_function("round", ["fixed", "num"], [180.126, 2], "types_ft")
  test "abs", do: assert 180 == apply_function("abs", ["num"], [-180], "types_ft")
  test "sqrt", do: assert 3.0 == apply_function("sqrt", ["num"], [9], "types_ft")
  test "div", do: assert 1 == apply_function("div(height, height)", "heights_ft")
  test "mod", do: assert 80 == apply_function("mod(height, 100)", "heights_ft")
  test "pow", do: assert 32_400.0 == apply_function("pow(height, 2)", "heights_ft")
  test "floor", do: assert 180 == apply_function("floor", ["frac"], [180.9], "types_ft")
  test "ceil", do: assert 181 == apply_function("ceil", ["frac"], [180.1], "types_ft")
  test "ceiling", do: assert 181 == apply_function("ceiling", ["frac"], [180.1], "types_ft")
  test "bucket", do: assert 150 == apply_function("bucket(height by 50)", "heights_ft")
  test "bucket lower", do: assert 150 == apply_function("bucket(height by 50 align lower)", "heights_ft")
  test "bucket upper", do: assert 200 == apply_function("bucket(height by 50 align upper)", "heights_ft")
  test "bucket middle", do: assert 175 == apply_function("bucket(height by 50 align middle)", "heights_ft")

  test "cast as integer", do: assert 42 == apply_function("cast(string_number AS integer)", "heights_ft")
  test "cast as real", do: assert 42.0 == apply_function("cast(string_number AS real)", "heights_ft")
  test "cast as text", do: assert "180" == apply_function("cast(height AS text)", "heights_ft")

  test "year", do: assert 2015 == apply_function("year(datetime)", "datetimes_ft")
  test "month", do: assert 2 == apply_function("month(datetime)", "datetimes_ft")
  test "day", do: assert 3 == apply_function("day(datetime)", "datetimes_ft")
  test "hour", do: assert 4 == apply_function("hour(datetime)", "datetimes_ft")
  test "minute", do: assert 5 == apply_function("minute(datetime)", "datetimes_ft")
  test "second", do: assert 6 == apply_function("second(datetime)", "datetimes_ft")
  test "quarter", do: assert 1 == apply_function("quarter(datetime)", "datetimes_ft")
  test "weekday", do: assert 2 == apply_function("weekday(datetime)", "datetimes_ft")

  test "extract(year)", do: assert 2015 == apply_function("extract(year from datetime)", "datetimes_ft")
  test "extract(month)", do: assert 2 == apply_function("extract(month from datetime)", "datetimes_ft")
  test "extract(day)", do: assert 3 == apply_function("extract(day from datetime)", "datetimes_ft")
  test "extract(hour)", do: assert 4 == apply_function("extract(hour from datetime)", "datetimes_ft")
  test "extract(minute)", do: assert 5 == apply_function("extract(minute from datetime)", "datetimes_ft")
  test "extract(second)", do: assert 6 == apply_function("extract(second from datetime)", "datetimes_ft")

  describe "date_trunc for datetime" do
    test "date_trunc('second')", do:
      assert "2015-02-03T04:05:06.000000" == apply_function("date_trunc('second', datetime)", "datetimes_ft")
    test "date_trunc('minute')", do:
      assert "2015-02-03T04:05:00.000000" == apply_function("date_trunc('minute', datetime)", "datetimes_ft")
    test "date_trunc('hour')", do:
      assert "2015-02-03T04:00:00.000000" == apply_function("date_trunc('hour', datetime)", "datetimes_ft")
    test "date_trunc('day')", do:
      assert "2015-02-03T00:00:00.000000" == apply_function("date_trunc('day', datetime)", "datetimes_ft")
    test "date_trunc('month')", do:
      assert "2015-02-01T00:00:00.000000" == apply_function("date_trunc('month', datetime)", "datetimes_ft")
    test "date_trunc('quarter')", do:
      assert "2015-01-01T00:00:00.000000" == apply_function("date_trunc('quarter', datetime)", "datetimes_ft")
    test "date_trunc('year')", do:
      assert "2015-01-01T00:00:00.000000" == apply_function("date_trunc('year', datetime)", "datetimes_ft")
  end

  describe "date_trunc for time" do
    test "date_trunc('second')", do:
      assert "04:05:06.000000" == apply_function("date_trunc('second', cast(datetime as time))", "datetimes_ft")
    test "date_trunc('minute')", do:
      assert "04:05:00.000000" == apply_function("date_trunc('minute', cast(datetime as time))", "datetimes_ft")
    test "date_trunc('hour')", do:
      assert "04:00:00.000000" == apply_function("date_trunc('hour', cast(datetime as time))", "datetimes_ft")
    test "date_trunc('day')", do:
      assert "00:00:00.000000" == apply_function("date_trunc('day', cast(datetime as time))", "datetimes_ft")
    test "date_trunc('quarter')", do:
      assert "00:00:00.000000" == apply_function("date_trunc('quarter', cast(datetime as time))", "datetimes_ft")
  end

  test "length", do: assert 3 == apply_function("length(cast(height as text))", "heights_ft")
  test "lower", do: assert "abc" == apply_function("lower", ["string"], ["AbC"], "types_ft")
  test "lcase", do: assert "abc" == apply_function("lcase", ["string"], ["AbC"], "types_ft")
  test "upper", do: assert "ABC" == apply_function("upper", ["string"], ["AbC"], "types_ft")
  test "ucase", do: assert "ABC" == apply_function("ucase", ["string"], ["AbC"], "types_ft")
  test "left", do: assert "A" == apply_function("left", ["string", "num"], ["AbC", 1], "types_ft")
  test "right", do: assert "C" == apply_function("right", ["string", "num"], ["AbC", 1], "types_ft")
  test "ltrim/1", do: assert "AbC" == apply_function("ltrim", ["string"], [" AbC"], "types_ft")
  test "ltrim/2", do: assert "bC" == apply_function("ltrim", ["string", "string2"], ["AbC", "A"], "types_ft")
  test "rtrim/1", do: assert "AbC" == apply_function("rtrim", ["string"], ["AbC "], "types_ft")
  test "rtrim/2", do: assert "Ab" == apply_function("rtrim", ["string", "string2"], ["AbC", "C"], "types_ft")
  test "btrim/1", do: assert "AbC" == apply_function("btrim", ["string"], [" AbC "], "types_ft")
  test "btrim/2", do: assert "b" == apply_function("btrim", ["string", "string2"], ["AbC", "AC"], "types_ft")
  test "trim (1)", do: assert "AbC" == apply_function("trim", ["string"], [" AbC "], "types_ft")
  test "trim (2)", do: assert "rst second third" == apply_function("trim(leading 'fist' from name)", "heights_ft")
  test "trim (3)", do: assert " second " == apply_function("trim('frsthid' name)", "heights_ft")
  test "substring from", do: assert "irst second third" == apply_function("substring(name from 2)", "heights_ft")
  test "substring for", do: assert "fi" == apply_function("substring(name for 2)", "heights_ft")
  test "substring from for", do: assert "i" == apply_function("substring(name from 2 for 1)", "heights_ft")
  test "concat", do: assert "first second third fourth" == apply_function("concat(name, ' fourth')", "heights_ft")
  test "||", do: assert "first second thirdbc" == apply_function("name || 'b' || 'c'", "heights_ft")

  defp apply_function(function_name, arg_columns, arg_values, table) do
    insert_rows(_user_ids = 1..5, table, arg_columns, arg_values)
    apply_function("#{function_name}(#{Enum.join(arg_columns, ",")})", table)
  end

  defp apply_function(sql_fragment, table_name) do
    assert_query(
      "select sql_res from (select user_id, (#{sql_fragment}) as sql_res from #{table_name}) alias",
      %{rows: [%{row: [result_subquery]}]}
    )

    result_simple_query = apply_elixir_function(sql_fragment, table_name)
    assert result_simple_query == result_subquery
    result_simple_query
  end

  defp apply_elixir_function(sql_fragment, table_name) do
    assert_query(
      "select (#{sql_fragment}) as elixir_res from #{table_name}",
      %{rows: [%{row: [result_simple_query]}]}
    )
    result_simple_query
  end
end
