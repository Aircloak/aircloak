defmodule Cloak.Query.ErrorTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("test_errors",
      "height INTEGER, name TEXT, male BOOLEAN, datetime TIMESTAMP, date_only DATE, time_only TIME"
    )
    :ok = Cloak.Test.DB.create_table("test_errors2", "height INTEGER")
  end

  test "query reports an error on invalid where clause identifier" do
    assert_query "select height from test_errors where nonexistant > 10", %{error: error}
    assert ~s/Column `nonexistant` doesn't exist in table `test_errors`./ == error
  end

  test "query reports an error on invalid order by field" do
    assert_query "select height from test_errors order by name", %{error: error}
    assert ~s/Non-selected column specified in `ORDER BY` clause./ == error
  end

  test "query reports an error on unknown function" do
    assert_query "select invalid_function(height) from test_errors", %{error: error}
    assert ~s/Unknown function `invalid_function`./ == error
  end

  test "reports an error on wrong cast" do
    assert_query "select * from test_errors where datetime > 0", %{error: error}
    assert ~s/Cannot cast `0` to datetime./ == error
  end

  test "reports an error on ambigous usage of an alias occurring multiple times" do
    assert_query "select count(*) as x, count(height) as x from test_errors order by x", %{error: error}
    assert ~s/Usage of `x` is ambiguous./ == error
  end

  test "query reports an error on invalid statement" do
    assert_query "invalid statement", %{error: "Expected `select or show` at line 1, column 1."}
  end

  test "query reports an error on invalid column" do
    assert_query "select invalid_column from test_errors", %{error: error}
    assert ~s/Column `invalid_column` doesn't exist in table `test_errors`./ == error
  end

  test "query reports an error on invalid table" do
    assert_query "select column from invalid_table", %{error: error}
    assert ~s/Table `invalid_table` doesn't exist./ == error
  end

  test "query reports an error when mixing aggregated and normal columns" do
    assert_query "select count(*), height from test_errors", %{error: error}
    assert error =~ ~r/`height` from table `test_errors` needs to appear in the `GROUP BY` clause/
  end

  test "query reports an error when grouping by nonexistent columns" do
    assert_query "select count(*) from test_errors group by nothing", %{error: error}
    assert error =~ ~r/Column `nothing` doesn't exist in table `test_errors`./
  end

  test "query reports an error when not grouping by some selected columns" do
    assert_query "select name, height from test_errors group by height", %{error: error}
    assert error =~ ~r/`name` from table `test_errors` needs to appear in the `GROUP BY` clause/
  end

  test "query reports an error on runner crash" do
    ExUnit.CaptureLog.capture_log(fn ->
      assert_query :invalid_query_type, %{error: "Unknown cloak error."}
    end)
  end

  test "substring with neither for nor from" do
    assert_query "select substring(name) from test_errors", %{error: error}
    assert error == "Function `substring` requires arguments of type (`text`, `integer`, [`integer`]),"
      <> " but got (`text`)"
  end

  test "query reports error on invalid limit / offset parameters" do
    assert_query "select name from test_errors limit -1", %{error: error}
    assert ~s/`LIMIT` clause expects a positive value./ == error
    assert_query "select name from test_errors offset -1", %{error: error}
    assert ~s/`OFFSET` clause expects a non-negative value./ == error
  end

  test "query reports error on invalid having clause" do
    assert_query "select name from test_errors group by name having height >= 100", %{error: error}
    assert ~s/`HAVING` clause can not be applied over column `height` from table `test_errors`./ == error
    assert_query "select name from test_errors having count(*) >= 10", %{error: error}
    assert ~s/Using the `HAVING` clause requires the `GROUP BY` clause to be specified./ == error
  end
end
