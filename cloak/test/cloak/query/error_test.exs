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

  test "reports an error on collision between alias and column in order by" do
    assert_query "select count(height) as height from test_errors order by height", %{error: error}
    assert ~s/Usage of `height` is ambiguous./ == error
  end

  test "reports an error on collision between alias and column in where" do
    assert_query "select abs(height) as height from test_errors where height = 20", %{error: error}
    assert ~s/Usage of `height` is ambiguous./ == error
  end

  test "reports an error on collision between alias and column in group by" do
    assert_query "select abs(height) as height from test_errors group by height", %{error: error}
    assert ~s/Usage of `height` is ambiguous./ == error
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
    assert error == "Function `substring` requires arguments of type " <>
      "(`text`, `integer`, [`integer`]), but got (`text`)."
  end

  test "query reports error on invalid limit parameters" do
    assert_query "select name from test_errors limit -1", %{error: error}
    assert error =~ ~r/Expected `positive integer constant`.*/
  end

  test "query reports error on invalid offset parameters" do
    assert_query "select name from test_errors offset -1", %{error: error}
    assert error =~ ~r/Expected `non-negative integer constant`.*/
  end

  test "query reports error on invalid having clause" do
    assert_query "select name from test_errors group by name having height >= 100", %{error: error}
    assert ~s/`HAVING` clause can not be applied over column `height` from table `test_errors`./ == error
  end

  test "query reports error on invalid where clause" do
    assert_query "select name from test_errors where max(height) >= 100", %{error: error}
    assert ~s/Expression `max` is not valid in the `WHERE` clause./ == error
  end

  test "query reports error on cast in a where clause" do
    assert_query "select name from test_errors where cast(name as integer) >= 100", %{error: error}
    assert ~s/Column `cast` must be limited to a finite, nonempty range./ == error
  end

  test "query reports error on invalid group by position" do
    assert_query "select name from test_errors group by 0",
      %{error: "`GROUP BY` position `0` is out of the range of selected columns."}
    assert_query "select name from test_errors group by 2",
      %{error: "`GROUP BY` position `2` is out of the range of selected columns."}
  end
  test "non-integer constants are not allowed in group by" do
    assert_query "select name from test_errors group by 1.0",
      %{error: "Non-integer constant is not allowed in `GROUP BY`."}
  end

  test "query reports error on invalid order by position" do
    assert_query "select name from test_errors order by 0",
      %{error: "`ORDER BY` position `0` is out of the range of selected columns."}
    assert_query "select name from test_errors order by 2",
      %{error: "`ORDER BY` position `2` is out of the range of selected columns."}
  end
  test "non-integer constants are not allowed in order by" do
    assert_query "select name from test_errors order by 1.0",
      %{error: "Non-integer constant is not allowed in `ORDER BY`."}
  end
end
