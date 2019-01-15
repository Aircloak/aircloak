defmodule Cloak.DataSource.Oracle.Test do
  use ExUnit.Case

  alias Cloak.DataSource.{Oracle, Table}
  import Cloak.Test.QueryHelpers

  test "queries without limit and offset are supported" do
    query = compile!("SELECT * FROM table ORDER BY uid", data_source())
    assert Oracle.supports_query?(query)
  end

  test "limit is unsupported" do
    query = compile!("SELECT * FROM table ORDER BY uid LIMIT 10", data_source())
    refute Oracle.supports_query?(query)
  end

  test "offset is not supported" do
    query = compile!("SELECT * FROM table ORDER BY uid OFFSET 10", data_source())
    refute Oracle.supports_query?(query)
  end

  describe "interval_mapper/1" do
    test "positive intervals" do
      assert Oracle.interval_mapper("+000000000 00:00:00.000000000") |> Timex.Duration.to_clock() == {0, 0, 0, 0}
      assert Oracle.interval_mapper("+000000001 02:03:04.000000000") |> Timex.Duration.to_clock() == {26, 3, 4, 0}
    end

    test "negative intervals" do
      assert Oracle.interval_mapper("-000000000 00:00:00.000000000") |> Timex.Duration.to_clock() == {0, 0, 0, 0}
      assert Oracle.interval_mapper("-000000001 02:03:04.000000000") |> Timex.Duration.to_clock() == {-26, -3, -4, 0}
    end

    test "fractions of a second are ignored",
      do: assert(Oracle.interval_mapper("-000000000 00:00:00.100000000") |> Timex.Duration.to_clock() == {0, 0, 0, 0})

    test "error is converted into a nil", do: assert(is_nil(Oracle.interval_mapper("invalid string")))
  end

  describe "nil_mapper/1" do
    test "doesn't invoke the inner mapper if the value is nil" do
      assert Oracle.nil_mapper(fn _ -> flunk("shouldn't happen") end).(nil) == nil
    end

    test "invokes the inner mapper if the value is not nil" do
      assert Oracle.nil_mapper(fn value -> {:invoked, value} end).(:value) == {:invoked, :value}
    end
  end

  describe "table_parts" do
    test "unqualified single part", do: assert(Oracle.table_parts("foo") == ["foo"])
    test "unqualified with whitespace", do: assert(Oracle.table_parts("foo bar baz") == ["foo bar baz"])
    test "unqualified multi parts", do: assert(Oracle.table_parts("foo.bar.baz") == ["foo", "bar", "baz"])

    test "qualified single part", do: assert(Oracle.table_parts(~s/"foo"/) == ["foo"])
    test "qualified with quotes in the name", do: assert(Oracle.table_parts(~s/"f""oo"/) == [~s(f"oo)])
    test "qualified multi part", do: assert(Oracle.table_parts(~s/"foo"."bar"."baz"/) == ["foo", "bar", "baz"])

    test "mixed multi part", do: assert(Oracle.table_parts(~s/"foo".bar."baz"/) == ["foo", "bar", "baz"])

    test "empty parts are not allowed" do
      assert_raise ArgumentError, fn -> Oracle.table_parts("") end
      assert_raise ArgumentError, fn -> Oracle.table_parts(".") end
      assert_raise ArgumentError, fn -> Oracle.table_parts("foo.") end
      assert_raise ArgumentError, fn -> Oracle.table_parts(".foo") end
      assert_raise ArgumentError, fn -> Oracle.table_parts("foo..bar") end
    end

    test "non-matching quotes are not allowed",
      do: assert_raise(ArgumentError, fn -> Oracle.table_parts(~s/"foobar/) end)
  end

  describe "fix_column_types_filter" do
    test "single part" do
      assert Oracle.fix_column_types_filter(%{db_name: "foo"}, "X = 1") == "X = 1 AND TABLE_NAME = 'foo'"
    end

    test "two parts" do
      assert Oracle.fix_column_types_filter(%{db_name: "foo.bar"}, "X = 1") ==
               "X = 1 AND OWNER = 'foo' AND TABLE_NAME = 'bar'"
    end
  end

  def data_source do
    %{
      name: "oracle_test_data_source",
      driver: Oracle,
      tables: %{
        table: Table.new("table", "uid", db_name: "table", columns: [Table.column("uid", :integer)])
      }
    }
  end
end
