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

  defp data_source do
    %{
      name: "oracle_test_data_source",
      driver: Oracle,
      tables: %{
        table: Table.new("table", "uid", db_name: "table", columns: [Table.column("uid", :integer)])
      }
    }
  end
end
