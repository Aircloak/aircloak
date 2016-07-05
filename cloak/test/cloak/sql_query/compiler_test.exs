defmodule Cloak.SqlQuery.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Compiler
  alias Cloak.SqlQuery.Parser

  setup do
    {:ok, %{
      data_source: %{tables: %{table: %{
         columns: [{"column", :timestamp}]
      }}}
    }}
  end

  test "adds an empty group by", %{data_source: data_source} do
    assert %{group_by: []} = compile!("select * from table", data_source)
  end

  test "casts timestamp where conditions", %{data_source: data_source} do
    result = compile!("select * from table where column > '2015-01-01'", data_source)

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert result[:where] == [{:comparison, "column", :>, time}]
  end

  test "casts timestamp in `in` conditions", %{data_source: data_source} do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source)

    assert [{:in, "column", times}] = result[:where]
    assert Enum.sort(times) == [
      %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)},
      %Timex.DateTime{year: 2015, month: 1, day: 2, timezone: Timex.Timezone.get(:utc)},
    ]
  end

  test "casts timestamp in negated conditions", %{data_source: data_source} do
    result = compile!("select * from table where column <> '2015-01-01'", data_source)

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert result[:where_not] == [{:comparison, "column", :=, time}]
  end

  test "reports malformed timestamps", %{data_source: data_source} do
    assert {:error, "Cannot cast `something stupid` to timestamp."} =
      compile("select * from table where column > 'something stupid'", data_source)
  end

  for function <- ~w(avg min max sum stddev median) do
    test "rejecting #{function} on non-numerical columns", %{data_source: data_source} do
      assert {:error, "Aggregation function used over non-numeric column `column`."} =
        compile("select #{unquote(function)}(column) from table", data_source)
    end
  end

  for function <- ~w(count) do
    test "allowing #{function} on non-numerical columns", %{data_source: data_source} do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source)
    end
  end

  defp compile!(query_string, data_source) do
    {:ok, result} = compile(query_string, data_source)
    result
  end

  defp compile(query_string, data_source) do
    query = Parser.parse!(query_string)
    Compiler.compile(data_source, query)
  end
end
