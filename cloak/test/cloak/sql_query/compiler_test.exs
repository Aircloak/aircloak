defmodule Cloak.SqlQuery.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Compiler
  alias Cloak.SqlQuery.Parser

  setup do
    {:ok, %{
      data_source: %{tables: %{
        table: %{
          columns: [{"column", :timestamp}, {"numeric", :integer}]
        },
        other_table: %{
          columns: [{"other_column", :timestamp}]
        }
      }}
    }}
  end

  test "adds an empty group by", %{data_source: data_source} do
    assert %{group_by: []} = compile!("select * from table", data_source)
  end

  test "casts timestamp where conditions", %{data_source: data_source} do
    result = compile!("select * from table where column > '2015-01-01'", data_source)

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert result[:where] == [{:comparison, {:qualified, "table", "column"}, :>, time}]
  end

  test "casts timestamp in `in` conditions", %{data_source: data_source} do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source)

    assert [{:in, {:qualified, "table", "column"}, times}] = result[:where]
    assert Enum.sort(times) == [
      %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)},
      %Timex.DateTime{year: 2015, month: 1, day: 2, timezone: Timex.Timezone.get(:utc)},
    ]
  end

  test "casts timestamp in negated conditions", %{data_source: data_source} do
    result = compile!("select * from table where column <> '2015-01-01'", data_source)

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert result[:where_not] == [{:comparison, {:qualified, "table", "column"}, :=, time}]
  end

  test "reports malformed timestamps", %{data_source: data_source} do
    assert {:error, "Cannot cast `something stupid` to timestamp."} =
      compile("select * from table where column > 'something stupid'", data_source)
  end

  for function <- ~w(avg min max sum stddev median) do
    test "rejecting #{function} on non-numerical columns", %{data_source: data_source} do
      assert {:error, "Aggregation function used over non-numeric column `column` from table `table`."} =
        compile("select #{unquote(function)}(column) from table", data_source)
    end
  end

  for function <- ~w(count) do
    test "allowing #{function} on non-numerical columns", %{data_source: data_source} do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source)
    end
  end

  test "rejecting outer where clause in queries unchecked sub-select", %{data_source: data_source} do
    assert {:error, "WHERE-clause in outer SELECT is not allowed in combination with a subquery"} =
      compile("SELECT a FROM (unchecked inner select) t WHERE a > 10", data_source)
  end

  test "rejecting missing column", %{data_source: data_source} do
    assert {:error, "Column `a` doesn't exist in table `table`."} =
      compile("SELECT a FROM table", data_source)
  end

  test "rejecting missing qualified column", %{data_source: data_source} do
    assert {:error, "Column `a` doesn't exist in table `table`."} =
      compile("SELECT table.a FROM table", data_source)
  end

  test "rejecting qualified SELECT from not selected table", %{data_source: data_source} do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT other_table.other_column FROM table", data_source)
  end

  test "rejecting qualified ORDER BY from not selected table", %{data_source: data_source} do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT column FROM table ORDER BY other_table.other_column", data_source)
  end

  test "rejecting qualified GROUP BY from not selected table", %{data_source: data_source} do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT column FROM table GROUP BY other_table.other_column", data_source)
  end

  test "rejecting qualified WHERE from not selected table", %{data_source: data_source} do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT column FROM table WHERE other_table.other_column <> ''", data_source)
  end

  Enum.each(["count", "min", "max", "median", "stddev"], fn(function) ->
    test "allows qualified identifiers in function calls (function #{function})", %{data_source: data_source} do
      result = compile!("select #{unquote(function)}(table.numeric) from table",
        data_source)
      assert result[:columns] == [
        {:function, unquote(function), {:qualified, "table", "numeric"}}
      ]
    end
  end)

  test "qualifies all identifiers", %{data_source: data_source} do
    result = compile!("""
        SELECT column, count(column)
        FROM table
        WHERE column > '2015-01-01' and column <> '2015-01-02'
        GROUP BY column
        ORDER BY count(column) DESC, count(table.column) DESC
      """,
      data_source)
    assert result[:columns] == [
      {:qualified, "table", "column"},
      {:function, "count", {:qualified, "table", "column"}}
    ]
    assert [{:comparison, {:qualified, "table", "column"}, :>, _}] = result[:where]
    assert [{:comparison, {:qualified, "table", "column"}, :=, _}] = result[:where_not]
    assert [{:qualified, "table", "column"}] = result[:unsafe_filter_columns]
    assert result[:group_by] == [{:qualified, "table", "column"}]
    assert result[:order_by] == [{1, :desc}, {1, :desc}]
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
