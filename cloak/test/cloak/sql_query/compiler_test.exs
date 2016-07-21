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
        },
        t1: %{
          columns: [{"c1", :integer}, {"c2", :integer}]
        },
        t2: %{
          columns: [{"c1", :integer}, {"c3", :integer}]
        },
        t3: %{
          name: "distinct_name",
          columns: [{"c1", :integer}]
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
    assert result[:where] == [{:comparison, {:identifier, "table", "column"}, :>, time}]
  end

  test "casts timestamp in `in` conditions", %{data_source: data_source} do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source)

    assert [{:in, {:identifier, "table", "column"}, times}] = result[:where]
    assert Enum.sort(times) == [
      %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)},
      %Timex.DateTime{year: 2015, month: 1, day: 2, timezone: Timex.Timezone.get(:utc)},
    ]
  end

  test "casts timestamp in negated conditions", %{data_source: data_source} do
    result = compile!("select * from table where column <> '2015-01-01'", data_source)

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert result[:where_not] == [{:comparison, {:identifier, "table", "column"}, :=, time}]
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

  test "rejecting missing column on join", %{data_source: data_source} do
    assert {:error, "Column `a` doesn't exist in any of the selected tables."} =
      compile("SELECT a FROM t1, t2", data_source)
  end

  test "rejecting missing qualified column", %{data_source: data_source} do
    assert {:error, "Column `a` doesn't exist in table `table`."} =
      compile("SELECT table.a FROM table", data_source)
  end

  test "rejecting qualified SELECT from not selected table", %{data_source: data_source} do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT other_table.other_column FROM table", data_source)
  end

  test "rejecting qualified SELECT from not selected table when join", %{data_source: data_source} do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT other_table.other_column FROM t1, t2", data_source)
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
      assert %{columns: [{:function, unquote(function), {:identifier, "table", "numeric"}}]} =
        compile!("select #{unquote(function)}(table.numeric) from table", data_source)
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
      {:identifier, "table", "column"},
      {:function, "count", {:identifier, "table", "column"}}
    ]
    assert [{:comparison, {:identifier, "table", "column"}, :>, _}] = result[:where]
    assert [{:comparison, {:identifier, "table", "column"}, :=, _}] = result[:where_not]
    assert [{:identifier, "table", "column"}] = result[:unsafe_filter_columns]
    assert result[:group_by] == [{:identifier, "table", "column"}]
    assert result[:order_by] == [{1, :desc}, {1, :desc}]
  end

  test "complains when tables don't exist", %{data_source: data_source} do
    assert {:error, "Table `t_doesnt_exist` doesn't exist."} =
      compile("SELECT c1 FROM t1, t_doesnt_exist", data_source)
  end

  test "expands all columns for all tables when cross joining", %{data_source: data_source} do
    result = compile!("SELECT * FROM t1, t2, t3", data_source)
    assert result[:columns] == [
      {:identifier, "t1", "c1"},
      {:identifier, "t1", "c2"},
      {:identifier, "t2", "c1"},
      {:identifier, "t2", "c3"},
      {:identifier, "t3", "c1"}
    ]
  end

  test "complains when an unqualified identifier cannot be pinned down", %{data_source: data_source} do
    assert {:error, "Column `c1` is ambiguous."} =
      compile("SELECT c1 FROM t1, t2", data_source)
  end

  test "allows for where, order by and group by when performing cross join", %{data_source: data_source} do
    result = compile!("""
        SELECT t1.c1
        FROM t1, t2
        WHERE c2 > 10
        GROUP BY t1.c1, c3
        ORDER BY t1.c1 DESC
      """,
      data_source)
    assert result[:columns] == [{:identifier, "t1", "c1"}]
    assert [{:comparison, {:identifier, "t1", "c2"}, :>, _}] = result[:where]
    assert result[:group_by] == [{:identifier, "t1", "c1"}, {:identifier, "t2", "c3"}]
    assert result[:order_by] == [{0, :desc}]
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
