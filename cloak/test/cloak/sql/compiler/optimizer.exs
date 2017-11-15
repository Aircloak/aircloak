defmodule Cloak.Sql.Compiler.Optimizer.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  test "unused columns selected in subquery are dropped" do
    assert %{from: {:subquery, %{ast: subquery}}} =
      compile!("SELECT COUNT(x.numeric) FROM (SELECT * FROM table) x", data_source())
    assert ["uid", "numeric"] = subquery.column_titles
  end

  test "unused columns selected in subquery with join are dropped" do
    assert %{from: {:subquery, %{ast: subquery}}} = compile!("""
      SELECT min(n) FROM (
        SELECT 1 AS a, t1.numeric AS n, t1.uid, t1.string, t2.* FROM
        table AS t1 JOIN table AS t2 ON t1.uid = t2.uid
      ) AS x
    """, data_source())
    assert ["n", "uid", "uid"] = subquery.column_titles
  end

  test "grouped by columns selected in subquery are kept" do
    assert %{from: {:subquery, %{ast: subquery}}} =
      compile!("SELECT COUNT(*) FROM (SELECT * FROM table) x GROUP BY numeric", data_source())
    assert ["uid", "numeric"] = subquery.column_titles
  end

  test "filtered columns selected in subquery are kept" do
    assert %{from: {:subquery, %{ast: subquery}}} =
      compile!("SELECT COUNT(*) FROM (SELECT * FROM table) x WHERE string = 'aa'", data_source())
    assert ["uid", "string"] = subquery.column_titles
  end

test "unused columns selected in joined subquery are dropped" do
  assert %{from: {:join, %{lhs: {:subquery, %{ast: subquery}}}}} = compile!("""
    SELECT avg(t1.numeric) FROM (SELECT * FROM table) AS t1 JOIN table AS t2 ON t1.uid = t2.uid
  """, data_source())
  assert ["uid", "numeric"] = subquery.column_titles
end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          columns: [
            Table.column("uid", :integer),
            Table.column("numeric", :integer),
            Table.column("string", :text),
          ]
        )
      }
    }
  end
end
