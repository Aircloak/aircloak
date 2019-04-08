defmodule Cloak.DataSource.Drill.Test do
  use ExUnit.Case

  alias Cloak.DataSource.{Drill, Table}
  import Cloak.Test.QueryHelpers

  test "CROSS JOIN is not supported" do
    query =
      compile!(
        """
          SELECT MEDIAN(a.uid) FROM
          table AS a
            CROSS JOIN table AS b
            JOIN table AS c ON a.uid = c.uid
            WHERE a.uid = b.uid
        """,
        data_source()
      )

    refute Drill.supports_query?(query)
  end

  test "date_trunc('quarter') is not supported" do
    query = compile!("SELECT COUNT(*) FROM table WHERE date_trunc('quarter', date) = date '2018-01-01'", data_source())
    refute Drill.supports_query?(query)
  end

  test "date_trunc('year') is supported" do
    query = compile!("SELECT COUNT(*) FROM table WHERE date_trunc('year', date) = date '2018-01-01'", data_source())
    assert Drill.supports_query?(query)
  end

  def data_source do
    %{
      name: "drill_test_data_source",
      driver: Drill,
      tables: %{
        table:
          Table.new("table", "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("date", :date)
            ]
          )
      }
    }
  end
end
