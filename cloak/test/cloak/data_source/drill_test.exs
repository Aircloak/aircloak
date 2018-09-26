defmodule Cloak.DataSource.Drill.Test do
  use ExUnit.Case

  alias Cloak.DataSource.{DrillRODBC, Table}
  import Cloak.Test.QueryHelpers

  test "CROSS JOIN is not supported" do
    query =
      compile!(
        """
          SELECT * FROM
          table AS a
            CROSS JOIN table AS b
            JOIN table AS c ON a.uid = c.uid
            WHERE a.uid = b.uid
        """,
        data_source()
      )

    refute DrillRODBC.supports_query?(query)
  end

  def data_source do
    %{
      name: "drill_test_data_source",
      driver: DrillRODBC,
      tables: %{
        table: Table.new("table", "uid", db_name: "table", columns: [Table.column("uid", :integer)])
      }
    }
  end
end
