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
