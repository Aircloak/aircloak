defmodule Cloak.DataSource.MySQLTest do
  use ExUnit.Case, async: true

  test "connection error" do
    data_source = %{
      name: "test_mysql",
      driver: Cloak.DataSource.MySQL,
      parameters: [hostname: "invalid_host", database: "invalid_database"],
      tables: []
    }

    assert_raise(
      Cloak.Query.ExecutionError,
      ~r/Timeout connecting to the database/,
      fn -> Cloak.DataSource.Connection.execute!(data_source, & &1) end
    )
  end
end
