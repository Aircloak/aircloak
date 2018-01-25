defmodule Cloak.DataSource.SQLServerTdsTest do
  use ExUnit.Case, async: true
  alias Cloak.DataSource.SQLServerTds

  test "reporting error on conncection" do
    assert_raise Cloak.Query.ExecutionError, "Unknown failure during database connection process",
      fn ->
        SQLServerTds.connect!(%{
          database: "db", hostname: "unknown_host",
          odbc_parameters: %{Port: "1433"},
          password: "some_password",
          username: "user"
        })
      end
  end
end
