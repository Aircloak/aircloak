defmodule Cloak.DataSource.SQLServerTdsTest do
  use ExUnit.Case, async: true
  alias Cloak.DataSource.SQLServerTds

  test "reporting error on connection" do
    assert_raise Cloak.Query.ExecutionError, "tcp connect: nxdomain",
      fn ->
        SQLServerTds.connect!(%{hostname: "unknown_host", username: "user", password: "some_password",database: "db"})
      end
  end
end
