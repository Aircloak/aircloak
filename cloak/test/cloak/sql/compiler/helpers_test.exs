defmodule Cloak.Sql.Compiler.Helpers.Test do
  use ExUnit.Case, async: false

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "group_by?" do
    test "true if query groups by columns" do
      assert "SELECT numeric FROM table GROUP BY numeric"
             |> compile!(data_source())
             |> Helpers.group_by?()
    end

    test "true if query groups by index" do
      assert "SELECT numeric FROM table GROUP BY 1"
             |> compile!(data_source())
             |> Helpers.group_by?()
    end

    test "false if no group by clause" do
      refute "SELECT numeric FROM table"
             |> compile!(data_source())
             |> Helpers.group_by?()
    end
  end

  defp data_source() do
    %{
      name: "normalization_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("numeric", :integer),
              Table.column("string", :text)
            ],
            keys: ["key"]
          )
      }
    }
  end
end
