defmodule Cloak.Sql.Compiler.Validation.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "DISTINCT" do
    test "fails on combination of DISTINCT, GROUP BY and ORDER BY" do
      assert {:error, error} =
               compile(
                 """
                   SELECT DISTINCT numeric
                   FROM table
                   GROUP BY numeric
                   ORDER BY numeric
                 """,
                 data_source()
               )

      assert error =~ ~r/Simultaneous usage of DISTINCT, GROUP BY, and ORDER BY/
    end

    test "Rejects non-aggregate queries with DISTINCT and GROUP BY where some but not all columns are grouped" do
      assert {:error, error} =
               compile(
                 """
                   SELECT DISTINCT numeric, string
                   FROM table
                   GROUP BY string
                 """,
                 data_source()
               )

      assert error =~ ~r/Column `numeric` .* needs to appear in the `GROUP BY` clause/
    end

    test "DISTINCT rewrite does not affect regular warning about missing GROUP BY" do
      assert {:error, error} =
               compile(
                 """
                   SELECT DISTINCT numeric, count(*)
                   FROM table
                 """,
                 data_source()
               )

      assert error =~ ~r/Column `numeric` .* needs to appear in the `GROUP BY` clause/
    end

    test "Rejects DISTINCT with aggregate where additional GROUP BY columns exist" do
      assert {:error, error} = compile("SELECT DISTINCT count(*) FROM table GROUP BY numeric", data_source())
      assert error =~ ~r/Grouping by unselected columns .* DISTINCT/
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
