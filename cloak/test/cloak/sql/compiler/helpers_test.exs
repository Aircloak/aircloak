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

  describe "grouped_by?" do
    test "true if a column is grouped by column name" do
      assert "SELECT numeric FROM table GROUP BY numeric"
             |> compile!(data_source())
             |> check_first_column(&Helpers.grouped_by?/2)
    end

    test "true if a column is grouped by alias" do
      assert "SELECT numeric as a FROM table GROUP BY a"
             |> compile!(data_source())
             |> check_first_column(&Helpers.grouped_by?/2)
    end

    test "true if a column is grouped by index" do
      assert "SELECT numeric as a FROM table GROUP BY 1"
             |> compile!(data_source())
             |> check_first_column(&Helpers.grouped_by?/2)
    end

    test "false if a column is not grouped by" do
      refute "SELECT numeric FROM table"
             |> compile!(data_source())
             |> check_first_column(&Helpers.grouped_by?/2)
    end
  end

  describe "aggregates?" do
    test "false if no column contains an aggregates" do
      refute "SELECT numeric FROM table"
             |> compile!(data_source())
             |> Helpers.aggregates?()
    end

    test "true if selected columns contains an aggregates" do
      assert "SELECT count(*) FROM table"
             |> compile!(data_source())
             |> Helpers.aggregates?()
    end

    test "true if nests an aggregates" do
      assert "SELECT trunc(avg(numeric)) FROM table"
             |> compile!(data_source())
             |> Helpers.aggregates?()
    end
  end

  describe "aggregated_column?" do
    test "false for plain column" do
      refute "SELECT numeric FROM table"
             |> compile!(data_source())
             |> check_first_column(&Helpers.aggregated_column?/2)
    end

    test "false for plain column with normal function" do
      refute "SELECT abs(numeric) FROM table"
             |> compile!(data_source())
             |> check_first_column(&Helpers.aggregated_column?/2)
    end

    test "false for plain column that is grouped" do
      refute "SELECT numeric FROM table GROUP BY numeric"
             |> compile!(data_source())
             |> check_first_column(&Helpers.aggregated_column?/2)
    end

    test "true for column that has an aggregate applied to it" do
      assert "SELECT avg(numeric) FROM table"
             |> compile!(data_source())
             |> check_first_column(&Helpers.aggregated_column?/2)
    end

    test "true for column that has a nested aggregate applied to it" do
      assert "SELECT trunc(avg(numeric)) FROM table"
             |> compile!(data_source())
             |> check_first_column(&Helpers.aggregated_column?/2)
    end
  end

  defp check_first_column(%Query{columns: [column | _]} = query, test), do: test.(query, column)

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
