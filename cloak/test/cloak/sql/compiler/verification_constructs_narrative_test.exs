defmodule Cloak.Sql.Compiler.VerificationConstructsNarrative.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "constructs a narrative based on column usage when a query is considered dangerous" do
    test "affected by math" do
      query = """
        SELECT value FROM (
          SELECT uid, numeric + 2 as value
          FROM table
        ) t
        WHERE value > 10 and value <= 20
      """
      assert get_compilation_error(query) =~ ~r/math function '\+'/
    end

    test "affected by discontinuity" do
      query = """
        SELECT value FROM (
          SELECT uid, div(numeric, 2) as value
          FROM table
        ) t
        WHERE value > 10 and value <= 20
      """
      assert get_compilation_error(query) =~ ~r/discontinuous function 'div'/
    end

    test "affected by discontinuity and dangerous math at the same time" do
      query = "SELECT numeric / (numeric + 10) FROM table"
      assert get_compilation_error(query) =~ ~r/divisor that could be zero/
    end

    test "constructs a narrative mentioning usage cast datetime column" do
      query = """
        SELECT value FROM (
          SELECT uid, length(cast(column as text)) as value
          FROM table
        ) t
        WHERE value = 10
      """
      assert get_compilation_error(query) =~ ~r/a cast to/
    end
  end

  defp get_compilation_error(query) do
    case compile(query, data_source()) do
      {:ok, _} -> raise "Expected query compilation to fail, but it didn't"
      {:error, reason} -> reason
    end
  end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("column", :datetime),
            Table.column("numeric", :integer),
            Table.column("float", :real),
            Table.column("string", :text)
          ]
        )
      }
    }
  end
end
