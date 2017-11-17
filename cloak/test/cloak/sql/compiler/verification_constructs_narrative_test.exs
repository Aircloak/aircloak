defmodule Cloak.Sql.Compiler.VerificationConstructsNarrative.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "constructs a narrative based on column usage when a query is considered dangerous" do
    test "affected by dangerous functions" do
      query = "SELECT abs(abs(abs(abs(abs(abs(numeric + 1)))))) FROM table"
      assert get_compilation_error(query) =~ ~r/potentially dangerous/
    end

    test "affected by potentially crashing functions" do
      query = "SELECT numeric / (numeric + 10) FROM table"
      assert get_compilation_error(query) =~ ~r/that could cause a runtime exception/
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
            Table.column("numeric", :integer),
            Table.column("float", :real),
            Table.column("string", :text)
          ]
        )
      }
    }
  end
end
