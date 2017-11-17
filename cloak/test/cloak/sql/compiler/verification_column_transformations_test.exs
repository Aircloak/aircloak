defmodule Cloak.Sql.Compiler.VerificationColumnTransformations.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "Rejects invalid transformations" do
    test "SELECTed expression with more than 5 dangerous transformations" do
      query = """
        SELECT value
        FROM #{offensive_subquery()}
      """
      refute transformations_valid?(query)
    end

    test "GROUP BY-clause expression with more than 5 dangerous transformations" do
      query = """
        SELECT count(*)
        FROM #{offensive_subquery()}
        GROUP BY value
      """
      refute transformations_valid?(query)
    end

    test "ORDER BY-clause expression with more than 5 dangerous transformations" do
      query = """
        SELECT string
        FROM #{offensive_subquery()}
        ORDER BY value ASC
      """
      refute transformations_valid?(query)
    end

    test "WHERE-clause expression with more than 5 dangerous transformations" do
      query = """
        SELECT string
        FROM #{offensive_subquery()}
        WHERE value <> 0
      """
      refute transformations_valid?(query)
    end

    test "HAVING-clause expression with more than 5 dangerous transformations" do
      query = """
        SELECT string
        FROM #{offensive_subquery()}
        HAVING count(value) > 0
      """
      refute transformations_valid?(query)
    end

    test "JOIN ON-clause expression with more than 5 dangerous transformations" do
      query = """
        SELECT string
        FROM #{offensive_subquery()} INNER JOIN table
          ON c.uid = table.uid and c.value <> 0
      """
      refute transformations_valid?(query)
    end
  end

  defp offensive_subquery(), do:
    """
    (
      SELECT
        a.uid,
        v1 + v2 as value,
        a.string as string
      FROM (
        SELECT
          uid,
          string,
          abs(abs(div(numeric, 2))) as v1 -- 3 dangerous transformations
        FROM table
      ) a INNER JOIN (
        SELECT
          uid,
          abs(abs(div(numeric, 2))) as v2 -- 3 dangerous transformations
        FROM table
      ) b ON a.uid = b.uid
    ) c
    """

  defp transformations_valid?(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} ->
        if reason =~ ~r/potentially dangerous/ do
          false
        else
          raise "Compilation failed with other reason than illegal filtering condition: #{inspect reason}"
        end
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
            Table.column("string", :text)
          ]
        )
      }
    }
  end
end
