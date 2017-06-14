defmodule Cloak.DataSource.SqlBuilder.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler, Parser}
  alias Cloak.DataSource
  alias Cloak.DataSource.SqlBuilder

  describe "postgres dialect" do
    test "no handling of regexes in fake subqueries" do
      query = compile!("SELECT extract_match(string, 'pattern') FROM table GROUP BY 1", data_source())

      assert_raise(FunctionClauseError, fn() -> SqlBuilder.build(%{query | subquery?: true}, :postgres) end)
    end
  end

  describe "debug dialect" do
    test "handling of regexes in fake subqueries" do
      query = compile!("SELECT extract_match(string, 'pattern') FROM table GROUP BY 1", data_source())

      assert SqlBuilder.build(%{query | subquery?: true}, :debug) |>
        String.contains?(~s[GROUP BY extract_match("table"."string", ~r/pattern/ui)])
    end

    test "handling of LIMIT and OFFSET" do
      query = compile!("SELECT * FROM table ORDER BY string LIMIT 10 OFFSET 10", data_source())

      assert SqlBuilder.build(%{query | subquery?: true}, :debug) |>
        String.contains?(~s[LIMIT 10 OFFSET 10])
    end

    test "handling of ILIKE" do
      query = compile!("SELECT * FROM table WHERE string ILIKE 'abc%'", data_source())

      assert SqlBuilder.build(%{query | subquery?: true}, :debug) =~ ~r/ILIKE 'abc%'/
    end
  end

  defp compile!(query_string, data_source, options \\ []) do
    query = Parser.parse!(query_string)
    {:ok, result} = Compiler.compile(data_source, query, Keyword.get(options, :parameters, []),
      Keyword.get(options, :views, %{}))
    result
  end

  defp data_source() do
    %{driver: :postgresql, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [
          DataSource.column("uid", :integer),
          DataSource.column("string", :text)
        ],
        projection: nil
      },
    }}
  end
end
