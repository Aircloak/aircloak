defmodule Cloak.DataSource.SqlBuilderTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.{SqlBuilder, Table}
  alias Cloak.Sql.Query
  import Cloak.Test.QueryHelpers

  test "non-text column is not force casted",
    do: refute(sql_string("select int from table") =~ ~r/CAST\("table"\."int"/)

  test "workaround for text comparisons on SQL Server ignoring trailing spaces",
    do: assert(sql_string("select count(*) from table where string = 'ab'", SQLServer) =~ "= N'ab.')")

  test "table name quoting" do
    assert SqlBuilder.quote_table_name("name") == "\"name\""
    assert SqlBuilder.quote_table_name("\"name\"") == "\"name\""
    assert SqlBuilder.quote_table_name("full.name") == "\"full\".\"name\""
    assert SqlBuilder.quote_table_name("long.full.name") == "\"long\".\"full\".\"name\""
  end

  describe "table_name_parts" do
    test "unqualified single part", do: assert(SqlBuilder.table_name_parts("foo") == ["foo"])
    test "unqualified with whitespace", do: assert(SqlBuilder.table_name_parts("foo bar baz") == ["foo bar baz"])
    test "unqualified multi parts", do: assert(SqlBuilder.table_name_parts("foo.bar.baz") == ["foo", "bar", "baz"])

    test "qualified single part", do: assert(SqlBuilder.table_name_parts(~s/"foo"/) == ["foo"])
    test "qualified with quotes in the name", do: assert(SqlBuilder.table_name_parts(~s/"f""oo"/) == [~s(f"oo)])
    test "qualified multi part", do: assert(SqlBuilder.table_name_parts(~s/"foo"."bar"."baz"/) == ["foo", "bar", "baz"])

    test "mixed multi part", do: assert(SqlBuilder.table_name_parts(~s/"foo".bar."baz"/) == ["foo", "bar", "baz"])

    test "empty parts are not allowed" do
      assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts("") end
      assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts(".") end
      assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts("foo.") end
      assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts(".foo") end
      assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts("foo..bar") end
    end

    test "non-matching quotes are not allowed",
      do: assert_raise(ArgumentError, fn -> SqlBuilder.table_name_parts(~s/"foobar/) end)
  end

  defp sql_string(query, dialect \\ PostgreSQL) do
    compiled_query =
      query
      |> compile!(data_source(Module.concat(Cloak.DataSource, dialect)))
      |> Query.resolve_db_columns()

    SqlBuilder.build(compiled_query)
  end

  defp data_source(driver) do
    %{
      driver: driver,
      tables: %{
        table:
          Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("string", :text),
              Table.column("int", :integer)
            ]
          )
      }
    }
  end
end
