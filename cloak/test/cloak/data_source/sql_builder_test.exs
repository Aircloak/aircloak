defmodule Cloak.DataSource.SqlBuilderTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.{SqlBuilder, Table}
  import Cloak.Test.QueryHelpers

  test "text column is force casted to text" do
    assert sql_string("select string from table") =~ ~r/CAST\("table"\."string" AS text\)/
    assert sql_string("select * from (select upper(string) as s from table) t") =~
      ~r/UPPER\(CAST\("table"\."string" AS text\)\)/
    assert sql_string("select int from table where string=''") =~ ~r/CAST\("table"\."string" AS text\) = ''/
  end

  test "non-text column is not force casted", do:
    refute sql_string("select int from table") =~ ~r/CAST\("table"\."int"/

  defp sql_string(query, dialect \\ Cloak.DataSource.SqlBuilder.PostgreSQL), do:
    query
    |> compile!(data_source())
    |> SqlBuilder.build(dialect)

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      driver_dialect: Cloak.DataSource.SqlBuilder.PostgreSQL,
      tables: %{
        table: %{
          db_name: "table",
          name: "table",
          user_id: "uid",
          columns: [
            Table.column("uid", :integer),
            Table.column("string", :text),
            Table.column("int", :integer),
          ],
          projection: nil,
        }
      }
    }
  end
end
