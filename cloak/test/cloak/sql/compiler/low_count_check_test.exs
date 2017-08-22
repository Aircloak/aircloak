defmodule Cloak.Sql.Compiler.LowCountCheck.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.Expression

  test "adds a low-count check for every LIKE condition" do
    result = compile!(
      "SELECT COUNT(*) FROM table WHERE name LIKE '%a%' AND name ILIKE '%b%' AND name2 LIKE '%c%'", data_source())

    assert [
      %{expressions: [%Expression{name: "name"}], type: :like},
      %{expressions: [%Expression{name: "name"}], type: :ilike},
      %{expressions: [%Expression{name: "name2"}], type: :like},
    ] = result.low_count_checks
  end

  test "adds a low-count check for aggregated LIKE conditions"

  test "floating columns from aggregating subquery"

  test "floating columns from non-aggregating subquery"

  defp compile!(query, data_source, opts \\ []), do:
    Cloak.Test.QueryHelpers.compile!(query, data_source, opts)
    |> Cloak.Sql.Compiler.LowCountCheck.compile()

  def data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      driver_dialect: :postgresql,
      tables: %{
        table: %{
          db_name: "table",
          name: "table",
          user_id: "uid",
          columns: [
            Table.column("uid", :integer),
            Table.column("name", :text),
            Table.column("name2", :text),
          ],
          projection: nil,
        },
      }
    }
  end
end
