defmodule Cloak.Sql.Compiler.LowCountChecks.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.Expression

  test "adds a low-count check for every LIKE condition" do
    result = compile!(
      "SELECT COUNT(*) FROM table WHERE name LIKE '%a%' AND name ILIKE '%b%' AND name2 LIKE '%c%'", data_source())

    assert [
      %{expressions: [%Expression{name: "name"}]},
      %{expressions: [%Expression{function: "lower", function_args: [%Expression{name: "name"}]}]},
      %{expressions: [%Expression{name: "name2"}]},
    ] = result.low_count_checks
  end

  test "adds a low-count check for every LIKE condition in a subquery" do
    result = compile!("SELECT COUNT(*) FROM (
      SELECT uid FROM table WHERE name LIKE '%a%' AND name LIKE '%b%' AND name2 ILIKE '%c%') x", data_source())
    {:subquery, %{ast: subquery}} = result.from

    assert [
      %{expressions: [%Expression{name: alias1}]},
      %{expressions: [%Expression{name: alias1}]},
      %{expressions: [%Expression{name: alias2}]},
    ] = result.low_count_checks
    assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: "name", alias: ^alias1}, &1))
    assert 1 = Enum.count(subquery.db_columns, &match?(
      %Expression{function: "lower", alias: ^alias2, function_args: [%Expression{name: "name2"}]}, &1))
  end

  test "floating columns from aggregating subquery" do
    result = compile!("SELECT COUNT(*) FROM (
      SELECT uid FROM table WHERE name LIKE '%a%' GROUP BY uid) x", data_source())
    {:subquery, %{ast: subquery}} = result.from

    assert [%{expressions: [
      %Expression{function?: false, name: alias1},
      %Expression{function?: false, name: alias2},
    ]}] = result.low_count_checks
    assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{function: "min", alias: ^alias1, function_args: [
      %Expression{name: "name"}]}, &1))
    assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{function: "max", alias: ^alias2, function_args: [
      %Expression{name: "name"}]}, &1))
  end

  test "floating complex columns from non-aggregating subquery" do
    result = compile!("SELECT COUNT(*) FROM (
      SELECT uid FROM table WHERE lower(name) LIKE '%a%') x", data_source())
    {:subquery, %{ast: subquery}} = result.from

    assert [%{expressions: [%Expression{function?: false, name: alias}]}] = result.low_count_checks
    refute is_nil(alias)
    assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{function: "lower", alias: ^alias, function_args: [
      %Expression{name: "name"}]}, &1))
  end

  defp compile!(query, data_source, opts \\ []), do:
    Cloak.Test.QueryHelpers.compile!(query, data_source, opts)
    |> Cloak.Sql.Compiler.LowCountChecks.compile()

  def data_source() do
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
            Table.column("name", :text),
            Table.column("name2", :text),
          ],
          projection: nil,
        },
      }
    }
  end
end
