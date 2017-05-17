defmodule Cloak.Sql.Compiler.NoiseLayers.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler, Parser, Expression}

  describe "picking columns for noise layers" do
    test "lists no noise layers by default" do
      assert [] = compile!("SELECT COUNT(*) FROM table", data_source()).noise_layers
    end

    test "lists columns filtered with WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with GROUP BY" do
      result = compile!("SELECT numeric, COUNT(*) FROM table GROUP BY numeric", data_source())

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with JOIN" do
      result = compile!(
        "SELECT COUNT(*) FROM table JOIN other ON table.numeric = 3 AND table.uid = other.uid",
        data_source()
      )

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with emulated WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE decoded = 'a'", data_source())

      assert [%{name: {"table", "decoded"}, expressions: [%Expression{name: "decoded"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "decoded"}, &1))
    end

    test "lists underlying columns when a function is applied" do
      result = compile!("SELECT COUNT(*) FROM table GROUP BY BUCKET(numeric BY 10)", data_source())

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "multiple filters on one column" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3 GROUP BY BUCKET(numeric BY 10)", data_source())

      assert [
        %{name: {"table", "numeric"}, expressions: [%Expression{name: "numeric"}]},
        %{name: {"table", "numeric"}, expressions: [%Expression{name: "numeric"}]}
      ] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end
  end

  describe "noise layers from subqueries" do
    test "floating noise layers from a subquery" do
      result = compile!("SELECT COUNT(*) FROM (SELECT * FROM table WHERE numeric = 3) foo", data_source())

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: name}]}] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
    end

    test "floating noise layers from a join" do
      result = compile!("""
        SELECT numeric FROM table JOIN (SELECT uid FROM table WHERE numeric = 3) foo ON foo.uid = table.uid
      """, data_source())

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: name}]}] = result.noise_layers
      assert name != "numeric"
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
    end

    test "floating noise layers from an aggregating subquery" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric FROM table GROUP BY uid, numeric) foo",
        data_source()
      )

      {:subquery, %{ast: subquery}} = result.from

      assert [%{name: {"table", "numeric"}, expressions: [%Expression{name: alias}]}] = result.noise_layers
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: "numeric", alias: ^alias}, &1))
    end

    test "floating columns that are not aggregated" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid FROM table WHERE numeric = 3 GROUP BY uid, dummy) foo",
        data_source()
      )

      %{from: {:subquery, %{ast: subquery}}} = result

      assert [%{alias: min_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: max_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: count_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "count", function_args: [%Expression{name: "numeric"}]}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^count_alias}, &1))
      assert 1 = Enum.count(result.noise_layers, &match?(%{name: {"table", "numeric"}, expressions: [
        %Expression{name: ^min_alias},
        %Expression{name: ^max_alias},
        %Expression{name: ^count_alias}
      ]}, &1))
    end

    test "* expansion doesn't include the carry columns" do
      result = compile!(
        "SELECT * FROM (SELECT uid, decoded FROM table GROUP BY uid, decoded) foo",
        data_source()
      )

      assert [%Expression{value: :*}, %Expression{name: "decoded"}] = result.columns
    end
  end

  describe "noise layers from nested subqueries" do
    test "floating columns from non-aggregating subqueries" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric FROM (SELECT uid, numeric FROM table WHERE numeric = 3) foo) bar",
        data_source()
      )

      assert [%{name: {"table", "numeric"}}] = result.noise_layers
    end

    test "floating complex noise layers through non-aggregating queries" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT * FROM
          (SELECT uid FROM table WHERE numeric = 3 GROUP BY uid, dummy) foo
        ) bar",
        data_source()
      )

      %{from: {:subquery, %{ast: subquery}}} = result
      %{from: {:subquery, %{ast: inner_subquery}}} = subquery

      assert [%{alias: min_alias}] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: max_alias}] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: count_alias}] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "count", function_args: [%Expression{name: "numeric"}]}, &1))
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: ^count_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^count_alias}, &1))
      assert 1 = Enum.count(result.noise_layers, &match?(%{name: {"table", "numeric"}, expressions: [
        %Expression{name: ^min_alias},
        %Expression{name: ^max_alias},
        %Expression{name: ^count_alias}
      ]}, &1))
    end

    test "floating columns that are not aggregated" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid FROM
          (SELECT uid, dummy FROM table WHERE numeric = 3 GROUP BY uid, dummy, dummy2) foo
        GROUP BY uid, dummy) bar",
        data_source()
      )

      %{from: {:subquery, %{ast: subquery}}} = result
      %{from: {:subquery, %{ast: inner_subquery}}} = subquery

      assert [%{alias: min_alias}] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: max_alias}] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: count_alias}] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "count", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: min_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: ^min_alias}]}, &1))
      assert [%{alias: max_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: ^max_alias}]}, &1))
      assert [%{alias: count_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "sum", function_args: [%Expression{name: ^count_alias}]}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^count_alias}, &1))
      assert 1 = Enum.count(result.noise_layers, &match?(%{name: {"table", "numeric"}, expressions: [
        %Expression{name: ^min_alias},
        %Expression{name: ^max_alias},
        %Expression{name: ^count_alias}
      ]}, &1))
    end
  end

  describe "noise layer names" do
    test "insensitive to being aliased" do
      %{noise_layers: [%{name: name}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric as foo FROM table) bar WHERE foo = 3",
      data_source())

      assert {"table", "numeric"} = name
    end

    test "insensitive to being aliased after operations" do
      %{noise_layers: [%{name: name1}, %{name: name2}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric + numeric2 as foo FROM table) bar WHERE foo = 3",
      data_source())

      assert [{"table", "numeric"}, {"table", "numeric2"}] = Enum.sort([name1, name2])
    end

    test "insensitive to being aliased in nested subqueries" do
      %{noise_layers: [%{name: name}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, foo as bar FROM (SELECT uid, numeric AS foo FROM table) x) y WHERE bar = 3",
      data_source())

      assert {"table", "numeric"} = name
    end

    test "insensitive to being aliased in a join" do
      %{noise_layers: [%{name: name}]} = compile!("""
        SELECT COUNT(*) FROM other JOIN (
          SELECT uid, numeric AS foo FROM table
        ) bar
        ON other.uid = bar.uid WHERE foo = 3
      """, data_source())

      assert {"table", "numeric"} = name
    end

    test "insensitive to being aliased in emulated queries"

    test "insensitive to the query casing" do
      %{noise_layers: [%{name: name1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{name: name2}]} = compile!("SELECT COUNT(*) FROM table WHERE nUmErIc = 3", data_source())

      assert name1 == name2
    end

    test "insensitive to being quoted" do
      %{noise_layers: [%{name: name1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{name: name2}]} = compile!("SELECT COUNT(*) FROM table WHERE \"numeric\" = 3", data_source())

      assert name1 == name2
    end

    test "insensitive to being scoped" do
      %{noise_layers: [%{name: name1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{name: name2}]} = compile!("SELECT COUNT(*) FROM table WHERE table.numeric = 3", data_source())

      assert name1 == name2
    end
  end

  defp compile!(query_string, data_source, options \\ []) do
    query = Parser.parse!(query_string)
    {:ok, result} = Compiler.compile(data_source, query, Keyword.get(options, :parameters, []),
      Keyword.get(options, :views, %{}))
    Compiler.NoiseLayers.compile(result)
  end

  defp data_source(driver \\ Cloak.DataSource.PostgreSQL) do
    %{
      driver: driver,
      tables: %{
        table: %{
          db_name: "table",
          name: "table",
          user_id: "uid",
          columns: [
            {"uid", :integer},
            {"numeric", :integer},
            {"numeric2", :integer},
            {"decoded", :text},
            {"dummy", :boolean},
            {"dummy2", :boolean}
          ],
          decoders: [%{method: "base64", columns: ["decoded"]}],
          projection: nil,
        },

        other: %{
          db_name: "other",
          name: "other",
          user_id: "uid",
          columns: [{"uid", :integer}],
          projection: nil,
        },
      }
    }
  end
end
