defmodule Cloak.Sql.Compiler.NoiseLayers.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser, Expression}

  describe "picking columns for noise layers" do
    test "lists no noise layers by default" do
      assert [] = compile!("SELECT COUNT(*) FROM table", data_source()).noise_layers
    end

    test "lists columns filtered with WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "noise layer bases are case-normalized" do
      result = compile!("SELECT COUNT(*) FROM camelTable WHERE camelColumn = 3", data_source())

      assert [%{base: {"cameltable", "camelcolumn", nil}}] = result.noise_layers
    end

    test "lists columns filtered with GROUP BY" do
      result = compile!("SELECT numeric, COUNT(*) FROM table GROUP BY numeric", data_source())

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with JOIN" do
      result = compile!(
        "SELECT COUNT(*) FROM table JOIN other ON table.numeric = 3 AND table.uid = other.uid",
        data_source()
      )

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with emulated WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE decoded = 'a'", data_source())

      assert [%{base: {"table", "decoded", nil}, expressions: [%Expression{name: "decoded"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "decoded"}, &1))
    end

    test "lists underlying columns when a function is applied" do
      result = compile!("SELECT COUNT(*) FROM table GROUP BY BUCKET(numeric BY 10)", data_source())

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]}] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "multiple filters on one column" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3 GROUP BY BUCKET(numeric BY 10)", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]}
      ] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end
  end

  describe "noise layers from ranges" do
    test "noise layer from a >=/< range" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric >= 0 AND numeric < 10", data_source())

      assert [
        %{base: {"table", "numeric", {0.0, 10.0}}, expressions: [%Expression{name: "numeric"}]},
      ] = result.noise_layers
    end

    test "noise layer from a BETWEEN" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric BETWEEN 0 AND 10", data_source())

      assert [
        %{base: {"table", "numeric", {0.0, 10.0}}, expressions: [%Expression{name: "numeric"}]},
      ] = result.noise_layers
    end
  end

  describe "noise layer from negative condition" do
    test "simple noise layer" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric <> 10", data_source())

      assert [
        %{base: {"table", "numeric", {:<>, 10}}, expressions: [%Expression{name: "numeric"}]},
      ] = result.noise_layers
    end

    test "no noise layer when compared column is not raw in a subquery" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT upper(name) AS name, uid FROM table) x WHERE lower(name) <> 'bob'",
        data_source()
      )

      assert [] = result.noise_layers
    end

    test "use a noise layer when compared column is raw in subquery" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT name, uid FROM table) x WHERE lower(name) <> 'bob'",
        data_source()
      )

      assert [%{base: {"table", "name", {:<>, "lower", "bob"}}}] = result.noise_layers
    end

    test "no noise layer when compared column is not raw in a join" do
      result = compile!("""
        SELECT COUNT(*) FROM other JOIN (SELECT lower(name) as name, uid FROM table) x
        ON other.uid = x.uid WHERE lower(name) <> 'bob'
      """, data_source())

      assert [] = result.noise_layers
    end

    test "use a noise layer when compared column is raw in a join" do
      result = compile!("""
        SELECT COUNT(*) FROM other JOIN (SELECT name, uid FROM table) x ON other.uid = x.uid WHERE lower(name) <> 'bob'
      """, data_source())

      assert [%{base: {"table", "name", {:<>, "lower", "bob"}}}] = result.noise_layers
    end

    test "noise layers for columns with allowed operations" do
      result = compile!("SELECT COUNT(*) FROM table WHERE lower(name) <> 'bob'", data_source())

      assert [
        %{base: {"table", "name", {:<>, "lower", "bob"}}, expressions: [%Expression{name: "name"}]},
      ] = result.noise_layers
    end

    test "no noise layer for columns with disallowed operations" do
      result = compile!("SELECT COUNT(*) FROM table WHERE sqrt(numeric) <> 10", data_source())

      assert [] = result.noise_layers
    end

    test "no noise layer for columns with allowed operations on non-raw columns" do
      result = compile!("SELECT COUNT(*) FROM table WHERE lower(lower(name)) <> 'bob'", data_source())

      assert [] = result.noise_layers
    end

    test "noise layers for NOT LIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name NOT LIKE 'bob'", data_source())

      assert [
        %{base: {"table", "name", {:not, :like, "bob"}}, expressions: [%Expression{name: "name"}]},
      ] = result.noise_layers
    end

    test "noise layers for NOT ILIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name NOT ILIKE 'bob'", data_source())

      assert [
        %{base: {"table", "name", {:not, :ilike, "bob"}}, expressions: [%Expression{name: "name"}]},
      ] = result.noise_layers
    end

    test "no noise layer when the argument to LIKE is not raw" do
      result = compile!("SELECT COUNT(*) FROM table WHERE lower(name) NOT LIKE 'bob'", data_source())

      assert [] = result.noise_layers
    end

    test "Issue #2077: noise layer for negative condition on COUNT(*)" do
      result = compile!("""
        SELECT COUNT(*) FROM (
          SELECT uid FROM (SELECT uid FROM table) y GROUP BY 1 HAVING COUNT(*) <> 1
        ) x
      """, data_source())

      assert [] = result.noise_layers
    end
  end

  describe "noise layers for LIKE" do
    test "a noise layer per wildcard in LIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name LIKE 'b%_o_%b'", data_source())
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
        %{base: {"table", "name", {:like, {:%, ^len, 1}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:like, {:_, ^len, 1}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:like, {:%, ^len, 3}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:like, {:_, ^len, 3}}}, expressions: [%Expression{name: "name"}]},
      ] = result.noise_layers
    end

    test "a noise layer per wildcard in ILIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name ILIKE 'b%_o_%b'", data_source())
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
        %{base: {"table", "name", {:ilike, {:%, ^len, 1}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:ilike, {:_, ^len, 1}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:ilike, {:%, ^len, 3}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:ilike, {:_, ^len, 3}}}, expressions: [%Expression{name: "name"}]},
      ] = result.noise_layers
    end

    test "noise layers for processed LIKE columns" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name || name2 LIKE 'b%_o_%b'", data_source())
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
        %{base: {"table", "name", {:like, {:%, ^len, 1}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name2", {:like, {:%, ^len, 1}}}, expressions: [%Expression{name: "name2"}]},
        %{base: {"table", "name", {:like, {:_, ^len, 1}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name2", {:like, {:_, ^len, 1}}}, expressions: [%Expression{name: "name2"}]},
        %{base: {"table", "name", {:like, {:%, ^len, 3}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name2", {:like, {:%, ^len, 3}}}, expressions: [%Expression{name: "name2"}]},
        %{base: {"table", "name", {:like, {:_, ^len, 3}}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name2", {:like, {:_, ^len, 3}}}, expressions: [%Expression{name: "name2"}]},
      ] = result.noise_layers
    end

    test "noise layers when LIKE has no wildcards" do
      [%{base: base1, expressions: [%{name: name1}]}] =
        compile!("SELECT COUNT(*) FROM table WHERE name LIKE 'bob'", data_source()).noise_layers
      [%{base: base2, expressions: [%{name: name2}]}] =
        compile!("SELECT COUNT(*) FROM table WHERE name = 'bob'", data_source()).noise_layers

      assert base1 == base2
      assert name1 == name2
    end

    test "noise layers when ILIKE has no wildcards" do
      [%{base: base1, expressions: [%{name: name1}]}] =
        compile!("SELECT COUNT(*) FROM table WHERE name ILIKE 'bob'", data_source()).noise_layers
      [%{base: base2, expressions: [%{name: name2}]}] =
        compile!("SELECT COUNT(*) FROM table WHERE name = 'bob'", data_source()).noise_layers

      assert base1 == base2
      assert name1 == name2
    end
  end

  describe "noise layers from IN" do
    test "IN (single_value)" do
      [%{base: base1, expressions: [%{name: name1}]}] =
        compile!("SELECT COUNT(*) FROM table WHERE name IN ('bob')", data_source()).noise_layers
      [%{base: base2, expressions: [%{name: name2}]}] =
        compile!("SELECT COUNT(*) FROM table WHERE name = 'bob'", data_source()).noise_layers

      assert base1 == base2
      assert name1 == name2
    end

    test "IN (many, values)" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name IN ('a', 'b')", data_source())

      assert [
        %{base: {"table", "name", nil}, expressions: [%{name: "name"}]},
        %{base: {"table", "name", {:in, "a"}}, expressions: [%{name: "name"}]},
        %{base: {"table", "name", {:in, "b"}}, expressions: [%{name: "name"}]},
      ] = result.noise_layers
    end
  end

  describe "noise layers from subqueries" do
    test "floating noise layers from a subquery" do
      result = compile!("SELECT COUNT(*) FROM (SELECT * FROM table WHERE numeric = 3) foo", data_source())

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}]}] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
    end

    test "floating noise layers from a join" do
      result = compile!("""
        SELECT numeric FROM table JOIN (SELECT uid FROM table WHERE numeric = 3) foo ON foo.uid = table.uid
      """, data_source())

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}]}] = result.noise_layers
      assert name != "numeric"
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
    end

    test "floating noise layers from an aggregating subquery" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric FROM table GROUP BY uid, numeric) foo",
        data_source()
      )

      {:subquery, %{ast: subquery}} = result.from

      assert [%{base: {"table", "numeric", nil}, expressions: [%Expression{name: alias}]}] = result.noise_layers
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
      assert 1 = Enum.count(result.noise_layers, &match?(%{base: {"table", "numeric", nil}, expressions: [
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

      assert [%{base: {"table", "numeric", nil}}] = result.noise_layers
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
      assert 1 = Enum.count(result.noise_layers, &match?(%{base: {"table", "numeric", nil}, expressions: [
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
      assert 1 = Enum.count(result.noise_layers, &match?(%{base: {"table", "numeric", nil}, expressions: [
        %Expression{name: ^min_alias},
        %Expression{name: ^max_alias},
        %Expression{name: ^count_alias}
      ]}, &1))
    end
  end

  describe "noise layer base data" do
    test "insensitive to being aliased" do
      %{noise_layers: [%{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric as foo FROM table) bar WHERE foo = 3",
      data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in views" do
      %{noise_layers: [%{base: base}]} = compile!(
        "SELECT count(*) FROM foo WHERE bar = 3",
      data_source(), views: %{"foo" => "SELECT uid, numeric AS bar FROM table"})

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased after operations" do
      %{noise_layers: [%{base: base1}, %{base: base2}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric + numeric2 as foo FROM table) bar WHERE foo = 3",
      data_source())

      assert [{"table", "numeric", nil}, {"table", "numeric2", nil}] = Enum.sort([base1, base2])
    end

    test "insensitive to being aliased in nested subqueries" do
      %{noise_layers: [%{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, foo as bar FROM (SELECT uid, numeric AS foo FROM table) x) y WHERE bar = 3",
      data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in a join" do
      %{noise_layers: [%{base: base}]} = compile!("""
        SELECT COUNT(*) FROM other JOIN (
          SELECT uid, numeric AS foo FROM table
        ) bar
        ON other.uid = bar.uid WHERE foo = 3
      """, data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in emulated queries" do
      %{noise_layers: [%{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, decoded AS bar FROM table) foo WHERE bar = 'a'",
      data_source())

      assert {"table", "decoded", nil} = base
    end

    test "insensitive to the query casing" do
      %{noise_layers: [%{base: base1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{base: base2}]} = compile!("SELECT COUNT(*) FROM table WHERE nUmErIc = 3", data_source())

      assert base1 == base2
    end

    test "insensitive to being quoted" do
      %{noise_layers: [%{base: base1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{base: base2}]} = compile!("SELECT COUNT(*) FROM table WHERE \"numeric\" = 3", data_source())

      assert base1 == base2
    end

    test "insensitive to being scoped" do
      %{noise_layers: [%{base: base1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{base: base2}]} = compile!("SELECT COUNT(*) FROM table WHERE table.numeric = 3", data_source())

      assert base1 == base2
    end
  end

  defp compile!(query_string, data_source, options \\ []) do
    query = Parser.parse!(query_string)
    {:ok, result} = Compiler.compile(data_source, query, Keyword.get(options, :parameters, []),
      Keyword.get(options, :views, %{}))
    result
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
            Table.column("uid", :integer),
            Table.column("numeric", :integer),
            Table.column("numeric2", :integer),
            Table.column("decoded", :text),
            Table.column("dummy", :boolean),
            Table.column("dummy2", :boolean),
            Table.column("name", :text),
            Table.column("name2", :text),
          ],
          decoders: [%{method: "base64", spec: &Base.decode64/1, columns: ["decoded"]}],
          projection: nil,
        },

        other: %{
          db_name: "other",
          name: "other",
          user_id: "uid",
          columns: [Table.column("uid", :integer)],
          projection: nil,
        },

        camel_table: %{
          db_name: "camelTable",
          name: "camelTable",
          user_id: "uid",
          columns: [Table.column("uid", :integer), Table.column("camelColumn", :integer)],
          projection: nil,
        },
      }
    }
  end
end
