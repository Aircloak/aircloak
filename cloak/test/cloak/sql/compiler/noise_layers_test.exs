defmodule Cloak.Sql.Compiler.NoiseLayers.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.Expression

  import Cloak.Test.QueryHelpers, except: [compile!: 2, compile!: 3]

  test "overwrites any existing noise layers" do
    compiled = Cloak.Test.QueryHelpers.compile!("SELECT COUNT(*) FROM table", data_source())
    query =
      %{compiled | noise_layers: [%{base: :to_be_overwritten, expressions: []}]}
      |> Cloak.Sql.Compiler.NoiseLayers.compile()

    assert [%{base: nil}] = query.noise_layers
  end

  test "adds a uid noise layer if no other layers are present" do
    assert [%{base: nil, expressions: [%Expression{name: "uid"}]}] =
      compile!("SELECT COUNT(*) FROM table", data_source()).noise_layers
  end

  describe "basic noise layers" do
    test "adds a uid and static noise layer for clear conditions" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{value: 3}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{value: 3}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "noise layers for clear condition don't depend on equality order" do
      result1 = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      result2 = compile!("SELECT COUNT(*) FROM table WHERE 3 = numeric", data_source())

      assert scrub_aliases(result1).noise_layers == scrub_aliases(result2).noise_layers
    end

    test "adds a uid and static noise layer for unclear conditions" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric + 1 = 4", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "noise layer bases are case-normalized" do
      result = compile!("SELECT COUNT(*) FROM camelTable WHERE camelColumn = 3", data_source())

      assert [%{base: {"cameltable", "camelcolumn", nil}}, _] = result.noise_layers
    end

    test "adds a uid and static noise layer for columns filtered with GROUP BY" do
      result = compile!("SELECT numeric, COUNT(*) FROM table GROUP BY numeric", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "adds a uid and static noise layer for columns filtered with JOIN" do
      result = compile!(
        "SELECT COUNT(*) FROM table JOIN other ON table.numeric + 1 = 3 AND table.uid = other.uid",
        data_source()
      )

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "emulated WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE lower(decoded) = 'a'", data_source())

      assert [
        %{base: {"table", "decoded", nil}, expressions: [%Expression{name: "decoded"}]},
        %{base: {"table", "decoded", nil}, expressions: [%Expression{name: "decoded"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "decoded"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "adds a uid and static noise layer for each underlying column when a function is applied" do
      result = compile!("SELECT COUNT(*) FROM table GROUP BY numeric + numeric2", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
        %{base: {"table", "numeric2", nil}, expressions: [%Expression{name: "numeric2"}]},
        %{base: {"table", "numeric2", nil}, expressions: [%Expression{name: "numeric2"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric2"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "multiple filters on one column" do
      result = compile!("""
        SELECT COUNT(*) FROM table WHERE numeric + 1 = 3 GROUP BY BUCKET(numeric BY 10)
      """, data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "columns in top-level select" do
      result = compile!("SELECT numeric FROM table", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "aggregated columns in top-level select are ignored" do
      result = compile!("SELECT COUNT(numeric) FROM table", data_source())

      assert [_generic_noise_layer = %{base: nil}] = result.noise_layers
    end

    test "having in top-level query" do
      result = compile!("SELECT COUNT(*) FROM table HAVING COUNT(numeric) = 10", data_source())

      assert [_generic_noise_layer = %{base: nil}] = result.noise_layers
    end

    test "having in subquery" do
      result = compile!("""
        SELECT COUNT(*) FROM (SELECT uid, COUNT(*) FROM table GROUP BY uid HAVING COUNT(numeric) = 10) x
      """, data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: _},
        %{base: {"table", "numeric", nil}, expressions: _},
      ] = result.noise_layers
    end

    test "clear condition in JOIN" do
      result = compile!("""
        SELECT COUNT(*) FROM table JOIN other ON table.numeric = 3 AND table.uid = other.uid
      """, data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{value: 3}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{value: 3}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      refute Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "a column from a subquery is not clear" do
      result = compile!("""
        SELECT COUNT(*) FROM (SELECT uid, numeric AS number FROM table) x WHERE number = 3
      """, data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%{name: alias}]},
        %{base: {"table", "numeric", nil}, expressions: [%{name: alias}, %{name: "uid"}]},
      ] = result.noise_layers
      refute is_nil(alias)
    end

    test "a comparison of two columns" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = numeric2", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%{name: "numeric"}]},
        %{base: {"table", "numeric", nil}, expressions: [%{name: "numeric"}, %{name: "uid"}]},
        %{base: {"table", "numeric2", nil}, expressions: [%{name: "numeric2"}]},
        %{base: {"table", "numeric2", nil}, expressions: [%{name: "numeric2"}, %{name: "uid"}]},
      ] = result.noise_layers
    end
  end

  describe "skipping noise layers for pk = fk conditions" do
    test "fk = pk" do
      result = compile!("SELECT COUNT(*) FROM table JOIN key_table
        ON table.uid = key_table.uid AND key_table.table_id = table.id", data_source())
      assert [%{base: nil}] = result.noise_layers
    end

    test "pk = fk" do
      result = compile!("SELECT COUNT(*) FROM table JOIN key_table
        ON table.uid = key_table.uid AND table.id = key_table.table_id", data_source())
      assert [%{base: nil}] = result.noise_layers
    end
  end

  describe "noise layers from ranges" do
    test "noise layer from a >=/< range" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric >= 0 AND numeric < 10", data_source())

      assert [
        %{base: {"table", "numeric", {0, 10}}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", {0, 10}}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
    end

    test "noise layer from a BETWEEN" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric BETWEEN 0 AND 10", data_source())

      assert [
        %{base: {"table", "numeric", {0, 10}}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", {0, 10}}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
    end

    test "noise layer from an implicit range" do
      result = compile!("SELECT COUNT(*) FROM table WHERE trunc(numeric) = 10", data_source())

      assert [
        %{base: {"table", "numeric", :implicit}, expressions: [%Expression{name: "numeric"}]},
        %{base: {"table", "numeric", :implicit}, expressions: [%Expression{name: "numeric"}, %Expression{name: "uid"}]},
      ] = result.noise_layers
    end

    test "no noise layer from sample_users" do
      result = compile!("SELECT COUNT(*) FROM (SELECT uid FROM table SAMPLE_USERS 10%) x", data_source())
      assert [_generic_noise_layer = %{base: nil}] = result.noise_layers
    end
  end

  describe "negative conditions" do
    test "noise layer from negative conditions" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric <> 10", data_source())

      assert [
        %{base: {"table", "numeric", :<>}, expressions: [%Expression{value: 10}]},
        %{base: {"table", "numeric", :<>}, expressions: [%Expression{value: 10}, %Expression{name: "uid"}]},
      ] = result.noise_layers
    end

    test "having of COUNT(*)" do
      result = compile!("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(*) <> 10) x
      """, data_source())

      assert [_generic_noise_layer = %{base: nil}] = result.noise_layers
    end

    test "having of count(distinct)" do
      result = compile!("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(distinct numeric) <> 10) x
      """, data_source())

      assert [
        %{base: {"table", "numeric", :<>}, expressions: [%Expression{}]},
        %{base: {"table", "numeric", :<>}, expressions: [%Expression{}, %Expression{name: "uid"}]},
      ] = result.noise_layers
    end
  end

  describe "noise layers for IS NULL" do
    test "a noise layer for IS NULL" do
      assert [
        %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}, %Expression{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name IS NULL", data_source()).noise_layers
    end

    test "a noise layer for IS NOT NULL" do
      assert [
        %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}, %Expression{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name IS NOT NULL", data_source()).noise_layers
    end
  end

  describe "noise layers for LIKE" do
    test "a noise layers in LIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name || name2 LIKE 'b%_o_%b'", data_source())
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
        %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:like, {:%, ^len, 1}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:like, {:_, ^len, 1}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:like, {:%, ^len, 3}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:like, {:_, ^len, 3}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name2", nil}, expressions: [%Expression{name: "name2"}]},
        %{base: {"table", "name2", {:like, {:%, ^len, 1}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
        %{base: {"table", "name2", {:like, {:_, ^len, 1}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
        %{base: {"table", "name2", {:like, {:%, ^len, 3}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
        %{base: {"table", "name2", {:like, {:_, ^len, 3}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
      ] = result.noise_layers
    end

    test "noise layers in ILIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name || name2 ILIKE 'b%_o_%b'", data_source())
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
        %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:ilike, {:%, ^len, 1}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:ilike, {:_, ^len, 1}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:ilike, {:%, ^len, 3}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:ilike, {:_, ^len, 3}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name2", nil}, expressions: [%Expression{name: "name2"}]},
        %{base: {"table", "name2", {:ilike, {:%, ^len, 1}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
        %{base: {"table", "name2", {:ilike, {:_, ^len, 1}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
        %{base: {"table", "name2", {:ilike, {:%, ^len, 3}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
        %{base: {"table", "name2", {:ilike, {:_, ^len, 3}}}, expressions: [%{name: "name2"}, %{name: "uid"}]},
      ] = result.noise_layers
    end

    test "noise layers when LIKE has no wildcards" do
      [
        %{base: base1, expressions: [%{value: "bob"}]},
        %{base: base2,  expressions: [%{value: "bob"}, %{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name LIKE 'bob'", data_source()).noise_layers

      assert [
        %{base: ^base1, expressions: [%{value: "bob"}]},
        %{base: ^base2,  expressions: [%{value: "bob"}, %{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name = 'bob'", data_source()).noise_layers
    end

    test "noise layers when ILIKE has no wildcards" do
      [
        %{base: base1, expressions: [%{name: "name"}]},
        %{base: base2,  expressions: [%{name: "name"}, %{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name ILIKE 'bob'", data_source()).noise_layers

      assert [
        %{base: ^base1, expressions: [%{value: "bob"}]},
        %{base: ^base2,  expressions: [%{value: "bob"}, %{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name = 'bob'", data_source()).noise_layers
    end

    test "noise layers for NOT LIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name NOT LIKE '_bob%'", data_source())
      len = String.length("_bob%") - String.length("%")

      assert [
        %{base: {"table", "name", {:not, :like, "_bob"}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:not, :like, {:_, ^len, 0}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:not, :like, {:%, ^len, 4}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
      ] = result.noise_layers
    end

    test "noise layers for NOT ILIKE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name NOT ILIKE '_bob%'", data_source())
      len = String.length("_bob%") - String.length("%")

      assert [
        %{base: {"table", "name", {:not, :ilike, "_bob"}}, expressions: [%Expression{name: "name"}]},
        %{base: {"table", "name", {:not, :ilike, {:_, ^len, 0}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
        %{base: {"table", "name", {:not, :ilike, {:%, ^len, 4}}}, expressions: [%{name: "name"}, %{name: "uid"}]},
      ] = result.noise_layers
    end

    test "noise layers when NOT LIKE has no wildcards" do
      result1 = compile!("SELECT COUNT(*) FROM table WHERE name NOT LIKE 'bob'", data_source())
      result2 = compile!("SELECT COUNT(*) FROM table WHERE name <> 'bob'", data_source())

      assert Enum.map(result1.noise_layers, & &1.base) == Enum.map(result2.noise_layers, & &1.base)
    end

    test "noise layers when NOT ILIKE has no wildcards" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name NOT ILIKE 'bob'", data_source())

      assert [
        %{base: {"table", "name", {:not, :ilike, "bob"}}, expressions: [%{name: "name"}]},
        %{base: {"table", "name", {:not, :ilike, "bob"}}, expressions: [%{name: "name"}, %{name: "uid"}]},
      ] = result.noise_layers
    end
  end

  describe "noise layers from IN" do
    test "IN (single_value)" do
      [
        %{base: base1, expressions: [%{name: name}]},
        %{base: base2,  expressions: [%{name: name}, %{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name IN ('bob')", data_source()).noise_layers

      assert [
        %{base: ^base1, expressions: [%{name: ^name}]},
        %{base: ^base2,  expressions: [%{name: ^name}, %{name: "uid"}]},
      ] = compile!("SELECT COUNT(*) FROM table WHERE name = 'bob'", data_source()).noise_layers
    end

    test "IN (many, values)" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name IN ('a', 'b')", data_source())

      assert [
        %{base: {"table", "name", nil}, expressions: [%{name: "name"}]},
        %{base: {"table", "name", nil}, expressions: [%{value: "a"}, %{name: "uid"}]},
        %{base: {"table", "name", nil}, expressions: [%{value: "b"}, %{name: "uid"}]},
      ] = result.noise_layers
    end

    for function <- ~w(upper lower) do
      test "#{function}(x) IN (many, values)" do
        result = compile!("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(name) IN ('a', 'b')", data_source())

        assert [
          %{base: {"table", "name", nil}, expressions: [%{name: "name"}]},
          %{base: {"table", "name", nil}, expressions: [%{value: "a"}, %{name: "uid"}]},
          %{base: {"table", "name", nil}, expressions: [%{value: "b"}, %{name: "uid"}]},
        ] = result.noise_layers
      end
    end
  end

  describe "noise layers from subqueries" do
    test "floating noise layers from a subquery" do
      result = compile!("SELECT COUNT(*) FROM (SELECT * FROM table WHERE numeric + 1 = 3) foo", data_source())

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "floating noise layers from a join" do
      result = compile!("""
        SELECT numeric FROM table JOIN (SELECT uid FROM table WHERE numeric + 1= 3) foo ON foo.uid = table.uid
      """, data_source())

      assert [
        _select_static_layer = %{},
        _select_uid_layer = %{},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}]},
        %{base: {"table", "numeric", nil}, expressions: [
          %Expression{name: name},
          %Expression{name: "uid", table: %{name: "table"}},
        ]},
      ] = result.noise_layers
      assert name != "numeric"
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "uid", table: %{name: "table"}}, &1))
    end

    test "floating noise layers from an aggregating subquery" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric FROM table GROUP BY uid, numeric) foo",
        data_source()
      )

      {:subquery, %{ast: subquery}} = result.from

      assert [
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: alias}]},
        %{base: {"table", "numeric", nil}, expressions: [%Expression{name: alias}, %Expression{name: "uid"}]},
      ] = result.noise_layers
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: "numeric", alias: ^alias}, &1))
    end

    test "floating columns that are not aggregated" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid FROM table WHERE numeric + 1 = 3 GROUP BY uid, dummy) foo",
        data_source()
      )

      %{from: {:subquery, %{ast: subquery}}} = result

      assert [%{alias: min_alias}, %{alias: other_min_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: max_alias}, %{alias: other_max_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: count_alias}, %{alias: other_count_alias}] = Enum.filter(subquery.db_columns,
        &match?(%Expression{function: "count", function_args: [%Expression{name: "numeric"}]}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^count_alias}, &1))
      assert 1 = Enum.count(result.noise_layers, &match?(%{base: {"table", "numeric", nil}, expressions: [
        %Expression{name: ^min_alias},
        %Expression{name: ^max_alias},
        %Expression{name: ^count_alias}
      ]}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^other_min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^other_max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^other_count_alias}, &1))
      assert 1 = Enum.count(result.noise_layers, &match?(%{base: {"table", "numeric", nil}, expressions: [
        %Expression{name: ^other_min_alias},
        %Expression{name: ^other_max_alias},
        %Expression{name: ^other_count_alias},
        %Expression{name: "uid"},
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

      assert [
        %{base: {"table", "numeric", nil}},
        %{base: {"table", "numeric", nil}, expressions: [_, %{name: "uid"}]},
      ] = result.noise_layers
    end

    test "floating complex noise layers through non-aggregating queries" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT * FROM
          (SELECT uid FROM table WHERE numeric + 1 = 3 GROUP BY uid, dummy) foo
        ) bar",
        data_source()
      )

      %{from: {:subquery, %{ast: subquery}}} = result
      %{from: {:subquery, %{ast: inner_subquery}}} = subquery

      assert [%{alias: min_alias}, _] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: max_alias}, _] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: count_alias}, _] = Enum.filter(inner_subquery.db_columns,
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
          (SELECT uid, dummy FROM table WHERE numeric + 1 = 3 GROUP BY uid, dummy, dummy2) foo
        GROUP BY uid, dummy) bar",
        data_source()
      )

      %{from: {:subquery, %{ast: subquery}}} = result
      %{from: {:subquery, %{ast: inner_subquery}}} = subquery

      assert [%{alias: min_alias}, _] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "min", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: max_alias}, _] = Enum.filter(inner_subquery.db_columns,
        &match?(%Expression{function: "max", function_args: [%Expression{name: "numeric"}]}, &1))
      assert [%{alias: count_alias}, _] = Enum.filter(inner_subquery.db_columns,
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
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric as foo FROM table) bar WHERE foo = 3",
      data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in views" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!(
        "SELECT count(*) FROM foo WHERE bar = 3",
      data_source(), views: %{"foo" => "SELECT uid, numeric AS bar FROM table"})

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased after operations" do
      %{noise_layers: [%{base: b1}, %{base: b2}, %{base: b3}, %{base: b4}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric + numeric2 as foo FROM table) bar WHERE foo = 3",
      data_source())

      assert [{"table", "numeric", nil}, {"table", "numeric2", nil}] = [b1, b2, b3, b4] |> Enum.uniq() |> Enum.sort()
    end

    test "insensitive to being aliased in nested subqueries" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, foo as bar FROM (SELECT uid, numeric AS foo FROM table) x) y WHERE bar = 3",
      data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in a join" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!("""
        SELECT COUNT(*) FROM other JOIN (
          SELECT uid, numeric AS foo FROM table
        ) bar
        ON other.uid = bar.uid WHERE foo = 3
      """, data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in emulated queries" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, decoded AS bar FROM table) foo WHERE bar = 'a'",
      data_source())

      assert {"table", "decoded", nil} = base
    end

    test "insensitive to the query casing" do
      %{noise_layers: [%{base: base1}, %{base: base1}]} =
        compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{base: base2}, %{base: base2}]} =
        compile!("SELECT COUNT(*) FROM table WHERE nUmErIc = 3", data_source())

      assert base1 == base2
    end

    test "insensitive to being quoted" do
      %{noise_layers: [%{base: base1}, %{base: base1}]} =
        compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{base: base2}, %{base: base2}]} =
        compile!(~s[SELECT COUNT(*) FROM table WHERE "numeric" = 3], data_source())

      assert base1 == base2
    end

    test "insensitive to being scoped" do
      %{noise_layers: [%{base: base1}, %{base: base1}]} =
        compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())
      %{noise_layers: [%{base: base2}, %{base: base2}]} =
        compile!("SELECT COUNT(*) FROM table WHERE table.numeric = 3", data_source())

      assert base1 == base2
    end

    test "insensitive to the table being aliased" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!(
        "SELECT COUNT(*) FROM table AS t WHERE numeric = 3",
      data_source())

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to the table being aliased in subquery" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric FROM table AS t WHERE numeric = 3) x",
      data_source())

      assert {"table", "numeric", nil} = base
    end
  end

  defp compile!(query, data_source, opts \\ []), do:
    Cloak.Test.QueryHelpers.compile!(query, data_source, opts)
    |> Cloak.Sql.Compiler.NoiseLayers.compile()

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("numeric", :integer),
            Table.column("numeric2", :integer),
            Table.column("decoded", :text),
            Table.column("dummy", :boolean),
            Table.column("dummy2", :boolean),
            Table.column("name", :text),
            Table.column("name2", :text),
            Table.column("id", :integer),
          ],
          decoders: [%{method: "base64", spec: &Base.decode64/1, columns: ["decoded"]}],
          keys: ["id"],
        ),

        other: Cloak.DataSource.Table.new("other", "uid",
          db_name: "other",
          columns: [Table.column("uid", :integer)]
        ),

        camel_table: Cloak.DataSource.Table.new("camelTable", "uid",
          db_name: "camelTable",
          columns: [Table.column("uid", :integer), Table.column("camelColumn", :integer)]
        ),

        key_table: Cloak.DataSource.Table.new("key_table", "uid",
          db_name: "key_table",
          columns: [Table.column("uid", :integer), Table.column("table_id", :integer)],
          keys: ["table_id"],
        )
      }
    }
  end
end
