defmodule Cloak.Sql.Compiler.NoiseLayers.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Expression, NoiseLayer}

  defmacrop static_layer(base) do
    quote do
      %NoiseLayer{expressions: [_, _, _], base: unquote(base)}
    end
  end

  defmacrop uid_layer(base) do
    quote do
      %NoiseLayer{expressions: [_, _, _, %Expression{user_id?: true}], base: unquote(base)}
    end
  end

  defmacrop generic_layer() do
    quote do
      %{base: nil}
    end
  end

  test "overwrites any existing noise layers" do
    compiled = Cloak.Test.QueryHelpers.compile!("SELECT COUNT(*) FROM table", data_source())

    query =
      %{compiled | noise_layers: [%{base: :to_be_overwritten, expressions: []}]}
      |> Cloak.Sql.Compiler.NoiseLayers.compile()

    assert [%{base: nil}] = query.noise_layers
  end

  test "adds a uid noise layer if no other layers are present" do
    assert [%{base: nil, expressions: [%Expression{user_id?: true}]}] =
             compile!("SELECT COUNT(*) FROM table").noise_layers
  end

  describe "basic noise layers" do
    test "adds a uid and static noise layer for clear conditions" do
      result = compile!("SELECT MEDIAN(numeric) FROM table WHERE numeric = 3")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{value: 3},
                   %Expression{value: 3},
                   %Expression{value: 1}
                 ]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{value: 3},
                   %Expression{value: 3},
                   %Expression{value: 1},
                   %Expression{user_id?: true}
                 ]
               }
             ] = result.noise_layers

      assert Enum.any?(result.db_columns, &match?(%Expression{user_id?: true}, &1))
    end

    test "column < column coditions" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric < numeric")
      assert [generic_layer()] = result.noise_layers
    end

    test "noise layers for clear condition don't depend on equality order" do
      result1 = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3")
      result2 = compile!("SELECT COUNT(*) FROM table WHERE 3 = numeric")

      assert result1.noise_layers == result2.noise_layers
    end

    test "adds a uid and static noise layer for unclear conditions" do
      result = compile!("SELECT MEDIAN(numeric) FROM table WHERE numeric + 1 = 4")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "numeric"},
                   %Expression{name: "numeric"},
                   %Expression{value: 1}
                 ]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "numeric"},
                   %Expression{name: "numeric"},
                   %Expression{value: 1},
                   %Expression{name: "uid"}
                 ]
               }
             ] = result.noise_layers

      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "noise layer bases are case-normalized" do
      result = compile!("SELECT COUNT(*) FROM camelTable WHERE camelColumn = 3")

      assert [%{base: {"cameltable", "camelcolumn", nil}}, _] = result.noise_layers
    end

    test "adds a uid and static noise layer for columns filtered with GROUP BY - uid anon" do
      result = compile!("SELECT numeric, MEDIAN(uid) FROM table GROUP BY numeric")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "numeric"},
                   %Expression{name: "numeric"},
                   %Expression{value: 1}
                 ]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "numeric"},
                   %Expression{name: "numeric"},
                   %Expression{value: 1},
                   %Expression{user_id?: true}
                 ]
               }
             ] = result.noise_layers

      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "adds a uid and static noise layer for columns filtered with GROUP BY - stats anon" do
      result = compile!("SELECT numeric, COUNT(*) FROM table GROUP BY numeric")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "__ac_group_0"},
                   %Expression{name: "__ac_group_0"},
                   %Expression{name: "__ac_agg_0_sum"}
                 ]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "__ac_group_0"},
                   %Expression{name: "__ac_group_0"},
                   %Expression{name: "__ac_agg_0_sum"},
                   %Expression{user_id?: true}
                 ]
               }
             ] = result.noise_layers

      assert Enum.any?(result.db_columns, &match?(%Expression{name: "__ac_group_0"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{user_id?: true}, &1))
    end

    test "adds a uid and static noise layer for columns filtered with JOIN" do
      result =
        compile!("SELECT MEDIAN(table.uid) FROM table JOIN other ON table.numeric + 1 = 3 AND table.uid = other.uid")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "numeric"},
                   %Expression{name: "numeric"},
                   %Expression{value: 1}
                 ]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: "numeric"},
                   %Expression{name: "numeric"},
                   %Expression{value: 1},
                   %Expression{name: "uid"}
                 ]
               }
             ] = result.noise_layers

      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "adds a uid and static noise layer for each underlying column when a function is applied" do
      result = compile!("SELECT MEDIAN(uid) FROM table GROUP BY numeric + numeric2")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: "numeric"}, _, _]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: "numeric"}, _, _, %Expression{name: "uid"}]
               },
               %{
                 base: {"table", "numeric2", nil},
                 expressions: [%Expression{name: "numeric2"}, _, _]
               },
               %{
                 base: {"table", "numeric2", nil},
                 expressions: [%Expression{name: "numeric2"}, _, _, %Expression{name: "uid"}]
               }
             ] = result.noise_layers

      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric2"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "multiple filters on one column" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE numeric + 1 = 3 GROUP BY BUCKET(numeric BY 10)")

      assert [
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: "numeric"}, _, _]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: "numeric"}, _, _, %Expression{name: "uid"}]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: "numeric"}, _, _]
               },
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: "numeric"}, _, _, %Expression{name: "uid"}]
               }
             ] = result.noise_layers

      assert 2 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "aggregated columns in top-level select are ignored" do
      result = compile!("SELECT COUNT(*) FROM table")

      assert [generic_layer()] = result.noise_layers
    end

    test "having in top-level query" do
      result = compile!("SELECT COUNT(*) FROM table HAVING COUNT(numeric) = 10")

      assert [generic_layer()] = result.noise_layers
    end

    test "having in subquery" do
      result =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid, COUNT(*) FROM table GROUP BY uid HAVING COUNT(numeric) = 10) x
        """)

      assert [
               static_layer({"table", "numeric", nil}),
               uid_layer({"table", "numeric", nil})
             ] = result.noise_layers
    end

    test "clear condition in JOIN" do
      result = compile!("SELECT MEDIAN(table.uid) FROM table JOIN other ON table.numeric = 3 AND table.uid = other.uid")

      assert [
               %{base: {"table", "numeric", nil}, expressions: [%Expression{value: 3}, _, _]},
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{value: 3}, _, _, %Expression{name: "uid"}]
               }
             ] = result.noise_layers

      refute Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "uid"}, &1))
    end

    test "a comparison of two columns" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE numeric = numeric2")

      assert [
               %{base: {"table", "numeric", nil}, expressions: [%{name: "numeric"}, _, _]},
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%{name: "numeric"}, _, _, %{name: "uid"}]
               },
               %{base: {"table", "numeric2", nil}, expressions: [%{name: "numeric2"}, _, _]},
               %{
                 base: {"table", "numeric2", nil},
                 expressions: [%{name: "numeric2"}, _, _, %{name: "uid"}]
               }
             ] = result.noise_layers
    end
  end

  describe "skipping noise layers for pk = fk conditions" do
    test "fk = pk" do
      result =
        compile!("""
          SELECT COUNT(*) FROM table JOIN key_table
          ON table.uid = key_table.uid AND key_table.table_id = table.id
        """)

      assert [generic_layer()] = result.noise_layers
    end

    test "pk = fk" do
      result =
        compile!("""
          SELECT COUNT(*) FROM table JOIN key_table
          ON table.uid = key_table.uid AND table.id = key_table.table_id
        """)

      assert [generic_layer()] = result.noise_layers
    end
  end

  describe "noise layers from ranges" do
    test "noise layer from a >=/< range" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE numeric >= 0 AND numeric < 10")

      assert [
               %{
                 base: {"table", "numeric", {0, 10}},
                 expressions: [%Expression{name: "numeric"}, _, _]
               },
               %{
                 base: {"table", "numeric", {0, 10}},
                 expressions: [%Expression{name: "numeric"}, _, _, %Expression{name: "uid"}]
               }
             ] = result.noise_layers
    end

    test "noise layer from a BETWEEN" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE numeric BETWEEN 0 AND 10")

      assert [
               %{
                 base: {"table", "numeric", {0, 10}},
                 expressions: [%Expression{name: "numeric"}, _, _]
               },
               %{
                 base: {"table", "numeric", {0, 10}},
                 expressions: [%Expression{name: "numeric"}, _, _, %Expression{name: "uid"}]
               }
             ] = result.noise_layers
    end

    test "noise layer from an implicit range" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE trunc(numeric, -1) = 10")

      assert [
               %{
                 base: {"table", "numeric", :implicit},
                 expressions: [%Expression{name: "numeric"}, _, _]
               },
               %{
                 base: {"table", "numeric", :implicit},
                 expressions: [%Expression{name: "numeric"}, _, _, %Expression{name: "uid"}]
               }
             ] = result.noise_layers
    end

    test "no noise layer from sample_users" do
      result = compile!("SELECT MEDIAN(uid) FROM (SELECT uid FROM table SAMPLE_USERS 10%) x")

      assert [generic_layer()] = result.noise_layers
    end
  end

  describe "negative conditions" do
    test "noise layer from negative conditions" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric <> 10")

      assert [
               %{base: {"table", "numeric", :<>}, expressions: [%Expression{value: 10}, _, _]},
               %{
                 base: {"table", "numeric", :<>},
                 expressions: [%Expression{value: 10}, _, _, %Expression{user_id?: true}]
               }
             ] = result.noise_layers
    end

    test "column <> column negative condition" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric <> numeric")
      assert [generic_layer()] = result.noise_layers
    end

    test "clear numeric negative condition" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric <> 1")

      assert [
               %{base: {"table", "numeric", :<>}, expressions: [%Expression{value: 1}, _, _]},
               %{
                 base: {"table", "numeric", :<>},
                 expressions: [%Expression{value: 1}, _, _, %Expression{user_id?: true}]
               }
             ] = result.noise_layers
    end

    test "clear numeric negative condition on uid-column" do
      result = compile!("SELECT COUNT(*) FROM table WHERE uid <> 1")

      assert [
               %{base: {"table", "uid", :<>}, expressions: [%Expression{value: 1}, _, _]},
               %{
                 base: {"table", "uid", :<>},
                 expressions: [%Expression{value: 1}, _, _, %Expression{user_id?: true}]
               }
             ] = result.noise_layers
    end

    test "clear string negative condition" do
      result = compile!("SELECT COUNT(*) FROM table WHERE name <> 'Foo'")

      assert [
               %{base: {"table", "name", :<>}, expressions: [%Expression{value: "Foo"}, _, _]},
               %{
                 base: {"table", "name", :<>},
                 expressions: [%Expression{value: "Foo"}, _, _, %Expression{user_id?: true}]
               },
               %{
                 base: {"table", "name", {:<>, :lower}},
                 expressions: [%Expression{value: "foo"}, _, _]
               }
             ] = result.noise_layers
    end

    test "string negative condition with upper" do
      result = compile!("SELECT COUNT(*) FROM table WHERE upper(name) <> 'FOO'")

      assert [
               %{base: {"table", "name", :<>}, expressions: [%Expression{value: "FOO"}, _, _]},
               %{
                 base: {"table", "name", :<>},
                 expressions: [%Expression{value: "FOO"}, _, _, %Expression{user_id?: true}]
               },
               %{
                 base: {"table", "name", {:<>, :lower}},
                 expressions: [%Expression{value: "foo"}, _, _]
               }
             ] = result.noise_layers
    end

    test "string negative condition with lower" do
      result = compile!("SELECT COUNT(*) FROM table WHERE lower(name) <> 'foo'")

      assert [
               %{base: {"table", "name", :<>}, expressions: [%Expression{value: "foo"}, _, _]},
               %{
                 base: {"table", "name", :<>},
                 expressions: [%Expression{value: "foo"}, _, _, %Expression{user_id?: true}]
               },
               %{
                 base: {"table", "name", {:<>, :lower}},
                 expressions: [%Expression{value: "foo"}, _, _]
               }
             ] = result.noise_layers
    end

    test "having of COUNT(*)" do
      result = compile!("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(*) <> 10) x")

      assert [generic_layer()] = result.noise_layers
    end

    test "having of count(distinct)" do
      result =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(distinct numeric) <> 10) x
        """)

      assert [
               %{base: {"table", "numeric", :<>}, expressions: [%Expression{}, _, _]},
               %{
                 base: {"table", "numeric", :<>},
                 expressions: [%Expression{}, _, _, %Expression{user_id?: true}]
               }
             ] = result.noise_layers
    end
  end

  describe "useless negative conditions" do
    test "overrides <> noise layers when a more specific positive one exists" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 1 AND numeric <> 2")

      assert [
               static_layer({"table", "numeric", nil}),
               uid_layer({"table", "numeric", nil}),
               static_layer({"table", "numeric", {:<>, :override}}),
               uid_layer({"table", "numeric", {:<>, :override}})
             ] = result.noise_layers
    end

    test "does not override <> noise layers when a more specific positive one exists on another column" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 1 AND numeric2 <> 2")

      assert [
               static_layer({"table", "numeric", nil}),
               uid_layer({"table", "numeric", nil}),
               static_layer({"table", "numeric2", :<>}),
               uid_layer({"table", "numeric2", :<>})
             ] = result.noise_layers
    end

    for operator <- ~w(LIKE ILIKE) do
      test "overrides NOT #{operator} noise layers when a more specific positive one exists" do
        result = compile!("SELECT MEDIAN(uid) FROM table WHERE name NOT #{unquote(operator)} 'a%b' GROUP BY name")

        assert [
                 static_layer({"table", "name", nil}),
                 uid_layer({"table", "name", nil}),
                 static_layer({"table", "name", {:not, _, _, :override}}),
                 uid_layer({"table", "name", {:not, _, _, :override}})
               ] = result.noise_layers
      end
    end

    test "overrides meaningless noise layers from subqueries" do
      result = compile!("SELECT COUNT(*) FROM (SELECT uid, numeric FROM table WHERE numeric <> 2) x WHERE numeric = 1")

      assert [
               static_layer({"table", "numeric", nil}),
               uid_layer({"table", "numeric", nil}),
               static_layer({"table", "numeric", {:<>, :override}}),
               uid_layer({"table", "numeric", {:<>, :override}})
             ] = result.noise_layers
    end

    test "overrides menaingless noise layers when a more specific positive one exists in a subquery" do
      result = compile!("SELECT COUNT(*) FROM (SELECT uid, numeric FROM table WHERE numeric = 2) x WHERE numeric <> 1")

      assert [
               static_layer({"table", "numeric", nil}),
               uid_layer({"table", "numeric", nil}),
               static_layer({"table", "numeric", {:<>, :override}}),
               uid_layer({"table", "numeric", {:<>, :override}})
             ] = result.noise_layers
    end
  end

  describe "noise layers for IS NULL" do
    test "a noise layer for IS NULL" do
      assert [
               %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}, _, _]},
               %{
                 base: {"table", "name", nil},
                 expressions: [%Expression{name: "name"}, _, _, %Expression{name: "uid"}]
               }
             ] = compile!("SELECT MEDIAN(uid) FROM table WHERE name IS NULL").noise_layers
    end

    test "a noise layer for IS NOT NULL" do
      assert [
               %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}, _, _]},
               %{
                 base: {"table", "name", nil},
                 expressions: [%Expression{name: "name"}, _, _, %Expression{name: "uid"}]
               }
             ] = compile!("SELECT MEDIAN(uid) FROM table WHERE name IS NOT NULL").noise_layers
    end

    test "no noise for IS NULL on uids",
      do: assert([generic_layer()] = compile!("SELECT COUNT(*) FROM table WHERE uid IS NULL").noise_layers)

    test "no noise for IS NOT NULL on uids",
      do: assert([generic_layer()] = compile!("SELECT COUNT(*) FROM table WHERE uid IS NOT NULL").noise_layers)
  end

  describe "noise layers for LIKE" do
    test "a noise layers in LIKE" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE name || name2 LIKE 'b%_o_%b'")
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
               %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}, _, _]},
               %{
                 base: {"table", "name", {:like, {:%, ^len, 1}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name", {:like, {:_, ^len, 1}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name", {:like, {:%, ^len, 3}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name", {:like, {:_, ^len, 3}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{base: {"table", "name2", nil}, expressions: [%Expression{name: "name2"}, _, _]},
               %{
                 base: {"table", "name2", {:like, {:%, ^len, 1}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name2", {:like, {:_, ^len, 1}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name2", {:like, {:%, ^len, 3}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name2", {:like, {:_, ^len, 3}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               }
             ] = result.noise_layers
    end

    test "noise layers in ILIKE" do
      result = compile!("SELECT MEDIAN(uid) FROM table WHERE name || name2 ILIKE 'b%_o_%b'")
      len = String.length("b%_o_%b") - String.length("%%")

      assert [
               %{base: {"table", "name", nil}, expressions: [%Expression{name: "name"}, _, _]},
               %{
                 base: {"table", "name", {:ilike, {:%, ^len, 1}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name", {:ilike, {:_, ^len, 1}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name", {:ilike, {:%, ^len, 3}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name", {:ilike, {:_, ^len, 3}}},
                 expressions: [%{name: "name"}, _, _, %{name: "uid"}]
               },
               %{base: {"table", "name2", nil}, expressions: [%Expression{name: "name2"}, _, _]},
               %{
                 base: {"table", "name2", {:ilike, {:%, ^len, 1}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name2", {:ilike, {:_, ^len, 1}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name2", {:ilike, {:%, ^len, 3}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               },
               %{
                 base: {"table", "name2", {:ilike, {:_, ^len, 3}}},
                 expressions: [%{name: "name2"}, _, _, %{name: "uid"}]
               }
             ] = result.noise_layers
    end

    test "noise layers when LIKE has no wildcards" do
      [
        %{base: base1, expressions: [%{value: "bob"}, _, _]},
        %{base: base2, expressions: [%{value: "bob"}, _, _, %{name: "uid"}]}
      ] = compile!("SELECT MEDIAN(uid) FROM table WHERE name LIKE 'bob'").noise_layers

      assert [
               %{base: ^base1, expressions: [%{value: "bob"}, _, _]},
               %{base: ^base2, expressions: [%{value: "bob"}, _, _, %{name: "uid"}]}
             ] = compile!("SELECT MEDIAN(uid) FROM table WHERE name = 'bob'").noise_layers
    end

    for column <- ~w(string uid) do
      test "noise layers when ILIKE has no wildcards (col: #{column})" do
        [
          %{base: base1, expressions: [%{name: unquote(column)}, _, _]},
          %{base: base2, expressions: [%{name: unquote(column)}, _, _, %{name: "uid"}]}
        ] =
          compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} ILIKE 'bob'").noise_layers

        assert [
                 %{base: ^base1, expressions: [%{value: "bob"}, _, _]},
                 %{base: ^base2, expressions: [%{value: "bob"}, _, _, %{name: "uid"}]}
               ] =
                 compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} = 'bob'").noise_layers
      end

      test "noise layers for NOT LIKE (col: #{column})" do
        result = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} NOT LIKE '_bob%'")

        len = String.length("_bob%") - String.length("%")

        assert [
                 %{
                   base: {"string_uid_table", column, {:not, :like, "_bob"}},
                   expressions: [%Expression{name: column}, _, _]
                 },
                 %{
                   base: {"string_uid_table", column, {:not, :like, {:_, ^len, 0}}},
                   expressions: [%{name: column}, _, _, %{name: "uid"}]
                 },
                 %{
                   base: {"string_uid_table", column, {:not, :like, {:%, ^len, 4}}},
                   expressions: [%{name: column}, _, _, %{name: "uid"}]
                 }
               ] = result.noise_layers
      end

      test "noise layers for NOT ILIKE (col: #{column})" do
        result = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} NOT ILIKE '_bob%'")

        len = String.length("_bob%") - String.length("%")

        assert [
                 %{
                   base: {"string_uid_table", column, {:not, :ilike, "_bob"}},
                   expressions: [%Expression{name: column}, _, _]
                 },
                 %{
                   base: {"string_uid_table", column, {:not, :ilike, {:_, ^len, 0}}},
                   expressions: [%{name: column}, _, _, %{name: "uid"}]
                 },
                 %{
                   base: {"string_uid_table", column, {:not, :ilike, {:%, ^len, 4}}},
                   expressions: [%{name: column}, _, _, %{name: "uid"}]
                 }
               ] = result.noise_layers
      end

      test "noise layers when NOT LIKE has no wildcards (col: #{column})" do
        result1 = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} NOT LIKE 'bob'")

        result2 = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} <> 'bob'")

        assert Enum.map(result1.noise_layers, & &1.base) == Enum.map(result2.noise_layers, & &1.base)
      end

      test "noise layers when NOT ILIKE has no wildcards (col: #{column})" do
        result1 = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} NOT ILIKE 'bOb'")

        result2 = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE lower(#{unquote(column)}) <> 'bob'")

        assert Enum.map(result1.noise_layers, & &1.base) == Enum.map(result2.noise_layers, & &1.base)
      end
    end
  end

  describe "noise layers from IN" do
    for column <- ["uid", "string"] do
      test "IN (single_value) on column #{column}" do
        [
          %{base: base1, expressions: [%{name: name}, _, _]},
          %{base: base2, expressions: [%{name: name}, _, _, %{name: "uid"}]}
        ] =
          compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} IN ('bob')").noise_layers

        assert [
                 %{base: ^base1, expressions: [%{name: ^name}, _, _]},
                 %{base: ^base2, expressions: [%{name: ^name}, _, _, %{name: "uid"}]}
               ] =
                 compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} = 'bob'").noise_layers
      end

      test "IN (many, values) on column #{column}" do
        result = compile!("SELECT MEDIAN(length(uid)) FROM string_uid_table WHERE #{unquote(column)} IN ('a', 'b')")

        assert [
                 %{
                   base: {"string_uid_table", unquote(column), nil},
                   expressions: [%{name: unquote(column)}, _, _]
                 },
                 %{
                   base: {"string_uid_table", unquote(column), nil},
                   expressions: [%{value: "a"}, _, _, %{name: "uid"}]
                 },
                 %{
                   base: {"string_uid_table", unquote(column), nil},
                   expressions: [%{value: "b"}, _, _, %{name: "uid"}]
                 }
               ] = result.noise_layers
      end

      for function <- ~w(upper lower) do
        test "#{function}(x) IN (many, values) on column #{column}" do
          result =
            compile!("""
              SELECT MEDIAN(length(uid)) FROM string_uid_table
              WHERE #{unquote(function)}(#{unquote(column)}) IN ('a', 'b')
            """)

          assert [
                   %{
                     base: {"string_uid_table", unquote(column), nil},
                     expressions: [%{name: unquote(column)}, _, _]
                   },
                   %{
                     base: {"string_uid_table", unquote(column), nil},
                     expressions: [%{value: "a"}, _, _, %{name: "uid"}]
                   },
                   %{
                     base: {"string_uid_table", unquote(column), nil},
                     expressions: [%{value: "b"}, _, _, %{name: "uid"}]
                   }
                 ] = result.noise_layers
        end
      end
    end
  end

  describe "noise layers from subqueries" do
    test "floating noise layers from a subquery" do
      result = compile!("SELECT COUNT(*) FROM (SELECT * FROM table WHERE numeric + 1 = 3) foo")

      assert [
               %{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}, _, _]},
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: name}, _, _, %Expression{user_id?: true}]
               }
             ] = result.noise_layers

      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{user_id?: true}, &1))
    end

    test "floating noise layers from a join" do
      result =
        compile!("""
          SELECT MEDIAN(numeric) FROM table JOIN (SELECT uid FROM table WHERE numeric + 1 = 3) foo ON foo.uid = table.uid
        """)

      assert [
               %{base: {"table", "numeric", nil}, expressions: [%Expression{name: name}, _, _]},
               %{
                 base: {"table", "numeric", nil},
                 expressions: [
                   %Expression{name: name},
                   _,
                   _,
                   %Expression{name: "uid", table: %{name: "table"}}
                 ]
               }
             ] = result.noise_layers

      assert name != "numeric"
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^name}, &1))

      assert 1 =
               Enum.count(
                 result.db_columns,
                 &match?(%Expression{name: "uid", table: %{name: "table"}}, &1)
               )
    end

    test "floating noise layers from an aggregating subquery" do
      result = compile!("SELECT MEDIAN(numeric) FROM (SELECT uid, numeric FROM table GROUP BY uid, numeric) foo")

      {:subquery, %{ast: subquery}} = result.from

      assert [
               %{base: {"table", "numeric", nil}, expressions: [%Expression{name: alias}, _, _]},
               %{
                 base: {"table", "numeric", nil},
                 expressions: [%Expression{name: alias}, _, _, %Expression{user_id?: true}]
               }
             ] = result.noise_layers

      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "floating columns that are not aggregated" do
      result =
        compile!("""
          SELECT MEDIAN(c) FROM (SELECT uid, COUNT(*) AS c FROM table WHERE numeric + 1 = 3 GROUP BY uid, dummy) foo
        """)

      %{from: {:subquery, %{ast: subquery}}} = result

      assert [%{alias: min_alias}] =
               Enum.filter(
                 subquery.db_columns,
                 &match?(
                   %Expression{function: "min", function_args: [%Expression{name: "numeric"}]},
                   &1
                 )
               )

      assert [%{alias: max_alias}] =
               Enum.filter(
                 subquery.db_columns,
                 &match?(
                   %Expression{function: "max", function_args: [%Expression{name: "numeric"}]},
                   &1
                 )
               )

      assert [%{alias: count_alias}] =
               Enum.filter(
                 subquery.db_columns,
                 &match?(
                   %Expression{function: "count", function_args: [:*]},
                   &1
                 )
               )

      assert 1 =
               Enum.count(
                 result.db_columns,
                 &match?(%Expression{name: ^min_alias, type: :integer}, &1)
               )

      assert 1 =
               Enum.count(
                 result.db_columns,
                 &match?(%Expression{name: ^max_alias, type: :integer}, &1)
               )

      assert 2 =
               Enum.count(
                 result.db_columns,
                 &match?(%Expression{name: ^count_alias, type: :integer}, &1)
               )

      assert 1 =
               Enum.count(
                 result.noise_layers,
                 &match?(
                   %{
                     base: {"table", "numeric", nil},
                     expressions: [
                       %Expression{name: ^min_alias},
                       %Expression{name: ^max_alias},
                       %Expression{name: ^count_alias}
                     ]
                   },
                   &1
                 )
               )
    end

    test "* expansion doesn't include the carry columns" do
      result = compile!("SELECT * FROM (SELECT uid, numeric as n FROM table GROUP BY uid, numeric) foo")

      assert [%Expression{value: :*}, %Expression{name: "__ac_group_0"}] = result.columns
    end
  end

  describe "noise layers from nested subqueries" do
    test "floating columns from non-aggregating subqueries" do
      result =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid, numeric FROM (SELECT uid, numeric FROM table WHERE numeric = 3) foo) bar
        """)

      assert [
               %{base: {"table", "numeric", nil}},
               %{base: {"table", "numeric", nil}, expressions: [_, _, _, %{user_id?: true}]}
             ] = result.noise_layers
    end

    test "floating complex noise layers through non-aggregating queries" do
      result =
        compile!("""
          SELECT MEDIAN(uid) FROM (SELECT * FROM
            (SELECT uid FROM table WHERE numeric + 1 = 3 GROUP BY uid, dummy) foo
          ) bar
        """)

      %{from: {:subquery, %{ast: subquery}}} = result
      %{from: {:subquery, %{ast: inner_subquery}}} = subquery

      assert [%{alias: min_alias}] =
               Enum.filter(
                 inner_subquery.db_columns,
                 &match?(
                   %Expression{function: "min", function_args: [%Expression{name: "numeric"}]},
                   &1
                 )
               )

      assert [%{alias: max_alias}] =
               Enum.filter(
                 inner_subquery.db_columns,
                 &match?(
                   %Expression{function: "max", function_args: [%Expression{name: "numeric"}]},
                   &1
                 )
               )

      assert [%{alias: count_alias}] =
               Enum.filter(
                 inner_subquery.db_columns,
                 &match?(
                   %Expression{function: "count", function_args: [:*]},
                   &1
                 )
               )

      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(subquery.db_columns, &match?(%Expression{name: ^count_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^count_alias}, &1))

      assert 1 =
               Enum.count(
                 result.noise_layers,
                 &match?(
                   %{
                     base: {"table", "numeric", nil},
                     expressions: [
                       %Expression{name: ^min_alias},
                       %Expression{name: ^max_alias},
                       %Expression{name: ^count_alias}
                     ]
                   },
                   &1
                 )
               )
    end

    test "floating columns that are not aggregated" do
      result =
        compile!("""
          SELECT COUNT(DISTINCT dummy) FROM (SELECT uid, dummy FROM
            (SELECT uid, dummy FROM table WHERE numeric + 1 = 3 GROUP BY uid, dummy, dummy2) foo
          GROUP BY uid, dummy) bar
        """)

      %{from: {:subquery, %{ast: subquery}}} = result
      %{from: {:subquery, %{ast: inner_subquery}}} = subquery

      assert [%{alias: min_alias}] =
               Enum.filter(
                 inner_subquery.db_columns,
                 &match?(
                   %Expression{function: "min", function_args: [%Expression{name: "numeric"}]},
                   &1
                 )
               )

      assert [%{alias: max_alias}] =
               Enum.filter(
                 inner_subquery.db_columns,
                 &match?(
                   %Expression{function: "max", function_args: [%Expression{name: "numeric"}]},
                   &1
                 )
               )

      assert [%{alias: count_alias}] =
               Enum.filter(
                 inner_subquery.db_columns,
                 &match?(
                   %Expression{function: "count", function_args: [:*]},
                   &1
                 )
               )

      assert [%{alias: min_alias}] =
               Enum.filter(
                 subquery.db_columns,
                 &match?(
                   %Expression{function: "min", function_args: [%Expression{name: ^min_alias}]},
                   &1
                 )
               )

      assert [%{alias: max_alias}] =
               Enum.filter(
                 subquery.db_columns,
                 &match?(
                   %Expression{function: "max", function_args: [%Expression{name: ^max_alias}]},
                   &1
                 )
               )

      assert [%{alias: count_alias}] =
               Enum.filter(
                 subquery.db_columns,
                 &match?(
                   %Expression{function: "sum", function_args: [%Expression{name: ^count_alias}]},
                   &1
                 )
               )

      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^min_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^max_alias}, &1))
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: ^count_alias}, &1))

      assert 1 =
               Enum.count(
                 result.noise_layers,
                 &match?(
                   %{
                     base: {"table", "numeric", nil},
                     expressions: [
                       %Expression{name: ^min_alias},
                       %Expression{name: ^max_alias},
                       %Expression{name: ^count_alias}
                     ]
                   },
                   &1
                 )
               )
    end

    test "floating aggregated boolean columns " do
      result =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid FROM
            (SELECT uid FROM table WHERE dummy = true GROUP BY uid, dummy) foo
          GROUP BY uid) bar
        """)

      %{from: {:subquery, %{ast: subquery}}} = result

      assert 1 =
               Enum.count(
                 subquery.db_columns,
                 &match?(
                   %Expression{
                     function: {:cast, :boolean},
                     function_args: [
                       %Expression{
                         function: "min",
                         function_args: [
                           %Expression{
                             function: {:cast, :integer},
                             function_args: [%Expression{type: :boolean}]
                           }
                         ]
                       }
                     ]
                   },
                   &1
                 )
               )

      assert 1 =
               Enum.count(
                 subquery.db_columns,
                 &match?(
                   %Expression{
                     function: {:cast, :boolean},
                     function_args: [
                       %Expression{
                         function: "max",
                         function_args: [
                           %Expression{
                             function: {:cast, :integer},
                             function_args: [%Expression{type: :boolean}]
                           }
                         ]
                       }
                     ]
                   },
                   &1
                 )
               )
    end
  end

  describe "noise layer base data" do
    test "insensitive to being aliased" do
      %{noise_layers: [%{base: base}, %{base: base}]} =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid, numeric as foo FROM table) bar WHERE foo = 3
        """)

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in views" do
      %{noise_layers: [%{base: base}, %{base: base}]} =
        compile!(
          "SELECT count(*) FROM foo WHERE bar = 3",
          views: %{"foo" => "SELECT uid, numeric AS bar FROM table"}
        )

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased after operations" do
      %{noise_layers: [%{base: b1}, %{base: b2}, %{base: b3}, %{base: b4}]} =
        compile!("SELECT COUNT(*) FROM (SELECT uid, numeric + numeric2 as foo FROM table) bar WHERE foo = 3")

      assert [{"table", "numeric", nil}, {"table", "numeric2", nil}] = [b1, b2, b3, b4] |> Enum.uniq() |> Enum.sort()
    end

    test "insensitive to being aliased in nested subqueries" do
      %{noise_layers: [%{base: base}, %{base: base}]} =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid, foo as bar FROM (SELECT uid, numeric AS foo FROM table) x) y WHERE bar = 3
        """)

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in a join" do
      %{noise_layers: [%{base: base}, %{base: base}]} =
        compile!("""
          SELECT COUNT(*) FROM other JOIN (
            SELECT uid, numeric AS foo FROM table
          ) bar
          ON other.uid = bar.uid WHERE foo = 3
        """)

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to being aliased in emulated queries" do
      %{noise_layers: [%{base: base}, %{base: base}]} =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid, upper(name) AS bar FROM table) foo WHERE bar = 'a'
        """)

      assert {"table", "name", nil} = base
    end

    test "insensitive to the query casing" do
      %{noise_layers: [%{base: base1}, %{base: base1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3")

      %{noise_layers: [%{base: base2}, %{base: base2}]} = compile!("SELECT COUNT(*) FROM table WHERE nUmErIc = 3")

      assert base1 == base2
    end

    test "insensitive to being quoted" do
      %{noise_layers: [%{base: base1}, %{base: base1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3")

      %{noise_layers: [%{base: base2}, %{base: base2}]} = compile!(~s[SELECT COUNT(*) FROM table WHERE "numeric" = 3])

      assert base1 == base2
    end

    test "insensitive to being scoped" do
      %{noise_layers: [%{base: base1}, %{base: base1}]} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3")

      %{noise_layers: [%{base: base2}, %{base: base2}]} = compile!("SELECT COUNT(*) FROM table WHERE table.numeric = 3")

      assert base1 == base2
    end

    test "insensitive to the table being aliased" do
      %{noise_layers: [%{base: base}, %{base: base}]} = compile!("SELECT COUNT(*) FROM table AS t WHERE numeric = 3")

      assert {"table", "numeric", nil} = base
    end

    test "insensitive to the table being aliased in subquery" do
      %{noise_layers: [%{base: base}, %{base: base}]} =
        compile!("""
          SELECT COUNT(*) FROM (SELECT uid, numeric FROM table AS t) x WHERE numeric = 3
        """)

      assert {"table", "numeric", nil} = base
    end
  end

  test "performance sanity check" do
    %{db_columns: db_columns} =
      compile!("""
        SELECT MEDIAN(numeric) FROM (
          SELECT uid, numeric FROM table AS t WHERE numeric BETWEEN 0 AND 1000000 GROUP BY 1, 2
        ) x
      """)

    assert 4 = length(db_columns)
  end

  test "[Issue #2395] range noise layer shouldn't override equality noise layer" do
    %{noise_layers: layers1} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 10 AND numeric BETWEEN 1 AND 2")
    %{noise_layers: layers2} = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3")
    %{noise_layers: layers3} = compile!("SELECT COUNT(*) FROM table WHERE numeric BETWEEN 1 AND 2")

    result1 = layers1 |> Enum.map(& &1.base) |> Enum.sort()
    result2 = (layers2 ++ layers3) |> Enum.map(& &1.base) |> Enum.sort()
    assert result1 == result2
  end

  defp compile!(query, opts \\ []),
    do:
      Cloak.Test.QueryHelpers.compile!(query, data_source(), opts)
      |> Cloak.Sql.Compiler.NoiseLayers.compile()
      |> Cloak.Sql.Query.resolve_db_columns()

  defp data_source() do
    %{
      name: "noise_layers_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("numeric", :integer),
              Table.column("numeric2", :integer),
              Table.column("dummy", :boolean),
              Table.column("dummy2", :boolean),
              Table.column("name", :text),
              Table.column("name2", :text),
              Table.column("id", :integer)
            ],
            keys: %{"id" => :unknown}
          ),
        other:
          Cloak.DataSource.Table.new(
            "other",
            "uid",
            db_name: "other",
            columns: [Table.column("uid", :integer)]
          ),
        camel_table:
          Cloak.DataSource.Table.new(
            "camelTable",
            "uid",
            db_name: "camelTable",
            columns: [Table.column("uid", :integer), Table.column("camelColumn", :integer)]
          ),
        key_table:
          Cloak.DataSource.Table.new(
            "key_table",
            "uid",
            db_name: "key_table",
            columns: [Table.column("uid", :integer), Table.column("table_id", :integer)],
            keys: %{"table_id" => :unknown}
          ),
        string_uid_table:
          Cloak.DataSource.Table.new(
            "string_uid_table",
            "uid",
            db_name: "string_uid_table",
            columns: [
              Table.column("uid", :text),
              Table.column("string", :text)
            ]
          ),
        userless:
          Cloak.DataSource.Table.new(
            "userless",
            nil,
            db_name: "userless",
            columns: [
              Table.column("value", :integer),
              Table.column("name", :text)
            ]
          )
      }
    }
  end
end
