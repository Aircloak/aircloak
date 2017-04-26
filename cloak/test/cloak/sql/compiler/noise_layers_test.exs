defmodule Cloak.Sql.Compiler.NoiseLayer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler, Parser, Expression}

  describe "picking columns for noise layers" do
    test "lists no noise layers by default" do
      assert [] = compile!("SELECT COUNT(*) FROM table", data_source()).noise_layers
    end

    test "lists columns filtered with WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source())

      assert [[%Expression{name: "numeric"}]] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with GROUP BY" do
      result = compile!("SELECT numeric, COUNT(*) FROM table GROUP BY numeric", data_source())

      assert [[%Expression{name: "numeric"}]] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with JOIN" do
      result = compile!(
        "SELECT COUNT(*) FROM table JOIN other ON table.numeric = 3 AND table.uid = other.uid",
        data_source()
      )

      assert [[%Expression{name: "numeric"}]] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "lists columns filtered with emulated WHERE" do
      result = compile!("SELECT COUNT(*) FROM table WHERE decoded = 'a'", data_source())

      assert [[%Expression{name: "decoded"}]] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "decoded"}, &1))
    end

    test "lists underlying columns when a function is applied" do
      result = compile!("SELECT COUNT(*) FROM table GROUP BY BUCKET(numeric BY 10)", data_source())

      assert [[%Expression{name: "numeric"}]] = result.noise_layers
      assert Enum.any?(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "multiple filters on one column" do
      result = compile!("SELECT COUNT(*) FROM table WHERE numeric = 3 GROUP BY BUCKET(numeric BY 10)", data_source())

      assert [[%Expression{name: "numeric"}], [%Expression{name: "numeric"}]] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end
  end

  describe "noise layers from subqueries" do
    test "floating noise layers from a subquery" do
      result = compile!("SELECT COUNT(*) FROM (SELECT * FROM table WHERE numeric = 3) foo", data_source())

      assert [[%Expression{name: "numeric"}]] = result.noise_layers
      assert 1 = Enum.count(result.db_columns, &match?(%Expression{name: "numeric"}, &1))
    end

    test "floating noise layers from an aggregating subquery" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid, numeric FROM table GROUP BY uid, numeric) foo",
      data_source())

      assert 1 = Enum.count(result.noise_layers, &match?([%Expression{name: "numeric"}], &1))
    end

    test "floating columns that are not aggregated" do
      result = compile!(
        "SELECT COUNT(*) FROM (SELECT uid FROM table WHERE numeric = 3 GROUP BY uid, decoded) foo",
      data_source())

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
      assert 1 = Enum.count(result.noise_layers, &match?([
        %Expression{name: ^min_alias},
        %Expression{name: ^max_alias},
        %Expression{name: ^count_alias}
      ], &1))
    end
  end

  test "noise layers from nested subqueries"

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
          columns: [{"uid", :integer}, {"numeric", :integer}, {"decoded", :text}],
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
