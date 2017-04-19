defmodule Cloak.Sql.Compiler.NoiseLayer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler, Parser, Expression}

  describe "picking columns for noise layers" do
    test "lists no noise layers by default" do
      assert [] = compile!("SELECT COUNT(*) FROM table", data_source()).noise_layers
    end

    test "lists columns filtered with WHERE" do
      assert [%Expression{name: "numeric"}] =
        compile!("SELECT COUNT(*) FROM table WHERE numeric = 3", data_source()).noise_layers
    end

    test "lists columns filtered with GROUP BY" do
      assert [%Expression{name: "numeric"}] =
        compile!("SELECT numeric, COUNT(*) FROM table GROUP BY numeric", data_source()).noise_layers
    end

    test "lists columns filtered with JOIN" do
      assert [%Expression{name: "numeric"}] =
        compile!(
          "SELECT COUNT(*) FROM table JOIN other ON table.numeric = 3 AND table.uid = other.uid",
          data_source()
        ).noise_layers
    end

    test "lists columns filtered with emulated WHERE" do
      assert [%Expression{name: "decoded"}] =
        compile!("SELECT COUNT(*) FROM table WHERE decoded = 'a'", data_source()).noise_layers
    end

    test "lists underlying columns when a function is applied"

    test "lists all columns when multiple filters are applied"
  end

  test "noise layer for a column that's filtered but not aggregated"

  test "noise layers when there are multiple filters for one column"

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
