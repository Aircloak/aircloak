defmodule Cloak.Sql.Compiler.NoiseLayer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler, Parser}

  describe "picking columns for noise layers" do
    test "lists no noise layers by default" do
      assert %{noise_layers: []} = compile!("SELECT COUNT(*) FROM table", data_source())
    end

    test "lists columns filtered with WHERE"

    test "lists columns filtered with GROUP BY"

    test "lists columns filtered with JOIN"

    test "lists underlying columns when a function is applied"

    test "lists all columns when multiple filters are applied"
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
          columns: [{"uid", :integer}, {"numeric", :integer}],
          projection: nil
        }
      }
    }
  end
end
