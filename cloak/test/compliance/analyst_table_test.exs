defmodule Compliance.AnalystTableTest do
  use ComplianceCase, async: true
  alias Cloak.AnalystTable

  @moduletag :compliance
  @moduletag :analyst_tables
  @tested_data_sources ~w(oracle postgresql9.4 postgresql)

  setup do
    for data_source <- tested_data_sources(),
        table_name <- AnalystTable.stored(data_source),
        quote_char = data_source.driver.sql_dialect_module.quote_char(),
        quoted_table_name = Cloak.DataSource.SqlBuilder.quote_table_name(table_name, quote_char),
        do: execute!(data_source, "drop table #{quoted_table_name}")

    :ok
  end

  for data_source_name <- @tested_data_sources do
    describe "#{data_source_name}" do
      test "all returns an empty list when there are no tables" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)),
             do: assert(AnalystTable.stored(data_source) == [])
      end

      test "all returns the list of the created tables" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, table1} = AnalystTable.new(1, "select user_id, height from users where age < 70", data_source)
          :ok = AnalystTable.store(table1)

          {:ok, table2} = AnalystTable.new(2, "select user_id, height from users where age < 70", data_source)
          :ok = AnalystTable.store(table2)

          assert Enum.sort(AnalystTable.stored(data_source)) ==
                   [table1, table2] |> Enum.map(&AnalystTable.name/1) |> Enum.sort()
        end
      end

      test "table can be created" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, table} = AnalystTable.new(1, "select user_id, height from users where age < 70", data_source)
          assert AnalystTable.store(table) == :ok
        end
      end

      test "table can be created multiple times" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, table} = AnalystTable.new(1, "select user_id, height from users where age < 70", data_source)
          assert AnalystTable.store(table) == :ok
          assert AnalystTable.store(table) == :ok
        end
      end

      test "stored table contains desired rows" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, table} = AnalystTable.new(1, "select user_id, height from users where age < 70", data_source)
          :ok = AnalystTable.store(table)

          materialized =
            data_source
            |> select!("select * from #{quote_table_name(table)}")
            |> Enum.map(fn [user_id, height] -> [to_integer(user_id), height] end)

          expected = select_direct!(data_source, "select user_id, height from users where age < 70")
          assert materialized == expected
        end
      end
    end
  end

  defp tested_data_sources(), do: Enum.filter(Cloak.DataSource.all(), &(&1.name in @tested_data_sources))

  defp execute!(data_source, statement) do
    Cloak.DataSource.Connection.execute!(
      data_source,
      fn connection ->
        case data_source.driver do
          Cloak.DataSource.PostgreSQL -> Postgrex.query!(connection, statement, [])
          Cloak.DataSource.Oracle -> Cloak.DataSource.RODBC.execute_direct!(connection, statement)
        end
      end
    )
  end

  defp select!(data_source, statement) do
    case data_source.driver do
      Cloak.DataSource.PostgreSQL -> execute!(data_source, statement).rows
      Cloak.DataSource.Oracle -> Enum.to_list(execute!(data_source, statement))
    end
  end

  defp select_direct!(data_source, statement) do
    parsed = Cloak.Sql.Parser.parse!(statement)
    compiled = Cloak.Sql.Compiler.compile_direct!(parsed, data_source)
    Cloak.Query.DbEmulator.select(compiled)
  end

  defp quote_table_name(%AnalystTable{} = table), do: quote_table_name(table.data_source, AnalystTable.name(table))

  defp quote_table_name(data_source, name) do
    quote_char = data_source.driver.sql_dialect_module.quote_char()
    Cloak.DataSource.SqlBuilder.quote_table_name(name, quote_char)
  end

  defp to_integer(int) when is_integer(int), do: int
  defp to_integer(string) when is_binary(string), do: String.to_integer(string)
end
