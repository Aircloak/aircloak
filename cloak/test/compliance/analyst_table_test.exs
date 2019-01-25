defmodule Compliance.AnalystTableTest do
  use ComplianceCase, async: true
  alias Cloak.AnalystTable

  @moduletag :compliance
  @moduletag :analyst_tables
  @tested_data_sources ~w(oracle postgresql9.4 postgresql)

  setup do
    for data_source <- tested_data_sources(),
        table_name <- stored_tables(data_source),
        quote_char = data_source.driver.sql_dialect_module.quote_char(),
        quoted_table_name = Cloak.DataSource.SqlBuilder.quote_table_name(table_name, quote_char),
        do: execute!(data_source, "drop table #{quoted_table_name}")

    :ok
  end

  for data_source_name <- @tested_data_sources do
    describe "#{data_source_name}" do
      test "table can be created" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          assert :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
        end
      end

      test "same query and id produce the same db_name" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          {:ok, %{db_name: name}} = AnalystTable.table_definition(1, "foo", data_source)

          :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          assert {:ok, %{db_name: ^name}} = AnalystTable.table_definition(1, "foo", data_source)
        end
      end

      test "different query leads to a different db_name" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          {:ok, %{db_name: name1}} = AnalystTable.table_definition(1, "foo", data_source)

          :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age > 70", data_source)
          {:ok, %{db_name: name2}} = AnalystTable.table_definition(1, "foo", data_source)

          assert name1 != name2
        end
      end

      test "different id leads to a different db_name" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          {:ok, %{db_name: name1}} = AnalystTable.table_definition(1, "foo", data_source)

          :ok = AnalystTable.store(2, "foo", "select user_id, height from users where age < 70", data_source)
          {:ok, %{db_name: name2}} = AnalystTable.table_definition(2, "foo", data_source)

          assert name1 != name2
        end
      end

      test "stored table contains desired rows" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          {:ok, table} = AnalystTable.table_definition(1, "foo", data_source)

          materialized =
            data_source
            |> select!("select * from #{quote_table_name(data_source, table.db_name)}")
            |> Enum.map(fn [user_id, height] -> [to_integer(user_id), height] end)

          expected = select_direct!(data_source, "select user_id, height from users where age < 70")
          assert materialized == expected
        end
      end

      test "simple table definition" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select user_id, sqrt(age) from users", data_source)

          assert {:ok, table_definition} = AnalystTable.table_definition(1, "foo", data_source)
          assert table_definition.name == "foo"
          assert String.starts_with?(table_definition.db_name, "__ac_")
          assert table_definition.user_id == "user_id"

          assert table_definition.columns == [
                   %{name: "user_id", type: :integer, visible?: true},
                   %{name: "sqrt", type: :real, visible?: true}
                 ]
        end
      end

      test "table definition in select all" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select * from users", data_source)

          assert {:ok, table_definition} = AnalystTable.table_definition(1, "foo", data_source)
          assert table_definition.name == "foo"
          assert String.starts_with?(table_definition.db_name, "__ac_")
          assert table_definition.user_id == "user_id"

          assert table_definition.columns == [
                   %{visible?: true, name: "active", type: :boolean},
                   %{visible?: true, name: "age", type: :integer},
                   %{name: "birthday", type: :date, visible?: true},
                   %{name: "column_with_a_very_long_name", type: :text, visible?: true},
                   %{name: "height", type: :real, visible?: true},
                   %{name: "id", type: :integer, visible?: true},
                   %{name: "name", type: :text, visible?: true},
                   %{name: "nullable", type: :real, visible?: true},
                   %{name: "user_id", type: :integer, visible?: true}
                 ]
        end
      end

      test "table definition error" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          :ok = AnalystTable.store(1, "foo", "select user_id, sqrt(age) from users", data_source)
          data_source = update_in(data_source.tables, &Map.delete(&1, :users))

          assert {:error, "Table `users` doesn't exist."} = AnalystTable.table_definition(1, "foo", data_source)
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
    statement
    |> Cloak.Sql.Parser.parse!()
    |> Cloak.Sql.Compiler.compile_direct!(data_source)
    |> Cloak.Query.DbEmulator.select()
  end

  defp quote_table_name(data_source, name) do
    quote_char = data_source.driver.sql_dialect_module.quote_char()
    Cloak.DataSource.SqlBuilder.quote_table_name(name, quote_char)
  end

  defp to_integer(int) when is_integer(int), do: int
  defp to_integer(string) when is_binary(string), do: String.to_integer(string)

  defp stored_tables(data_source),
    do: Cloak.DataSource.Connection.execute!(data_source, &data_source.driver.analyst_tables/1)
end
