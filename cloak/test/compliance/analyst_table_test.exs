defmodule Compliance.AnalystTableTest do
  use ComplianceCase, async: true
  alias Cloak.AnalystTable
  import Cloak.Test.QueryHelpers

  @moduletag :compliance
  @moduletag :analyst_tables
  @tested_data_sources ~w(oracle postgresql9.4 postgresql)

  setup do
    for data_source <- tested_data_sources(),
        table_name <- stored_tables(data_source),
        do: drop_table!(data_source, table_name)

    :ok
  end

  for data_source_name <- @tested_data_sources do
    describe "#{data_source_name}" do
      test "table can be created" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          assert {:ok, _} =
                   AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
        end
      end

      test "same query and id produce the same db_name" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          name = AnalystTable.table_definition!(1, "foo", data_source).db_name

          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          assert %{db_name: ^name} = AnalystTable.table_definition!(1, "foo", data_source)
        end
      end

      test "different query leads to a different db_name" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          name1 = AnalystTable.table_definition!(1, "foo", data_source).db_name

          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age > 70", data_source)
          name2 = AnalystTable.table_definition!(1, "foo", data_source).db_name

          assert name1 != name2
        end
      end

      test "different id leads to a different db_name" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          name1 = AnalystTable.table_definition!(1, "foo", data_source).db_name

          {:ok, _} = AnalystTable.store(2, "foo", "select user_id, height from users where age < 70", data_source)
          name2 = AnalystTable.table_definition!(2, "foo", data_source).db_name

          assert name1 != name2
        end
      end

      test "stored table contains desired rows" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          expected = select_direct!(1, data_source, "select user_id, height from users where age < 70")
          materialized = select_direct!(1, data_source, "select user_id, height from foo")

          assert materialized == expected
        end
      end

      test "analyst table can be queried" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)

          assert_query(
            "select * from foo",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["user_id", "height"]}
          )
        end
      end

      test "simple table definition" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, sqrt(age), height as h from users", data_source)

          table_definition = AnalystTable.table_definition!(1, "foo", data_source)

          assert table_definition.name == "foo"
          assert String.starts_with?(table_definition.db_name, "__ac_")
          assert table_definition.user_id == "user_id"

          assert table_definition.columns == [
                   %{name: "user_id", type: :integer, visible?: true},
                   %{name: "sqrt", type: :real, visible?: true},
                   %{name: "h", type: :real, visible?: true}
                 ]
        end
      end

      test "table definition in select all" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select * from users", data_source)

          table_definition = AnalystTable.table_definition!(1, "foo", data_source)

          assert table_definition.name == "foo"
          assert String.starts_with?(table_definition.db_name, "__ac_")
          assert table_definition.user_id == "user_id"
          assert table_definition.columns == Cloak.DataSource.table(data_source, :users).columns
        end
      end

      test "analyst_tables returns all tables of the given analyst" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, _} = AnalystTable.store(1, "foo", "select user_id, height from users where age < 70", data_source)
          table1 = AnalystTable.table_definition!(1, "foo", data_source)

          {:ok, _} = AnalystTable.store(1, "bar", "select user_id, height from users where age < 70", data_source)
          table2 = AnalystTable.table_definition!(1, "bar", data_source)

          {:ok, _} = AnalystTable.store(2, "bar", "select user_id, height from users where age < 70", data_source)
          _table3 = AnalystTable.table_definition!(2, "baz", data_source)

          assert Enum.sort_by(AnalystTable.analyst_tables(1, data_source), & &1.name) ==
                   Enum.sort_by([table1, table2], & &1.name)
        end
      end

      test "analyst table registration" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, registration_info} = AnalystTable.store(1, "foo", "select user_id from users", data_source)
          drop_table!(data_source, AnalystTable.table_definition!(1, "foo", data_source).db_name)
          assert AnalystTable.register_tables([registration_info]) == :ok

          assert_query(
            "select * from foo",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["user_id"]}
          )
        end
      end

      test "obsolete analyst tables are dropped when tables are registered" do
        with {:ok, data_source} <- Cloak.DataSource.fetch(unquote(data_source_name)) do
          {:ok, info1} = AnalystTable.store(1, "foo", "select user_id as a from users", data_source)
          {:ok, info2} = AnalystTable.store(1, "foo", "select user_id as b from users", data_source)
          {:ok, info3} = AnalystTable.store(1, "foo", "select user_id as c from users", data_source)
          {:ok, info4} = AnalystTable.store(2, "bar", "select user_id from users", data_source)

          db_name1 = AnalystTable.table_definition!(1, "foo", data_source).db_name
          db_name2 = AnalystTable.table_definition!(2, "bar", data_source).db_name

          :ok = AnalystTable.register_tables([info1, info2, info3, info4])

          assert Enum.sort(stored_tables(data_source)) == Enum.sort([db_name1, db_name2])
        end
      end
    end
  end

  defp tested_data_sources(), do: Enum.filter(Cloak.DataSource.all(), &(&1.name in @tested_data_sources))

  defp drop_table!(data_source, table_name) do
    quote_char = data_source.driver.sql_dialect_module.quote_char()
    quoted_table_name = Cloak.DataSource.SqlBuilder.quote_table_name(table_name, quote_char)
    execute!(data_source, "drop table #{quoted_table_name}")
  end

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

  defp select_direct!(analyst_id, data_source, statement) do
    statement
    |> Cloak.Sql.Parser.parse!()
    |> Cloak.Sql.Compiler.compile_direct!(analyst_id, data_source)
    |> Cloak.Query.DbEmulator.select()
  end

  defp stored_tables(data_source),
    do: Cloak.DataSource.Connection.execute!(data_source, &data_source.driver.analyst_tables/1)
end
