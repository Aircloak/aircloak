defmodule Compliance.AnalystTableTest do
  use ComplianceCase, async: true
  alias Cloak.AnalystTable
  import Cloak.Test.QueryHelpers
  import Aircloak.AssertionHelper

  @moduletag :compliance
  @moduletag :analyst_tables
  @tested_data_sources ~w(oracle postgresql9.4 postgresql)

  for data_source_name <- @tested_data_sources do
    describe "#{data_source_name}" do
      test "table can be created" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          assert {:ok, _, _} =
                   recreate_table(1, "view1", "select user_id, height from users where age < 70", data_source)
        end
      end

      test "same query and id produce the same db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view2", "select user_id, height from users where age < 70", data_source)
          name = AnalystTable.table_definition(1, "view2", data_source).db_name

          {:ok, _, _} = recreate_table(1, "view2", "select user_id, height from users where age < 70", data_source)
          assert %{db_name: ^name} = AnalystTable.table_definition(1, "view2", data_source)
        end
      end

      test "different query leads to a different db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view3", "select user_id, height from users where age < 70", data_source)
          name1 = AnalystTable.table_definition(1, "view3", data_source).db_name

          {:ok, _, _} = recreate_table(1, "view3", "select user_id, height from users where age > 70", data_source)
          name2 = AnalystTable.table_definition(1, "view3", data_source).db_name

          assert name1 != name2
        end
      end

      test "different id leads to a different db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view4", "select user_id, height from users where age < 70", data_source)
          name1 = AnalystTable.table_definition(1, "view4", data_source).db_name

          {:ok, _, _} = recreate_table(2, "view4", "select user_id, height from users where age < 70", data_source)
          name2 = AnalystTable.table_definition(2, "view4", data_source).db_name

          assert name1 != name2
        end
      end

      test "stored table contains desired rows" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view5", "select user_id, height from users where age < 70", data_source)
          expected = select_direct!(1, data_source, "select user_id, height from users where age < 70")
          materialized = select_direct!(1, data_source, "select user_id, height from view5")

          assert materialized == expected
        end
      end

      test "analyst table can be queried" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view6", "select user_id, height from users where age < 70", data_source)

          assert_query(
            "select * from view6",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["user_id", "height"]}
          )
        end
      end

      test "simple table definition" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view7", "select user_id, sqrt(age), height as h from users", data_source)

          table_definition = AnalystTable.table_definition(1, "view7", data_source)

          assert table_definition.name == "view7"
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
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view8", "select * from users", data_source)

          table_definition = AnalystTable.table_definition(1, "view8", data_source)

          assert table_definition.name == "view8"
          assert String.starts_with?(table_definition.db_name, "__ac_")
          assert table_definition.user_id == "user_id"
          assert table_definition.columns == Cloak.DataSource.table(data_source, :users).columns
        end
      end

      test "analyst_tables returns all tables of the given analyst" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = recreate_table(1, "view9", "select user_id, height from users where age < 70", data_source)
          {:ok, _, _} = recreate_table(1, "view10", "select user_id, height from users where age < 70", data_source)
          {:ok, _, _} = recreate_table(2, "view11", "select user_id, height from users where age < 70", data_source)

          assert Enum.sort(Enum.map(AnalystTable.analyst_tables(1, data_source), & &1.name)) == ~w(view10 view9)
        end
      end

      test "analyst table registration" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, registration_info, _} = recreate_table(1, "view12", "select user_id from users", data_source)
          drop_table!(data_source, AnalystTable.table_definition(1, "view12", data_source).db_name)

          assert AnalystTable.register_tables([registration_info]) == :ok
          assert soon(table_created?(1, "view12", data_source), :timer.seconds(5), repeat_wait_time: 10)

          assert_query(
            "select * from view12",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["user_id"]}
          )
        end
      end

      test "obsolete analyst tables are dropped when tables are registered" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _info, _} = recreate_table(1, "view13", "select user_id as a from users", data_source)
          {:ok, _info, _} = recreate_table(1, "view13", "select user_id as b from users", data_source)
          {:ok, info1, _} = recreate_table(1, "view13", "select user_id as c from users", data_source)
          {:ok, info2, _} = recreate_table(2, "view14", "select user_id from users", data_source)

          db_name1 = AnalystTable.table_definition(1, "view13", data_source).db_name
          db_name2 = AnalystTable.table_definition(2, "view14", data_source).db_name
          :ok = AnalystTable.register_tables([info1, info2])

          assert soon(Enum.sort(stored_tables(data_source)) == Enum.sort([db_name1, db_name2]), 5000)
        end
      end

      test "failed analyst table creation" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, registration_info, _} = recreate_table(1, "view15", "select user_id from users", data_source)
          clear_analyst_tables(data_source)

          log =
            ExUnit.CaptureLog.capture_log(fn ->
              registration_info =
                registration_info
                |> Jason.decode!()
                |> put_in(["store_info"], "foo bar baz")
                |> Jason.encode!()

              AnalystTable.register_tables([registration_info])

              assert soon(
                       table_created?(1, "view15", data_source, :create_error),
                       :timer.seconds(5),
                       repeat_wait_time: 10
                     )
            end)

          assert log =~ ~r/Error creating table.*view15/

          assert_query(
            "select * from view15",
            [analyst_id: 1, data_sources: [data_source]],
            %{error: "An error happened while creating the table `view15`."}
          )
        end
      end

      test "pending creation" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, registration_info, _} = recreate_table(1, "view16", "select user_id, name from users", data_source)
          clear_analyst_tables(data_source)

          registration_info =
            registration_info
            |> Jason.decode!()
            |> update_in(
              ["store_info"],
              &String.replace(&1, ~r/AS SELECT/, "AS SELECT -1, pg_sleep(1)::text UNION SELECT")
            )
            |> Jason.encode!()

          AnalystTable.register_tables([registration_info])

          assert_query(
            "select * from view16",
            [analyst_id: 1, data_sources: [data_source]],
            %{error: "The table `view16` is still being created."}
          )
        end
      end

      test "columns information" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, columns} = recreate_table(1, "view17", "select user_id, height from users", data_source)

          assert Enum.sort_by(columns, & &1.name) == [
                   %{name: "height", type: "real", user_id: false},
                   %{name: "user_id", type: "integer", user_id: true}
                 ]
        end
      end

      test "table is not automatically updated if the underlying table is changed" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          execute!(data_source, "delete from users where user_id = -1")

          {:ok, _, _} = recreate_table(1, "view18", "select user_id, height from users", data_source)
          execute!(data_source, "insert into users(user_id) values (-1)")

          assert select_direct!(1, data_source, "select count(*) from view18 where user_id = -1") == [[0]]
        end
      end

      test "table is repopulated on recreate" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          execute!(data_source, "delete from users where user_id = -1")

          {:ok, _, _} = recreate_table(1, "view19", "select user_id, height from users", data_source)
          execute!(data_source, "insert into users(user_id) values (-1)")
          {:ok, _, _} = recreate_table(1, "view19", "select user_id, height from users", data_source)

          assert select_direct!(1, data_source, "select count(*) from view19 where user_id = -1") == [[1]]
        end
      end
    end
  end

  defp prepare_data_source(data_source_name) do
    with {:ok, data_source} <- Cloak.DataSource.fetch(data_source_name) do
      clear_analyst_tables(data_source)
      {:ok, data_source}
    end
  end

  defp clear_analyst_tables(data_source) do
    AnalystTable.sync_serialized(fn ->
      data_source |> stored_tables() |> Enum.each(&drop_table!(data_source, &1))
      :ets.match_delete(AnalystTable, {{:_, data_source.name, :_}, :_})
    end)
  end

  defp recreate_table(analyst_id, name, statement, data_source) do
    with {:ok, registration_info, columns} <- AnalystTable.recreate(analyst_id, name, statement, data_source) do
      assert soon(table_created?(analyst_id, name, data_source), :timer.seconds(5), repeat_wait_time: 10)
      {:ok, registration_info, columns}
    end
  end

  defp table_created?(analyst_id, name, data_source, expected_status \\ :created) do
    with table_definition <- AnalystTable.table_definition(analyst_id, name, data_source),
         false <- is_nil(table_definition),
         ^expected_status <- table_definition.status,
         do: true,
         else: (_ -> false)
  end

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
