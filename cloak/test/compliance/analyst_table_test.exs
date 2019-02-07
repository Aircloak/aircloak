defmodule Compliance.AnalystTableTest do
  use ComplianceCase, async: false
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
                   create_or_update(1, "table1", "select user_id, height from users where age < 70", data_source)
        end
      end

      test "same query and id produce the same db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table2", "select user_id, height from users where age < 70", data_source)
          name = AnalystTable.table_definition(1, "table2", data_source).db_name

          {:ok, _, _} = create_or_update(1, "table2", "select user_id, height from users where age < 70", data_source)
          assert %{db_name: ^name} = AnalystTable.table_definition(1, "table2", data_source)
        end
      end

      test "different query leads to a different db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table3", "select user_id, height from users where age < 70", data_source)
          name1 = AnalystTable.table_definition(1, "table3", data_source).db_name

          {:ok, _, _} = create_or_update(1, "table3", "select user_id, height from users where age > 70", data_source)
          name2 = AnalystTable.table_definition(1, "table3", data_source).db_name

          assert name1 != name2
        end
      end

      test "different id leads to a different db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table4", "select user_id, height from users where age < 70", data_source)
          name1 = AnalystTable.table_definition(1, "table4", data_source).db_name

          {:ok, _, _} = create_or_update(2, "table4", "select user_id, height from users where age < 70", data_source)
          name2 = AnalystTable.table_definition(2, "table4", data_source).db_name

          assert name1 != name2
        end
      end

      test "stored table contains desired rows" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table5", "select user_id, height from users where age < 70", data_source)
          expected = select_direct!(1, data_source, "select user_id, height from users where age < 70")
          materialized = select_direct!(1, data_source, "select user_id, height from table5")

          assert materialized == expected
        end
      end

      test "analyst table can be queried" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table6", "select user_id, height from users where age < 70", data_source)

          assert_query(
            "select * from table6",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["user_id", "height"]}
          )
        end
      end

      test "simple table definition" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table7", "select user_id, sqrt(age), height as h from users", data_source)

          table_definition = AnalystTable.table_definition(1, "table7", data_source)

          assert table_definition.name == "table7"
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
          {:ok, _, _} = create_or_update(1, "table8", "select * from users", data_source)

          table_definition = AnalystTable.table_definition(1, "table8", data_source)

          assert table_definition.name == "table8"
          assert String.starts_with?(table_definition.db_name, "__ac_")
          assert table_definition.user_id == "user_id"
          assert table_definition.columns == Cloak.DataSource.table(data_source, :users).columns
        end
      end

      test "analyst_tables returns all tables of the given analyst" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table9", "select user_id, height from users where age < 70", data_source)
          {:ok, _, _} = create_or_update(1, "table10", "select user_id, height from users where age < 70", data_source)
          {:ok, _, _} = create_or_update(2, "table11", "select user_id, height from users where age < 70", data_source)

          assert Enum.sort(Enum.map(AnalystTable.analyst_tables(1, data_source), & &1.name)) == ~w(table10 table9)
        end
      end

      test "obsolete analyst tables are dropped when tables are registered" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _info, _} = create_or_update(1, "table13", "select user_id as a from users", data_source)
          {:ok, _info, _} = create_or_update(1, "table13", "select user_id as b from users", data_source)
          {:ok, info1, _} = create_or_update(1, "table13", "select user_id as c from users", data_source)
          {:ok, info2, _} = create_or_update(2, "table14", "select user_id from users", data_source)

          db_name1 = AnalystTable.table_definition(1, "table13", data_source).db_name
          db_name2 = AnalystTable.table_definition(2, "table14", data_source).db_name
          :ok = AnalystTable.register_tables([info1, info2])

          assert soon(Enum.sort(stored_tables(data_source)) == Enum.sort([db_name1, db_name2]), 5000)
        end
      end

      test "columns information" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, columns} = create_or_update(1, "table17", "select user_id, height from users", data_source)

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

          {:ok, _, _} = create_or_update(1, "table18", "select user_id, height from users", data_source)

          execute!(data_source, "insert into users(user_id) values (-1)")

          try do
            assert select_direct!(1, data_source, "select count(*) from table18 where user_id = -1") == [[0]]
          after
            execute!(data_source, "delete from users where user_id = -1")
          end
        end
      end

      test "table is repopulated on recreate" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          execute!(data_source, "delete from users where user_id = -1")

          {:ok, _, _} = create_or_update(1, "table19", "select user_id, height from users", data_source)
          execute!(data_source, "insert into users(user_id) values (-1)")

          try do
            {:ok, _, _} = create_or_update(1, "table19", "select user_id, height from users", data_source)

            assert select_direct!(1, data_source, "select count(*) from table19 where user_id = -1") == [[1]]
          after
            execute!(data_source, "delete from users where user_id = -1")
          end
        end
      end

      test "tables can be queried after registration" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, table_info1, _} = create_or_update(1, "table20", "select user_id from users", data_source)
          {:ok, table_info2, _} = create_or_update(1, "table21", "select user_id from users", data_source)

          :ets.delete_all_objects(AnalystTable)

          assert AnalystTable.register_tables([table_info1, table_info2]) == :ok
          assert soon(table_created?(1, "table20", data_source), :timer.seconds(5), repeat_wait_time: 10)
          assert soon(table_created?(1, "table21", data_source), :timer.seconds(5), repeat_wait_time: 10)

          assert_query("select * from table20", [analyst_id: 1, data_sources: [data_source]], %{columns: ["user_id"]})
          assert_query("select * from table21", [analyst_id: 1, data_sources: [data_source]], %{columns: ["user_id"]})
        end
      end

      test "obsolete tables are deleted after registration" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, table_info, _} = create_or_update(1, "table22", "select user_id from users", data_source)
          {:ok, _, _} = create_or_update(1, "table23", "select user_id from users", data_source)

          db_name = AnalystTable.table_definition(1, "table22", data_source).db_name

          :ets.delete_all_objects(AnalystTable)

          assert AnalystTable.register_tables([table_info]) == :ok
          assert AnalystTable.table_definition(1, "table23", data_source) == nil
          assert soon(stored_tables(data_source) == [db_name], 5000)
        end
      end

      test "table can reference another table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _, _} = create_or_update(1, "table24", "select user_id, height from users", data_source)
          {:ok, _, _} = create_or_update(1, "table25", "select * from table24", data_source)

          assert_query(
            "select * from table25",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["user_id", "height"]}
          )
        end
      end

      test "reporting an error when querying a table which is still being created" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          AnalystTable.with_custom_store_fun(
            fn _ -> Process.sleep(:timer.seconds(1)) end,
            fn ->
              {:ok, _, _} = AnalystTable.create_or_update(1, "table26", "select user_id from users", data_source)

              assert_query(
                "select * from table26",
                [analyst_id: 1, data_sources: [data_source]],
                %{error: "The table `table26` is still being created."}
              )
            end
          )
        end
      end

      test "reporting an error if the table creation failed" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          AnalystTable.with_custom_store_fun(
            fn _ -> {:error, "some error"} end,
            fn ->
              log =
                ExUnit.CaptureLog.capture_log(fn ->
                  {:ok, _, _} = AnalystTable.create_or_update(1, "table27", "select user_id from users", data_source)

                  assert soon(
                           table_created?(1, "table27", data_source, :create_error),
                           :timer.seconds(5),
                           repeat_wait_time: 10
                         )
                end)

              assert log =~ ~r/Error creating table.*some error/

              assert_query(
                "select * from table27",
                [analyst_id: 1, data_sources: [data_source]],
                %{error: "An error happened while creating the table `table27`."}
              )
            end
          )
        end
      end

      test "invoking create_or_update kills the store process for the same table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          me = self()

          AnalystTable.with_custom_store_fun(
            fn real_store ->
              send(me, {:create_process, self()})
              Process.sleep(:timer.seconds(1))
              real_store.()
            end,
            fn ->
              {:ok, _, _} = AnalystTable.create_or_update(1, "table28", "select user_id from users", data_source)
              assert_receive {:create_process, pid}
              mref = Process.monitor(pid)

              {:ok, _, _} = create_or_update(1, "table28", "select user_id from users", data_source)

              assert_receive {:DOWN, ^mref, :process, ^pid, :killed}, :timer.seconds(1)

              assert_query(
                "select * from table28",
                [analyst_id: 1, data_sources: [data_source]],
                %{columns: ["user_id"]}
              )
            end
          )
        end
      end

      test "registration while the table is being stored" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          AnalystTable.with_custom_store_fun(
            fn real_store ->
              Process.sleep(:timer.seconds(1))
              real_store.()
            end,
            fn ->
              {:ok, table_info, _} =
                AnalystTable.create_or_update(1, "table29", "select user_id from users", data_source)

              assert AnalystTable.register_tables([table_info]) == :ok
              assert soon(table_created?(1, "table29", data_source), :timer.seconds(5), repeat_wait_time: 10)

              assert_query(
                "select * from table29",
                [analyst_id: 1, data_sources: [data_source]],
                %{columns: ["user_id"]}
              )
            end
          )
        end
      end

      test "storing of an obsolete table is stopped during registration" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          me = self()

          AnalystTable.with_custom_store_fun(
            fn real_store ->
              send(me, {:create_process, self()})
              Process.sleep(:timer.seconds(1))
              real_store.()
            end,
            fn ->
              {:ok, _, _} = AnalystTable.create_or_update(1, "table30", "select user_id from users", data_source)

              assert_receive {:create_process, pid}
              mref = Process.monitor(pid)

              assert AnalystTable.register_tables([]) == :ok
              assert_receive {:DOWN, ^mref, :process, ^pid, :killed}, :timer.seconds(1)

              assert soon(
                       is_nil(AnalystTable.table_definition(1, "table30", data_source)),
                       :timer.seconds(5),
                       repeat_wait_time: 10
                     )
            end
          )
        end
      end

      test "table can't be created if the functionality isn't enabled" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          data_source = %{data_source | analyst_tables_enabled: false}

          assert_raise(
            RuntimeError,
            "analyst tables are not enabled on this data source",
            fn -> create_or_update(1, "table1", "select *from users", data_source) end
          )
        end
      end

      test "table can't be created if the driver doesn't support analyst tables" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          data_source = %{data_source | driver: Cloak.DataSource.MongoDB}

          assert_raise(
            RuntimeError,
            "analyst tables are not supported on this data source.",
            fn -> create_or_update(1, "table1", "select *from users", data_source) end
          )
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

  defp create_or_update(analyst_id, name, statement, data_source) do
    with {:ok, registration_info, columns} <- AnalystTable.create_or_update(analyst_id, name, statement, data_source) do
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
