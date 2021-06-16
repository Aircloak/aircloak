defmodule Compliance.AnalystTableTest do
  use ComplianceCase, async: false
  alias Cloak.AnalystTable
  alias Cloak.Test.AnalystTableHelpers
  import Cloak.Test.QueryHelpers
  import Aircloak.AssertionHelper
  import AnalystTableHelpers, only: [create_or_update: 4, create_or_update: 5, table_created?: 4]

  @moduletag :compliance
  @moduletag :analyst_tables
  @tested_data_sources ~w(oracle postgresql)

  setup_all do
    air_name = "some_air_instance"
    Cloak.Air.register_air(air_name)
    on_exit(&Cloak.Air.unregister_air/0)
    {:ok, %{air_name: air_name}}
  end

  for data_source_name <- @tested_data_sources do
    describe "#{data_source_name}" do
      test "table can be created" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          assert {:ok, _} =
                   create_or_update(
                     1,
                     "table1",
                     "select user_id, height from users where age between 0 and 100",
                     data_source
                   )
        end
      end

      test "same query and id produce the same db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table2", "select user_id, height from users", data_source)
          name = AnalystTable.find(1, "table2", data_source).db_name

          {:ok, _} = create_or_update(1, "table2", "select user_id, height from users", data_source)
          assert %{db_name: ^name} = AnalystTable.find(1, "table2", data_source)
        end
      end

      test "different id leads to a different db_name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table4", "select user_id, height from users", data_source)
          name1 = AnalystTable.find(1, "table4", data_source).db_name

          {:ok, _} = create_or_update(2, "table4", "select user_id, height from users", data_source)
          name2 = AnalystTable.find(2, "table4", data_source).db_name

          assert name1 != name2
        end
      end

      test "stored table contains desired rows" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} =
            create_or_update(1, "table5", "select user_id, height from users where age between 0 and 100", data_source)

          expected = select_direct!(1, data_source, "select user_id, height from users where age between 0 and 100")
          materialized = select_direct!(1, data_source, "select user_id, height from table5")

          assert materialized == expected
        end
      end

      test "analyst table can be queried" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} =
            create_or_update(
              1,
              "table6",
              "select user_id as uid, height from users where age between 0 and 100",
              data_source
            )

          assert_query(
            "select height from table6",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["height"]}
          )
        end
      end

      test "simple table definition" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table7", "select user_id, sqrt(age), height as h from users", data_source)

          table_definition = AnalystTable.find(1, "table7", data_source)

          assert table_definition.name == "table7"
          assert String.starts_with?(table_definition.db_name, "__ac_")
        end
      end

      test "table definition in select all" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table8", "select * from users", data_source)

          table_definition = AnalystTable.find(1, "table8", data_source)

          assert table_definition.name == "table8"
          assert String.starts_with?(table_definition.db_name, "__ac_")
        end
      end

      test "analyst_tables returns all tables of the given analyst" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table9", "select user_id, height from users", data_source)
          {:ok, _} = create_or_update(1, "table10", "select user_id, height from users", data_source)
          {:ok, _} = create_or_update(2, "table11", "select user_id, height from users", data_source)

          assert Enum.sort(Enum.map(AnalystTable.analyst_tables(1, data_source), & &1.name)) == ~w(table10 table9)
        end
      end

      test "columns information" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, columns} = create_or_update(1, "table17", "select user_id, height from users", data_source)

          assert Enum.sort_by(columns, & &1.name) == [
                   %{
                     name: "height",
                     type: "real",
                     key_type: nil,
                     access: "visible",
                     comment: "This is column height."
                   },
                   %{
                     name: "user_id",
                     type: "integer",
                     key_type: "user_id",
                     access: "visible",
                     comment: "This is column user_id."
                   }
                 ]
        end
      end

      test "table is not automatically updated if the underlying table is changed" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          execute!(data_source, "delete from users where user_id = -1")

          {:ok, _} = create_or_update(1, "table18", "select user_id, height from users", data_source)

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

          {:ok, _} = create_or_update(1, "table19", "select user_id, height from users", data_source)
          execute!(data_source, "insert into users(user_id) values (-1)")

          try do
            {:ok, _} = create_or_update(1, "table19", "select user_id, height from users", data_source)

            assert select_direct!(1, data_source, "select count(*) from table19 where user_id = -1") == [[1]]
          after
            execute!(data_source, "delete from users where user_id = -1")
          end
        end
      end

      test "table can reference another table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table24", "select user_id, height from users", data_source)
          {:ok, _} = create_or_update(1, "table25", "select * from table24", data_source)

          assert_query(
            "select height from table25",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["height"]}
          )
        end
      end

      test "reporting an error when querying a table which is still being created" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          AnalystTable.with_custom_store_fun(
            fn _ -> Process.sleep(:timer.seconds(1)) end,
            fn ->
              {:ok, _} =
                AnalystTable.create_or_update(1, "table26", nil, "select user_id from users", data_source, nil, %{})

              assert_query(
                "select from table26",
                [analyst_id: 1, data_sources: [data_source]],
                %{error: "analyst table `table26` is still being created"}
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
                  {:ok, _} =
                    AnalystTable.create_or_update(1, "table27", nil, "select user_id from users", data_source, nil, %{})

                  assert_soon(
                    table_created?(1, "table27", data_source, :create_error),
                    timeout: :timer.seconds(5)
                  )
                end)

              assert log =~ ~r/Error creating analyst table.*some error/

              assert_query(
                "select from table27",
                [analyst_id: 1, data_sources: [data_source]],
                %{error: "creation of analyst table `table27` failed"}
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
              {:ok, _} =
                AnalystTable.create_or_update(1, "table28", nil, "select user_id from users", data_source, nil, %{})

              assert_receive {:create_process, pid}
              mref = Process.monitor(pid)

              {:ok, _} = create_or_update(1, "table28", "select user_id from users", data_source)

              assert_receive {:DOWN, ^mref, :process, ^pid, :killed}, :timer.seconds(1)

              assert_query(
                "select from table28",
                [analyst_id: 1, data_sources: [data_source]],
                %{columns: []}
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
          data_source = %{data_source | driver: Cloak.DataSource.SQLServer}

          assert_raise(
            RuntimeError,
            "analyst tables are not supported on this data source.",
            fn -> create_or_update(1, "table1", "select *from users", data_source) end
          )
        end
      end

      test "table is registered in the meta table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          assert {:ok, _} = create_or_update(1, "table31", "select * from users", data_source)
          db_name = AnalystTable.find(1, "table31", data_source).db_name
          assert Enum.member?(registered_tables(data_source), db_name)
        end
      end

      test "dropping a table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          assert {:ok, _} = create_or_update(1, "table32", "select * from users", data_source)
          db_name = AnalystTable.find(1, "table32", data_source).db_name
          assert AnalystTable.drop_tables(1, data_source.name, ["table32"]) == :ok
          refute Enum.member?(registered_tables(data_source), db_name)
          refute Enum.member?(AnalystTableHelpers.stored_tables(data_source), db_name)
          assert AnalystTable.find(1, "table32", data_source) == nil
        end
      end

      test "properly storing long queries" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          comments =
            "-- #{"foobar" |> List.duplicate(10) |> to_string()}"
            |> List.duplicate(100)
            |> Enum.join("\n")

          statement = """
            #{comments}
            SELECT
              user_id,
              height
            FROM users
          """

          assert {:ok, _} = create_or_update(1, "table33", statement, data_source)
          assert AnalystTable.find(1, "table33", data_source).statement == statement
        end
      end

      test "registered_analyst_tables", context do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table34", "select * from users", data_source)
          {:ok, _} = create_or_update(1, "table35", "select * from users", data_source)

          registered_tables = Enum.map(AnalystTable.registered_analyst_tables(context.air_name, data_source), & &1.name)
          assert Enum.sort(registered_tables) == ["table34", "table35"]
        end
      end

      test "reporting anonymization error in a combination of user query and the analyst table SQL" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table36", "select user_id as uid, sqrt(height) as h1 from users", data_source)

          assert_query(
            """
            select count(*) from table36 where
            round(h1) = 10
            """,
            [analyst_id: 1, data_sources: [data_source]],
            %{error: error}
          )

          assert error =~ ~r/Implicit ranges.*at line 2, column 1/s
        end
      end

      test "querying an analyst table under an alias" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table37", "select user_id, height from users", data_source)

          assert_query(
            "select y.height from table37 x
            inner join table37 y on x.user_id = y.user_id
            where x.height between 0 and 200 and y.height between 0 and 200",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["height"]}
          )
        end
      end

      test "table is included in show tables" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table38", "select * from users", data_source)

          assert_query(
            "show tables",
            [analyst_id: 1, data_sources: [data_source], analyst_tables: %{"table38" => %{}}],
            %{rows: rows}
          )

          assert Enum.any?(rows, &(&1.row == ["table38", "personal", nil]))
        end
      end

      test "table works with show columns" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table39", "select * from users", data_source)

          assert_query("show columns from table39", [analyst_id: 1, data_sources: [data_source]], %{rows: rows})
          names = Enum.map(rows, fn %{row: [name, _type, _isolator_status, _key, _comment]} -> name end)

          assert MapSet.new(names) ==
                   MapSet.new(~w/active age birthday column_with_a_very_long_name height id
                   name_unicode nullable signed_float signed_integer user_id/s)
        end
      end

      test "circular references are not allowed" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, _} = create_or_update(1, "table40", "select * from users", data_source)
          {:ok, _} = create_or_update(1, "table41", "select * from table40", data_source)
          {:ok, _} = create_or_update(1, "table42", "select * from table41", data_source)

          assert {:error, error} = create_or_update(1, "table40", "select * from table42", data_source)
          assert error =~ ~r/.*table40.*dependency cycle/
        end
      end

      test "when a query references an analyst table, the data is selected directly from the db table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, _} = create_or_update(1, "table43", "select user_id, height from users", data_source)
          db_name = AnalystTable.find(1, "table43", data_source).db_name

          execute!(data_source, ~s/truncate table "#{db_name}"/)

          assert_query(
            "select count(*) from table43",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["count"], rows: [%{occurrences: 1, row: [0]}]}
          )
        end
      end

      test "if a fingerprint has changed, the table can't be queried" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, _} = create_or_update(1, "table44", "select user_id, height from users", data_source)
          make_table_stale(data_source, "table44")

          assert_query(
            "select count(*) from table44",
            [analyst_id: 1, data_sources: [data_source]],
            %{error: "table `table44` needs to be updated before it can be queried"}
          )
        end
      end

      test "noise layer columns are not reported" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          {:ok, [%{name: "user_id"}, %{name: "height"}]} =
            create_or_update(
              1,
              "table45",
              "select user_id, height from users where name_unicode LIKE '%Bob%'",
              data_source
            )
        end
      end

      test "analyst table is resolved before emulator" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, _} =
            create_or_update(
              1,
              "table46",
              "select user_id as uid from notes n inner join users u on u.id = n.user_fk",
              data_source
            )

          assert_query(
            "select bar.foo from (select table46.uid, stddev(1) as foo from table46 group by 1) as bar",
            [analyst_id: 1, data_sources: [data_source]],
            %{columns: ["foo"], rows: [%{row: [0.0]}, %{row: [nil]}]}
          )
        end
      end

      test "aliasing analyst table" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, _} =
            create_or_update(
              1,
              "table47",
              "select user_id as uid from addresses a inner join users u on u.id = a.user_fk",
              data_source
            )

          assert {:ok, _} =
                   create_or_update(
                     1,
                     "table48",
                     "select foo.uid, count(foo.uid) from table47 as foo group by 1",
                     data_source
                   )
        end
      end

      test "previous table is dropped on table rename" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, _} = create_or_update(1, "table49", "select * from users", data_source)

          db_name = AnalystTable.find(1, "table49", data_source).db_name
          assert {:ok, _} = create_or_update(1, "table50", "table49", "select * from users", data_source)

          refute Enum.member?(registered_tables(data_source), db_name)
          refute Enum.member?(AnalystTableHelpers.stored_tables(data_source), db_name)

          assert_query(
            "select from table49",
            [analyst_id: 1, data_sources: [data_source]],
            %{error: "Table `table49` doesn't exist." <> _}
          )
        end
      end

      test "reporting a correct analyst table name" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
             true <- String.starts_with?(data_source.name, "postgresql") do
          {:ok, _} = create_or_update(1, "table51", "select user_id, height from users", data_source)
          make_table_stale(data_source, "table51")

          assert_query(
            "select count(*) from table51 as another_name",
            [analyst_id: 1, data_sources: [data_source]],
            %{error: "table `table51` needs to be updated before it can be queried"}
          )
        end
      end

      for pseudoconstant <- ~w/current_time current_date current_datetime/ do
        test "usage of #{pseudoconstant} is forbidden" do
          with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)),
               true <- String.starts_with?(data_source.name, "postgresql") do
            assert {:error, error} =
                     create_or_update(
                       1,
                       "table52",
                       "select user_id, #{unquote(pseudoconstant)}() from users",
                       data_source
                     )

            assert error == "Function `#{unquote(pseudoconstant)}` is not allowed when creating a table"
          end
        end
      end

      test "[Issue 3730] pseudoconstant verification works with *" do
        with {:ok, data_source} <- prepare_data_source(unquote(data_source_name)) do
          assert {:ok, _} =
                   create_or_update(1, "table53", "select user_id, count(*) from users group by 1", data_source)
        end
      end
    end
  end

  defp make_table_stale(data_source, table_name) do
    # set a different fingerprint and reprime analyst table state
    db_name = AnalystTable.find(1, table_name, data_source).db_name
    execute!(data_source, "update __ac_analyst_tables_1 set fingerprint='' where db_name = '#{db_name}'")
    AnalystTable.refresh()

    # dummy state fetching to ensure that refresh has finished
    :sys.get_state(AnalystTable)

    :ok
  end

  defp registered_tables(data_source) do
    Cloak.DataSource.Connection.execute!(
      data_source,
      &data_source.driver.select!(
        &1,
        "SELECT #{quote_identifier("db_name", data_source)} FROM
        #{quote_identifier("__ac_analyst_tables_1", data_source)}"
      )
    )
    |> Enum.map(fn [name] -> name end)
  end

  defp quote_identifier(identifier, data_source),
    do: Cloak.DataSource.SqlBuilder.quote_table_name(identifier, data_source.driver.sql_dialect_module.quote_char())

  defp prepare_data_source(data_source_name) do
    with {:ok, data_source} <- Cloak.DataSource.fetch(data_source_name) do
      AnalystTableHelpers.clear_analyst_tables(data_source)
      {:ok, data_source}
    end
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
    |> Cloak.Query.DbEmulator.compile()
    |> Cloak.Query.DbEmulator.select()
  end
end
