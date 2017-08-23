defmodule Cloak.DataSource.SAPHanaTest do
  use ExUnit.Case, async: true

  if :os.type() == {:unix, :darwin} do
    IO.puts "Warning: SAP HANA tests can't be executed on macOS. Use `make dev-container` instead."
  else
    @schema String.upcase("TEST_SCHEMA_#{Base.encode16(:crypto.strong_rand_bytes(10))}")

    @moduletag :saphana
    @moduletag :exclude_in_dev

    defmacrop assert_query(data_source, query, expected_response) do
      quote do
        result = Cloak.Query.Runner.run_sync("#{:erlang.unique_integer([:positive])}", unquote(data_source),
          unquote(query), [], %{})
        assert unquote(expected_response) = result
      end
    end

    setup_all do
      setup_test_schema()
      on_exit(&drop_test_schema/0)
      {:ok, %{data_source: data_source()}}
    end

    test "basic select", context do
      assert_query(context.data_source, "select int_value from test", %{rows: [
        %{row: [1], occurrences: 10},
        %{row: [2], occurrences: 5}
      ]})
    end

    for {name, subquery} <- (
      [{"subquery", "select uid, int_value as value from test"}] ++
      Enum.map(~w(count sum min max avg stddev),
        &{&1, "select uid, #{&1}(int_value) as value from test group by uid"}) ++
      Enum.map(~w(year quarter month day hour minute second weekday),
        &{&1, "select uid, #{&1}(datetime_value) as value from time group by uid, datetime_value"})
    ) do
      test "#{name} is not emulated", context do
        query = "select sq.value from (#{unquote(subquery)}) sq"

        {:ok, %{from: {:subquery, %{ast: subquery}}}} = compile_query(context.data_source, query)
        assert subquery.emulated? == false

        assert_query(context.data_source, query, %{rows: _})
      end
    end

    defp setup_test_schema() do
      conn = connect!()
      Cloak.SapHanaHelpers.ensure_schema!(conn, @schema)

      Cloak.SapHanaHelpers.recreate_table!(conn, @schema, "TEST", "UID integer, INT_VALUE integer")
      Cloak.SapHanaHelpers.insert_rows!(conn, @schema, "TEST", ["UID", "INT_VALUE"],
        for {value, uids} <- %{1 => 1..10, 2 => 1..5, 3 => 1..1}, uid <- uids do
          [uid, value]
        end
      )

      Cloak.SapHanaHelpers.recreate_table!(conn, @schema, "TIME", "UID integer, DATETIME_VALUE datetime")
      Cloak.SapHanaHelpers.insert_rows!(conn, @schema, "TIME", ["UID", "DATETIME_VALUE"],
        for {value, uids} <- %{~c(timestamp'2017-08-23 01:02:03') => 1..10}, uid <- uids do
          [uid, value]
        end
      )
    end

    defp drop_test_schema() do
      conn = connect!()
      {:updated, _} = Cloak.SapHanaHelpers.execute(conn, "drop schema #{@schema} CASCADE")
      :ok
    end

    defp connection_params(), do:
      Map.new(Application.fetch_env!(:cloak, :sap_hana))

    defp connect!() do
      params = connection_params()
      {:ok, conn} = Cloak.SapHanaHelpers.connect(params.hostname, params.port, params.username, params.password,
        params.database)
      conn
    end

    defp data_source(), do:
      Cloak.DataSource.add_tables(%{
        name: "saphana_test",
        driver: Cloak.DataSource.SAPHana,
        driver_dialect: :saphana,
        parameters: connection_params(),
        tables: [],
        initial_tables: %{
          test: %{name: "test", db_name: "#{@schema}.TEST", user_id: "UID", ignore_unsupported_types: true},
          time: %{name: "time", db_name: "#{@schema}.TIME", user_id: "UID", ignore_unsupported_types: true},
        },
        initial_errors: [],
      })

    defp compile_query(data_source, query) do
      with {:ok, parsed} <- Cloak.Sql.Parser.parse(query), do:
        Cloak.Sql.Compiler.compile(data_source, parsed, [], %{})
    end
  end
end
