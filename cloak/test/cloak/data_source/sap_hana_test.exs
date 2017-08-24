defmodule Cloak.DataSource.SAPHanaTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Runner

  if :os.type() == {:unix, :darwin} do
    IO.puts "Warning: SAP HANA tests can't be executed on macOS. Use `make dev-container` instead."
  else
    @schema String.upcase("TEST_SCHEMA_#{Base.encode16(:crypto.strong_rand_bytes(10))}")

    @moduletag :saphana
    @moduletag :exclude_in_dev

    defmacrop assert_query(data_source, query, expected_response) do
      quote do
        result = Runner.run_sync("#{:erlang.unique_integer([:positive])}", unquote(data_source), unquote(query), [],
          %{})
        assert unquote(expected_response) = result
      end
    end

    setup_all do
      setup_test_schema()
      on_exit(&drop_test_schema/0)
      {:ok, %{data_source: data_source()}}
    end

    test "basic select", context do
      assert_query(context.data_source, "select value from ints", %{rows: [
        %{row: [1], occurrences: 10},
        %{row: [2], occurrences: 5}
      ]})
    end

    test "default string decoding", context, do:
      assert_query(context.data_source, "select value from strings", %{rows: [%{row: ["a string value"]}]})

    test "default datetime decoding", context, do:
      assert_query(context.data_source, "select value from times", %{rows: [%{row: ["2017-08-23T01:02:03.000000"]}]})

    test "inner join", context, do:
      assert_query(context.data_source,
        "select ints.value as i, strings.value as s from ints inner join strings on ints.uid = strings.uid",
        %{rows: [%{row: [1, "a string value"]}, %{row: [2, "a string value"]}]}
      )

    test "non emulated functions", context do
      # We're running these tests concurrently from a single test. The reason is that we need to execute a lot of
      # queries on the remote SAP HANA database, and running these queries sequentially can be quite slow.
      # Therefore, we're issuing multiple queries from separate tasks, using chunking to ensure we don't run
      # too many queries at the same time.
      errors =
        [
          [{"subquery", "select uid, value from ints"}],
          Enum.map(~w(count sum min max avg stddev), &{&1, "select uid, #{&1}(value) as value from ints group by uid"}),
          Enum.map(~w(year quarter month day hour minute second weekday),
            &{&1, "select uid, #{&1}(value) as value from times group by uid, value"}),
          Enum.map(~w(sqrt floor ceil abs round), &{&1, "select uid, #{&1}(value) as value from ints"}),
          [{"mod", "select uid, mod(value, 2) as value from ints"}],
          Enum.map(~w(% * / + - ^), &{&1, "select uid, (value #{&1} 2) as value from ints"}),
          Enum.map(~w(length lower upper btrim ltrim rtrim), &{&1, "select uid, #{&1}(value) as value from strings"}),
          Enum.map(~w(ltrim rtrim), &{&1, "select uid, #{&1}(value, ' ') as value from strings"}),
          Enum.map(~w(left right), &{&1, "select uid, #{&1}(value, 1) as value from strings"}),
          [{"substring", "select uid, substring(value from 1) as value from strings"}],
          [{"substring/2", "select uid, substring(value from 1 for 1) as value from strings"}],
          [{"substring_for", "select uid, substring_for(value, 1) as value from strings"}],
          [{"bucket", "select uid, bucket(value by 2) as value from ints"}],
          [{"cast", "select uid, cast(value as text) as value from ints"}],
        ]
        |> Stream.concat()
        |> Stream.chunk(10, 10, [])
        |> Stream.map(fn(tests) -> Enum.map(tests, &Task.async(fn -> verify_native(context.data_source, &1) end)) end)
        |> Stream.map(fn(tasks) -> Enum.map(tasks, &Task.await(&1, :timer.seconds(30))) end)
        |> Stream.concat()
        |> Enum.reject(&is_nil/1)

      case errors do
        [] -> :ok
        [_|_] -> flunk(Enum.join(errors, "\n\n"))
      end
    end

    defp setup_test_schema() do
      conn = connect!()
      Cloak.SapHanaHelpers.ensure_schema!(conn, @schema)

      [
        fn ->
          conn = connect!()
          Cloak.SapHanaHelpers.recreate_table!(conn, @schema, "INTS", "UID integer, VALUE integer")
          Cloak.SapHanaHelpers.insert_rows!(conn, @schema, "INTS", ["UID", "VALUE"],
            for {value, uids} <- %{1 => 1..10, 2 => 1..5, 3 => 1..1}, uid <- uids do
              [uid, value]
            end
          )
        end,
        fn ->
          conn = connect!()
          Cloak.SapHanaHelpers.recreate_table!(conn, @schema, "TIMES", "UID integer, VALUE datetime")
          Cloak.SapHanaHelpers.insert_rows!(conn, @schema, "TIMES", ["UID", "VALUE"],
            for {value, uids} <- %{~c(timestamp'2017-08-23 01:02:03') => 1..10}, uid <- uids do
              [uid, value]
            end
          )
        end,
        fn ->
          conn = connect!()
          Cloak.SapHanaHelpers.recreate_table!(conn, @schema, "STRINGS", "UID integer, VALUE nvarchar(100)")
          Cloak.SapHanaHelpers.insert_rows!(conn, @schema, "STRINGS", ["UID", "VALUE"],
            for {value, uids} <- %{~c('a string value') => 1..10}, uid <- uids do
              [uid, value]
            end
          )
        end
      ]
      |> Enum.map(&Task.async/1)
      |> Enum.map(&Task.await(&1, :timer.seconds(30)))
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
          ints: %{name: "ints", db_name: "#{@schema}.INTS", user_id: "UID", ignore_unsupported_types: true},
          times: %{name: "times", db_name: "#{@schema}.TIMES", user_id: "UID", ignore_unsupported_types: true},
          strings: %{name: "strings", db_name: "#{@schema}.STRINGS", user_id: "UID", ignore_unsupported_types: true},
        },
        initial_errors: [],
      })

    defp compile_query(data_source, query) do
      with {:ok, parsed} <- Cloak.Sql.Parser.parse(query), do:
        Cloak.Sql.Compiler.compile(data_source, parsed, [], %{})
    end

    defp verify_native(data_source, {function, subquery}) do
      query = "select sq.value from (#{subquery}) sq"

      {:ok, %{from: {:subquery, %{ast: subquery}}}} = compile_query(data_source, query)

      if subquery.emulated? do
        "subquery using `#{function}` is emulated"
      else
        case Runner.run_sync("#{:erlang.unique_integer([:positive])}", data_source, query, [], %{}) do
          %{error: error} -> "error running a query using `#{function}`: #{error}"
          %{rows: _} -> nil
        end
      end
    end
  end
end
