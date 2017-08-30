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
      tables_def = setup_test_schema()
      on_exit(&drop_test_schema/0)
      {:ok, %{data_source: data_source(tables_def)}}
    end

    test "basic select", context do
      assert_query(context.data_source, "select value from ints", %{rows: [
        %{row: [1], occurrences: 10},
        %{row: [2], occurrences: 5}
      ]})
    end

    test "default nvarchar decoding", context, do:
      assert_query(context.data_source, "select value from strings", %{rows: [%{row: ["a string value"]}]})

    test "default varchar decoding", context, do:
      assert_query(context.data_source, "select cast(value as text) from varchars",
        %{rows: [%{row: ["a string value"]}]})

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
          [{"subquery",
            "select uid, value from ints"}],
          Enum.map(~w(count sum min max avg stddev),
            &{&1, "select uid, #{&1}(value) as value from ints group by uid"}),
          Enum.map(~w(year quarter month day hour minute second weekday),
            &{&1, "select uid, #{&1}(value) as value from times group by uid, value"}),
          Enum.map(~w(sqrt floor ceil abs round),
            &{&1, "select uid, #{&1}(value) as value from ints"}),
          [{"mod",
            "select uid, mod(value, 2) as value from ints"}],
          Enum.map(~w(% * / + - ^),
            &{&1, "select uid, (value #{&1} 2) as value from ints"}),
          Enum.map(~w(length lower upper btrim ltrim rtrim),
            &{&1, "select uid, #{&1}(value) as value from strings"}),
          Enum.map(~w(ltrim rtrim),
            &{&1, "select uid, #{&1}(value, ' ') as value from strings"}),
          Enum.map(~w(left right),
            &{&1, "select uid, #{&1}(value, 1) as value from strings"}),
          [{"substring",
            "select uid, substring(value from 1) as value from strings"}],
          [{"substring/2",
            "select uid, substring(value from 1 for 1) as value from strings"}],
          [{"substring_for",
            "select uid, substring_for(value, 1) as value from strings"}],
          [{"bucket",
            "select uid, bucket(value by 2) as value from ints"}],
          [{"cast",
            "select uid, cast(value as text) as value from ints"}],
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
      Cloak.SapHanaHelpers.ensure_schema!(connection_params(), @schema)

      [
        table_spec("INTS", [value: "integer"], %{1..10 => [[1]], 1..5 => [[2]], 1..1 => [[3]]}),
        table_spec("TIMES", [value: "datetime"], %{1..10 => [[~c(timestamp'2017-08-23 01:02:03')]]}),
        table_spec("STRINGS", [value: "nvarchar(100)"], %{1..10 => [[~c('a string value')]]}),
        table_spec("VARCHARS", [value: "varchar(100)"], %{1..10 => [[~c('a string value')]]}),
      ]
      |> Enum.map(&Task.async/1)
      |> Stream.map(&Task.await(&1, :timer.seconds(30)))
      |> Enum.reduce(%{}, &Map.merge/2)
    end

    defp table_spec(table_name, table_def, rows_map) do
      fn ->
        conn = connect!(test_schema_connection_params())

        table_def =
          [uid: "integer"]
          |> Keyword.merge(table_def)
          |> Enum.map(fn({column_name, type}) -> {String.upcase(to_string(column_name)), type} end)

        Cloak.SapHanaHelpers.recreate_table!(conn, @schema, table_name,
          table_def
          |> Enum.map(fn({column_name, type}) -> "#{column_name} #{type}" end)
          |> Enum.join(", ")
        )

        Cloak.SapHanaHelpers.insert_rows!(conn, @schema, table_name, Keyword.keys(table_def),
          for {uids, rows} <- rows_map, uid <- uids, row <- rows do
            [uid | row]
          end
        )

        %{String.to_atom(String.downcase(table_name)) =>
          %{
            name: String.downcase(table_name),
            db_name: String.upcase(table_name),
            user_id: "UID",
            ignore_unsupported_types: true
          }
        }
      end
    end

    defp drop_test_schema() do
      {:updated, _} = Cloak.SapHanaHelpers.execute(connect!(connection_params()), "drop schema #{@schema} CASCADE")
      :ok
    end

    defp connection_params(), do:
      Map.new(Application.fetch_env!(:cloak, :sap_hana))

    defp test_schema_connection_params(), do:
      Map.put(connection_params(), :default_schema, @schema)

    defp connect!(connection_params) do
      {:ok, conn} = Cloak.SapHanaHelpers.connect(connection_params)
      conn
    end

    defp data_source(tables_def), do:
      Cloak.DataSource.add_tables(%{
        name: "saphana_test",
        driver: Cloak.DataSource.SAPHana,
        driver_dialect: :saphana,
        parameters: test_schema_connection_params(),
        tables: [],
        initial_tables: tables_def,
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
