defmodule Cloak.DataSource.SAPHanaRODBCTest do
  use ExUnit.Case, async: false

  alias Cloak.Query.Runner
  alias Cloak.SapHanaHelpers

  @moduletag :saphana
  @moduletag :exclude_in_dev

  defmacrop assert_query(data_source, query, expected_response) do
    quote do
      result =
        Runner.run_sync(
          "#{:erlang.unique_integer([:positive])}",
          unquote(data_source),
          unquote(query),
          [],
          %{}
        )

      assert unquote(expected_response) = result
    end
  end

  setup_all do
    cond do
      :os.type() == {:unix, :darwin} ->
        raise "SAP HANA tests can't be executed on macOS. Use `make ci.compliance.debug` instead."

      is_nil(schema()) ->
        raise "Default SAP HANA schema not configured. Please edit your `test.local.exs` file."

      true ->
        tables_def = setup_test_schema()
        on_exit(fn -> drop_test_tables(tables_def) end)
        {:ok, %{data_source: data_source(tables_def)}}
    end
  end

  test(
    "default nvarchar decoding",
    context,
    do:
      assert_query(context.data_source, "select value from strings", %{
        rows: [%{row: ["a string value"]}]
      })
  )

  test(
    "default varchar decoding",
    context,
    do:
      assert_query(context.data_source, "select cast(value as text) from varchars", %{
        rows: [%{row: ["a string value"]}]
      })
  )

  defp schema(), do: Cloak.DataSource.SAPHana.default_schema()

  defp setup_test_schema() do
    SapHanaHelpers.ensure_schema!(connection_params(), schema())

    [
      table_spec("strings", [value: "nvarchar(100)"], %{(1..10) => [["a string value"]]}),
      table_spec("varchars", [value: "varchar(100)"], %{(1..10) => [["a string value"]]})
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
        |> Enum.map(fn {column_name, type} -> {to_string(column_name), type} end)

      db_name = "ExUnit.SAPHanaRODBC.#{table_name}"

      SapHanaHelpers.recreate_table!(
        conn,
        schema(),
        db_name,
        table_def
        |> Enum.map(fn {column_name, type} -> ~s/"#{column_name}" #{type}/ end)
        |> Enum.join(", ")
      )

      SapHanaHelpers.insert_rows!(
        conn,
        schema(),
        db_name,
        Keyword.keys(table_def),
        for {uids, rows} <- rows_map,
            uid <- uids,
            row <- rows do
          [uid | row]
        end
      )

      %{
        String.to_atom(table_name) => %{
          name: table_name,
          db_name: db_name,
          user_id: "uid",
          ignore_unsupported_types: true,
          query: nil
        }
      }
    end
  end

  defp drop_test_tables(tables_def) do
    connection = connect!(connection_params())

    Enum.each(tables_def, fn {_, %{db_name: table_name}} ->
      SapHanaHelpers.execute!(connection, ~s/drop table "#{schema()}"."#{table_name}"/)
    end)
  end

  defp connection_params(), do: Map.new(Application.fetch_env!(:cloak, :sap_hana))

  defp test_schema_connection_params(), do: Map.put(connection_params(), :default_schema, schema())

  defp connect!(connection_params) do
    {:ok, conn} = SapHanaHelpers.connect(connection_params)
    conn
  end

  defp data_source(tables_def),
    do:
      Cloak.DataSource.add_tables(%{
        name: "saphana_test",
        driver: Cloak.DataSource.SAPHanaRODBC,
        concurrency: 0,
        parameters: test_schema_connection_params(),
        tables: [],
        initial_tables: tables_def,
        initial_errors: []
      })
end
