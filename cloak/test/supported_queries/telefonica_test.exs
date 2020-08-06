defmodule Cloak.Regressions.TeamBank.Test do
  use ExUnit.Case, async: true

  test "party id – dual: select *", do:
    assert_compiles_successfully("SELECT * FROM dual", data_source_party_id())

  test "party id - dual: type casts" do
    query = """
    select cast(true as boolean) as col_boolean
         , current_date() as col_date
         , current_timestamp() as col_timestamp
     --    , current_time() as col_time
         , cast(123456789 as integer) as col_integer
         , cast(3.141592653589 as real) as col_real
         , 'text column äöüß' as col_text
      from dual
    """

    assert_compiles_successfully(query, data_source_party_id())
  end

  defp data_source_party_id(), do: %{dual: []}

  defp assert_compiles_successfully(query, data_source_scaffold) do
    parsed_query = Cloak.Sql.Parser.parse!(query)
    data_source = generate_data_source_config(data_source_scaffold)
    assert {:ok, _} = Cloak.Sql.Compiler.compile(parsed_query, nil, data_source, nil, %{})
  end

  defp generate_data_source_config(scaffold) do
    tables = for {name, _} = table <- scaffold, into: %{}, do: {name, table_from_scaffold(table)}

    %{
      name: "telefonic_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: tables
    }
  end

  defp table_from_scaffold({table, column_data}) do
    table = to_string(table)
    uid_column_name = uid_column_name(column_data)

    columns =
      Enum.map(column_data, fn {name, params} ->
        Cloak.DataSource.Table.column(name, Keyword.get(params, :type))
      end)

    Cloak.DataSource.Table.new(table, uid_column_name, db_name: table, columns: columns)
  end

  defp uid_column_name(columns) do
    Enum.find_value(columns, fn {name, params} ->
      case Keyword.get(params, :uid) do
        nil -> nil
        true -> name
      end
    end)
  end
end
