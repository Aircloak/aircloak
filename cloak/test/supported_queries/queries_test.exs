defmodule Cloak.Regressions.Queries.Test do
  use ExUnit.Case, async: true

  test "basic sample" do
    query = "SELECT count(*) FROM table"
    assert_compiles_successfully(query, %{table: [{"uid", [type: :integer, uid: true]}]})
  end

  defp assert_compiles_successfully(query, data_source_scaffold) do
    parsed_query = Cloak.Sql.Parser.parse!(query)
    data_source = generate_data_source_config(data_source_scaffold)
    assert {:ok, _, _} = Cloak.Sql.Compiler.compile(data_source, parsed_query, nil, %{})
  end

  defp generate_data_source_config(scaffold) do
    tables = for {name, _} = table <- scaffold, into: %{}, do:
      {name, table_from_scaffold(table)}
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: tables,
    }
  end

  defp table_from_scaffold({table, column_data}) do
    table = to_string(table)
    uid_column_name = uid_column_name(column_data)
    columns = Enum.map(column_data, fn({name, params}) ->
      Cloak.DataSource.Table.column(name, Keyword.get(params, :type))
    end)
    Cloak.DataSource.Table.new(table, uid_column_name, db_name: table, columns: columns)
  end

  defp uid_column_name(columns) do
    Enum.find_value(columns, fn({name, params}) ->
      case Keyword.get(params, :uid) do
        nil -> nil
        true -> name
      end
    end)
  end
end
