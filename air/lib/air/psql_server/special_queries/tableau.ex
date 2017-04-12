defmodule Air.PsqlServer.SpecialQueries.Tableau do
  @moduledoc "Handles common special queries issued by Tableau."
  @behaviour Air.PsqlServer.SpecialQueries

  alias Air.PsqlServer
  alias Air.PsqlServer.{Protocol, RanchServer}


  #-----------------------------------------------------------------------------------------------------------
  # SpecialQueries callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def handle_query(conn, query) do
    cond do
      query =~ ~r/begin;declare.* for select relname, nspname, relkind from.*fetch.*/i ->
        fetch_tables(conn)
      table_name = table_name_from_table_info_query(query) ->
        fetch_table_info(conn, table_name)
      true ->
        nil
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp fetch_tables(conn) do
    PsqlServer.start_async_query(conn, "show tables", [],
      fn(conn, {:ok, show_tables_response}) ->
        table_names =
          show_tables_response
          |> Map.fetch!("rows")
          |> Enum.map(fn(%{"row" => [table_name]}) -> table_name end)

        conn
        |> RanchServer.set_query_result(command: :begin, intermediate: true)
        |> RanchServer.set_query_result(command: :"declare cursor", intermediate: true)
        |> RanchServer.set_query_result(table_list(table_names))
      end
    )
  end

  defp table_list(table_names), do:
    [
      command: :fetch,
      columns:
        [
          %{name: "relname", type: :name},
          %{name: "nspname", type: :name},
          %{name: "relkind", type: :char},
        ],
      rows: Enum.map(table_names, &[&1, "public", ?r])
    ]

  defp table_name_from_table_info_query(query) do
    case Regex.named_captures(~r/^select n.nspname.*relname like '(?<table_name>.*)' and/, query) do
      %{"table_name" => table_name} -> table_name
      _ -> nil
    end
  end

  defp fetch_table_info(conn, table_name) do
    PsqlServer.start_async_query(conn, "show columns from #{table_name}", [],
      fn(conn, {:ok, show_columns_response}) ->
        RanchServer.set_query_result(conn,
          show_columns_response
          |> Map.fetch!("rows")
          |> Enum.map(&Map.fetch!(&1, "row"))
          |> column_list(conn.assigns.data_source_id, table_name)
        )
      end
    )
  end

  defp column_list(table_columns, data_source_id, table_name) do
    result_columns = [
      nspname: :name, relname: :name, attname: :name, atttypid: :text, typname: :name, attnum: :int2,
      attlen: :int2, atttypmod: :int4, attnotnull: :boolean, relhasrules: :boolean, relkind: :char,
      oid: :int4, pg_get_expr: :text, case: :text, typtypmod: :int4, relhasoids: :boolean,
    ]

    [
      columns: Enum.map(result_columns, fn({name, type}) -> %{name: to_string(name), type: type} end),
      rows:
        table_columns
        |> Enum.with_index()
        |> Enum.map(&column_row_for_tableau(&1, data_source_id, table_name, result_columns))
    ]
  end

  defp column_row_for_tableau({[column_name, column_type], index}, data_source_id, table_name, result_columns) do
    psql_type = PsqlServer.psql_type(column_type)
    type_info = Protocol.Value.type_info(psql_type)
    row_fields = %{
      nspname: "public", relname: table_name, attname: column_name, atttypid: type_info.oid,
      typname: psql_type, attnum: index + 1, attlen: type_info.len, atttypmod: -1, attnotnull: false,
      relhasrules: false, relkind: ?r, oid: :erlang.phash2({data_source_id, table_name}), pg_get_expr: "",
      case: "0", typtypmod: -1, relhasoids: false,
    }
    Enum.map(result_columns, fn({column_name, _type}) -> Map.fetch!(row_fields, column_name) end)
  end
end
