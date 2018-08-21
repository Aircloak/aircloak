defmodule Air.PsqlServer.SpecialQueries.Common do
  @moduledoc "Handles common special queries issued by various clients, such as ODBC driver and postgrex."

  require Record
  require Logger
  alias Air.PsqlServer
  alias Air.PsqlServer.{Protocol, RanchServer, SpecialQueries}

  @behaviour SpecialQueries

  Record.defrecord(
    :row_description_field,
    Record.extract(:row_description_field, from: "deps/pgsql/src/pgsql_internal.hrl")
  )

  # -------------------------------------------------------------------
  # SpecialQueries callback functions
  # -------------------------------------------------------------------

  @impl SpecialQueries
  def run_query(conn, query) do
    cond do
      cursor_query = cursor_query?(query) ->
        if internal_query?(cursor_query.inner_query) do
          result = Keyword.merge(select_from_shadow_db!(conn, cursor_query.inner_query), command: :fetch)
          first_cursor_fetch(conn, cursor_query.cursor, cursor_query.count, result)
        else
          PsqlServer.run_cancellable_query_on_cloak(
            conn,
            cursor_query.inner_query,
            [],
            &first_cursor_fetch(
              &1,
              cursor_query.cursor,
              cursor_query.count,
              PsqlServer.decode_cloak_query_result(&2)
            )
          )
        end

      cursor_fetch = cursor_count_fetch?(query) ->
        fetch_from_cursor(conn, cursor_fetch.cursor, cursor_fetch.count)

      internal_query?(query) ->
        RanchServer.query_result(conn, select_from_shadow_db!(conn, query))

      query =~ ~r/^begin$/i ->
        RanchServer.query_result(conn, command: :begin)

      query =~ ~r/^set /i ->
        RanchServer.query_result(conn, command: :set)

      cursor = close_cursor_query?(query) ->
        conn
        |> RanchServer.unassign({:cursor_result, cursor})
        |> RanchServer.query_result(command: :"close cursor")

      permission_denied_query?(query) ->
        RanchServer.query_result(conn, {:error, "permission denied"})

      prepared_statement = deallocate_prepared_statement(query) ->
        conn
        |> RanchServer.update_protocol(&Protocol.deallocate_prepared_statement(&1, prepared_statement))
        |> RanchServer.query_result(command: :deallocate)

      true ->
        nil
    end
  end

  @impl SpecialQueries
  def describe_query(conn, query, _params) do
    cond do
      permission_denied_query?(query) ->
        RanchServer.describe_result(conn, columns: [], param_types: [])

      query =~ ~r/show "lc_collate"/i ->
        RanchServer.describe_result(
          conn,
          columns: [%{name: "lc_collate", type: :text}],
          param_types: []
        )

      true ->
        nil
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp cursor_query?(query) do
    case Regex.named_captures(
           ~r/begin;declare\s+"(?<cursor>.+)"\s+cursor.+for\s+(?<inner_query>.*);fetch\s+(?<count>\d+)/is,
           query
         ) do
      %{"cursor" => cursor, "inner_query" => inner_query, "count" => count} ->
        %{cursor: cursor, inner_query: inner_query, count: String.to_integer(count)}

      nil ->
        nil
    end
  end

  defp internal_query?(query) do
    select_from_any_of?(query, ~w(pg_attribute pg_type pg_catalog)) or simple_select?(query) or internal_show?(query)
  end

  defp select_from_any_of?(query, tables),
    do: query =~ ~r/\s*select.*\sfrom\s.*(#{tables |> Stream.map(&Regex.escape/1) |> Enum.join("|")})/si

  defp simple_select?(query), do: query =~ ~r/^\s*select\s+/si and not (query =~ ~r/\sfrom\s/si)

  defp internal_show?(query), do: query =~ ~r/^\s*show/si and not (query =~ ~r/show\s+(tables|columns)/si)

  defp close_cursor_query?(query) do
    case Regex.named_captures(~r/close "(?<cursor>.+)"/is, query) do
      %{"cursor" => cursor} -> cursor
      nil -> nil
    end
  end

  defp permission_denied_query?(query),
    do:
      [
        ~r/SELECT.*INTO TEMPORARY TABLE/is,
        ~r/^DROP TABLE/i,
        ~r/^CREATE\s/i
      ]
      |> Enum.any?(&(query =~ &1))

  defp deallocate_prepared_statement(query) do
    case Regex.named_captures(~r/^deallocate\s+\"(?<prepared_statement>.+)\"$/i, query) do
      %{"prepared_statement" => prepared_statement} -> prepared_statement
      _ -> nil
    end
  end

  defp select_from_shadow_db!(conn, query) do
    {_operation, columns, rows} =
      :pgsql_connection.simple_query(to_charlist(query), [return_descriptions: true], conn.assigns.shadow_db_conn)

    columns =
      Enum.map(
        columns,
        &%{
          name: row_description_field(&1, :name),
          type: &1 |> row_description_field(:data_type_oid) |> Air.PsqlServer.Protocol.Value.type_from_oid()
        }
      )

    rows = Enum.map(rows, &map_row(columns, Tuple.to_list(&1)))

    [columns: columns, rows: rows]
  end

  defp map_row(columns, values) do
    columns
    |> Stream.map(& &1.type)
    |> Stream.zip(values)
    |> Enum.map(&map_value/1)
  end

  for passthrough <- ~w/oid name int2 int4 numeric float4 float8 boolean varchar text bpchar/a do
    defp map_value({unquote(passthrough), value}), do: value
  end

  defp map_value({:unknown, {:unknown, value}}), do: value
  defp map_value({:char, {:char, <<byte>>}}), do: byte
  defp map_value({:regproc, {:regproc, value}}), do: value
  defp map_value({:oidarray, {:array, values}}), do: values |> Stream.map(&{:oid, &1}) |> Enum.map(&map_value/1)
  defp map_value({:date, date}), do: Date.from_erl!(date)

  defp map_value({:timestamptz, datetime}) do
    {:timestamp, datetime}
    |> map_value()
    |> DateTime.from_naive!("Etc/UTC")
  end

  defp map_value({:timestamp, {date, time}}) do
    {time, microseconds} = convert_time(time)
    NaiveDateTime.from_erl!({date, time}, microseconds)
  end

  defp map_value({:time, time}) do
    {time, microseconds} = convert_time(time)
    Time.from_erl!(time, microseconds)
  end

  defp map_value({:timetz, time}) do
    {:timestamptz, {Date.utc_today() |> Date.to_erl(), time}}
    |> map_value()
    |> DateTime.to_time()
  end

  defp convert_time({hours, minutes, seconds}) do
    seconds_int = (1.0 * seconds) |> Float.floor() |> round()
    microseconds = round((seconds - seconds_int) * 1_000_000)
    {{hours, minutes, seconds_int}, microseconds}
  end

  defp first_cursor_fetch(conn, cursor_name, count, query_result),
    do:
      conn
      |> RanchServer.query_result(command: :begin, intermediate: true)
      |> RanchServer.query_result(command: :"declare cursor", intermediate: true)
      |> store_cursor_result(cursor_name, query_result)
      |> fetch_from_cursor(cursor_name, count)

  defp fetch_from_cursor(conn, cursor_name, count) do
    case Map.fetch(conn.assigns, {:cursor_result, cursor_name}) do
      {:ok, {:error, _} = error} ->
        RanchServer.query_result(conn, error)

      {:ok, query_result} ->
        {rows_to_return, remaining_rows} = Enum.split(Keyword.fetch!(query_result, :rows), count)

        conn
        |> RanchServer.query_result(Keyword.merge(query_result, command: :fetch, rows: rows_to_return))
        |> store_cursor_result(cursor_name, Keyword.put(query_result, :rows, remaining_rows))

      :error ->
        RanchServer.query_result(conn, {:error, "cursor `#{cursor_name}` does not exist"})
    end
  end

  defp store_cursor_result(conn, cursor_name, query_result),
    do: RanchServer.assign(conn, {:cursor_result, cursor_name}, query_result)

  defp cursor_count_fetch?(query) do
    case Regex.named_captures(
           ~r/fetch\s+(?<count>\d+)\s+in\s+"(?<cursor>.+)"/is,
           query
         ) do
      %{"cursor" => cursor, "count" => count} ->
        %{cursor: cursor, count: String.to_integer(count)}

      nil ->
        nil
    end
  end
end
