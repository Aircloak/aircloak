defmodule Air.PsqlServer.SpecialQueries.Tableau do
  @moduledoc "Handles common special queries issued by Tableau."

  alias Air.PsqlServer
  alias Air.PsqlServer.{RanchServer, SpecialQueries}
  alias Air.Service.DataSource

  @behaviour SpecialQueries

  # -------------------------------------------------------------------
  # SpecialQueries callback functions
  # -------------------------------------------------------------------

  @impl SpecialQueries
  def run_query(conn, query) do
    cond do
      cursor_query = cursor_query?(query) ->
        cond do
          query =~ ~r/select relname, nspname, relkind from.*/i ->
            fetch_tables(conn, cursor_query.cursor, cursor_query.count)

          cursor_query.inner_query =~ ~r/select\spt.tgargs.*FROM.*pg_catalog.pg_trigger.*/i ->
            # fetching triggers
            first_cursor_fetch(
              conn,
              cursor_query.cursor,
              cursor_query.count,
              empty_result(
                :fetch,
                ~w(tgargs tgnargs tgdeferrable tginitdeferred pp1.proname pp2.proname pc.oid
                pc1.oid relname tgconstrname nspname)
              )
            )

          cursor_query.inner_query =~ ~r/select c.relname, i.indkey/i ->
            # related fields
            first_cursor_fetch(
              conn,
              cursor_query.cursor,
              cursor_query.count,
              empty_result(
                :fetch,
                ~w(relname indkey indisunique indisclustered amname relhasrules nspname oid
                relhasoids ?column?)
              )
            )

          cursor_query.inner_query =~ ~r/select ta.attname, ia.attnum.*ia.attrelid = i.indexrelid.*/i ->
            # indexed columns
            first_cursor_fetch(
              conn,
              cursor_query.cursor,
              cursor_query.count,
              empty_result(:fetch, ~w(attname attnum relname nspname relname))
            )

          true ->
            # query will be delegated to cloak
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

      cursor_fetch = cursor_fetch?(query) ->
        fetch_from_cursor(conn, cursor_fetch.cursor, cursor_fetch.count)

      true ->
        nil
    end
  end

  @impl SpecialQueries
  def describe_query(_conn, _query, _params), do: nil

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp fetch_tables(conn, cursor, count) do
    case get_tables(conn) do
      {:ok, tables} ->
        first_cursor_fetch(conn, cursor, count, tables |> Enum.map(& &1.id) |> table_list())

      {:error, :unauthorized} ->
        RanchServer.query_result(conn, {:error, "not authorized"})
    end
  end

  defp table_list(table_names),
    do: [
      command: :fetch,
      columns: [
        %{name: "relname", type: :name},
        %{name: "nspname", type: :name},
        %{name: "relkind", type: :char}
      ],
      rows: Enum.map(table_names, &[&1, "", ?r])
    ]

  defp get_tables(conn) do
    user = conn.assigns.user

    case DataSource.fetch_as_user(conn.assigns.data_source_id, user) do
      {:ok, data_source} -> {:ok, DataSource.views_and_tables(user, data_source)}
      error -> error
    end
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

  defp empty_result(command, column_names),
    do: [
      command: command,
      columns: Enum.map(column_names, &%{name: &1, type: :unknown}),
      rows: []
    ]

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

  defp cursor_fetch?(query) do
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
