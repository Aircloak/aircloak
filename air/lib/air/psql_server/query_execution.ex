defmodule Air.PsqlServer.QueryExecution do
  @moduledoc "Handles query execution."

  require Record
  require Logger
  alias Air.PsqlServer.{CloakQuery, Protocol, RanchServer}

  Record.defrecord(:epgsql_statement, Record.extract(:statement, from_lib: "epgsql/include/epgsql.hrl"))
  Record.defrecord(:epgsql_column, Record.extract(:column, from_lib: "epgsql/include/epgsql.hrl"))
  Record.defrecord(:epgsql_error, Record.extract(:error, from_lib: "epgsql/include/epgsql.hrl"))

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the given query."
  @spec run_query(RanchServer.t(), String.t(), [Protocol.param_with_type()]) :: RanchServer.t()
  def run_query(conn, query, params) do
    execute(fn ->
      cond do
        permission_denied_query?(conn, query) ->
          RanchServer.query_result(conn, {:error, "permission denied"})

        cursor = cursor_query?(query) ->
          if internal_query?(cursor.inner_query) do
            select_from_shadow_db(conn, cursor.inner_query, params, &first_cursor_fetch(&1, cursor, &2))
            conn
          else
            CloakQuery.run_query(conn, cursor.inner_query, [], &first_cursor_fetch(&1, cursor, &2))
            conn
          end

        cursor_fetch = cursor_count_fetch?(query) ->
          fetch_from_cursor(conn, cursor_fetch.cursor, cursor_fetch.count)

        internal_query?(query) ->
          select_from_shadow_db(conn, query, params, &RanchServer.query_result/2)
          conn

        query =~ ~r/^begin$/i ->
          RanchServer.query_result(conn, command: :begin)

        query =~ ~r/^set /i ->
          RanchServer.query_result(conn, command: :set)

        cursor = close_cursor_query?(query) ->
          conn
          |> RanchServer.unassign({:cursor_result, cursor})
          |> RanchServer.query_result(command: :"close cursor")

        prepared_statement = deallocate_prepared_statement(query) ->
          conn
          |> RanchServer.update_protocol(&Protocol.deallocate_prepared_statement(&1, prepared_statement))
          |> RanchServer.query_result(command: :deallocate)

        true ->
          CloakQuery.run_query(conn, query, params, &RanchServer.query_result/2)
          conn
      end
    end)
  end

  @doc "Describes the given query."
  @spec describe_query(RanchServer.t(), String.t(), [Protocol.db_value()]) :: RanchServer.t()
  def describe_query(conn, query, params) do
    execute(fn ->
      cond do
        permission_denied_query?(conn, query) ->
          RanchServer.query_result(conn, {:error, "permission denied"})

        internal_query?(query) ->
          describe_from_shadow_db(conn.assigns.data_source_name, query)
          conn

        true ->
          CloakQuery.describe_query(conn, query, params)
          conn
      end
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute(fun) do
    if Application.get_env(:air, :integration_tests, false) do
      try do
        fun.()
      catch
        t, e ->
          IO.puts(Exception.format(t, e, :erlang.get_stacktrace()))
          raise "error executing query"
      end
    else
      fun.()
    end
  end

  defp cursor_query?(query) do
    case Regex.named_captures(
           ~r/begin;declare\s+"(?<cursor>.+)"\s+cursor.+for\s+(?<inner_query>.*);fetch\s+(?<count>\d+)/is,
           query
         ) do
      %{"cursor" => cursor, "inner_query" => inner_query, "count" => count} ->
        %{name: cursor, inner_query: inner_query, count: String.to_integer(count)}

      nil ->
        nil
    end
  end

  defp internal_query?(query) do
    query = strip_comments(query)
    select_from_any_of?(query, ~w(pg_attribute pg_type pg_catalog)) or simple_select?(query) or internal_show?(query)
  end

  defp strip_comments(query), do: String.replace(query, ~r/^\s*--.*$/m, "")

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

  defp permission_denied_query?(conn, query),
    do: forbidden_query?(query) or not Air.Service.User.is_enabled?(conn.assigns.user) or not permitted?(conn)

  defp forbidden_query?(query) do
    [
      ~r/SELECT.*INTO TEMPORARY TABLE/is,
      ~r/INSERT INTO/is,
      ~r/^DROP TABLE/i,
      ~r/^CREATE\s/i
    ]
    |> Enum.any?(&(query =~ &1))
  end

  defp permitted?(conn),
    do: match?({:ok, _}, Air.Service.DataSource.fetch_as_user(conn.assigns.data_source_id, conn.assigns.user))

  defp deallocate_prepared_statement(query) do
    case Regex.named_captures(~r/^deallocate\s+\"(?<prepared_statement>.+)\"$/i, query) do
      %{"prepared_statement" => prepared_statement} -> prepared_statement
      _ -> nil
    end
  end

  defp select_from_shadow_db(conn, query, params, on_success) do
    RanchServer.run_async(
      fn ->
        case Air.PsqlServer.ShadowDb.query(
               conn.assigns.data_source_name,
               query,
               Enum.map(params || [], fn {_type, value} -> value end)
             ) do
          {:error, _} = error -> error
          {:ok, columns, rows} -> [columns: columns, rows: rows]
        end
      end,
      on_success: on_success,
      on_failure: fn conn, _exit_reason -> RanchServer.query_result(conn, {:error, "query failed"}) end
    )
  end

  defp describe_from_shadow_db(data_source_name, query) do
    RanchServer.run_async(
      fn -> Air.PsqlServer.ShadowDb.parse(data_source_name, query) end,
      on_success: fn
        conn, {:ok, columns, param_types} ->
          RanchServer.describe_result(conn, columns: columns, param_types: param_types)

        conn, {:error, _reason} = error ->
          RanchServer.describe_result(conn, error)
      end,
      on_failure: fn conn, _exit_reason -> RanchServer.describe_result(conn, {:error, "parsing failed"}) end
    )
  end

  defp first_cursor_fetch(conn, cursor, query_result) do
    conn
    |> RanchServer.query_result(command: :begin, intermediate: true)
    |> RanchServer.query_result(command: :"declare cursor", intermediate: true)
    |> store_cursor_result(cursor.name, query_result)
    |> fetch_from_cursor(cursor.name, cursor.count)
  end

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
