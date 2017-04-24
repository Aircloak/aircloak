defmodule Air.PsqlServer.SpecialQueries.Common do
  @moduledoc "Handles common special queries issued by various clients, such as ODBC driver and postgrex."
  @behaviour Air.PsqlServer.SpecialQueries

  alias Air.PsqlServer.{Protocol, RanchServer}


  #-----------------------------------------------------------------------------------------------------------
  # SpecialQueries callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def run_query(conn, query) do
    cond do
      query =~ ~r/^set /i ->
        RanchServer.query_result(conn, command: :set)
      query =~ ~r/^close /i ->
        RanchServer.query_result(conn, command: :"close cursor")
      query =~ ~r/^select t.oid, t.typname, t.typsend, t.typreceive.*FROM pg_type AS t\s*$/is ->
        return_types_for_postgrex(conn)
      query =~ ~r/^select.+from pg_type/si ->
        RanchServer.query_result(conn, [columns: [%{name: "oid", type: :text}], rows: []])
      query =~ ~r/select current_schema()/i ->
        RanchServer.query_result(conn, [columns: [%{name: "current_schema", type: :text}], rows: [[""]]])
      query =~ ~r/show "lc_collate"/i ->
        # returning C means no "no locale" (https://www.postgresql.org/docs/current/static/locale.html)
        RanchServer.query_result(conn, [columns: [%{name: "lc_collate", type: :text}], rows: [["C"]]])
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

  @doc false
  def describe_query(conn, query, _params) do
    cond do
      permission_denied_query?(query) ->
        RanchServer.describe_result(conn, columns: [], param_types: [])
      query =~ ~r/show "lc_collate"/i ->
        RanchServer.describe_result(conn, [columns: [%{name: "lc_collate", type: :text}], param_types: []])
      true ->
        nil
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp permission_denied_query?(query), do:
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

  defp return_types_for_postgrex(conn), do:
    RanchServer.query_result(conn, [
      columns:
        ~w(oid typname typsend typreceive typoutput typinput typelem coalesce array)
        |> Enum.map(&%{name: &1, type: :text}),
      rows:
        [
          ~w(16 bool boolsend boolrecv boolout boolin 0 0 {}),
          ~w(21 int2 int2send int2recv int2out int2in 0 0 {}),
          ~w(23 int4 int4send int4recv int4out int4in 0 0 {}),
          ~w(20 int8 int8send int8recv int8out int8in 0 0 {}),
          ~w(25 text textsend textrecv textout textin 0 0 {}),
          ~w(700 float4 float4send float4recv float4out float4in 0 0 {}),
          ~w(701 float8 float8send float8recv float8out float8in 0 0 {}),
          ~w(705 unknown unknownsend unknownrecv unknownout unknownin 0 0 {}),
          ~w(1082 date date_send date_recv date_out date_in 0 0 {}),
          ~w(1083 time time_send time_recv time_out time_in 0 0 {}),
          ~w(1114 timestamp timestamp_send timestamp_recv timestamp_out timestamp_in 0 0 {}),
          ~w(1700 numeric numeric_send numeric_recv numeric_out numeric_in 0 0 {})
        ]
    ])
end
