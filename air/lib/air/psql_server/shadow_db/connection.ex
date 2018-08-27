defmodule Air.PsqlServer.ShadowDb.Connection do
  @moduledoc "Module for working with a shadow database connection."

  require Logger
  require Record

  Record.defrecordp(:epgsql_statement, Record.extract(:statement, from_lib: "epgsql/include/epgsql.hrl"))
  Record.defrecordp(:epgsql_column, Record.extract(:column, from_lib: "epgsql/include/epgsql.hrl"))
  Record.defrecordp(:epgsql_error, Record.extract(:error, from_lib: "epgsql/include/epgsql.hrl"))

  @type query_result :: {:ok, [column], [term]} | {:error, String.t()}
  @type parse_result :: {:ok, [column], [atom]} | {:error, String.t()}
  @type column :: %{name: String.t(), type: atom}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the given SQL query."
  @spec query(pid, String.t(), [term]) :: query_result
  def query(conn, query, params) do
    case :epgsql.equery(conn, to_charlist(query), params) do
      {:ok, columns, rows} ->
        columns = Enum.map(columns, &map_column/1)
        rows = Enum.map(rows, &map_row(columns, Tuple.to_list(&1)))
        {:ok, columns, rows}

      {:error, epgsql_error} ->
        Logger.error("error executing shadow db query: #{query}")
        {:error, epgsql_error(epgsql_error, :message)}
    end
  end

  @doc "Parses the given SQL query."
  @spec parse(pid, String.t()) :: parse_result
  def parse(conn, query) do
    case :epgsql.parse(conn, to_charlist(query)) do
      {:ok, statement} ->
        columns = Enum.map(epgsql_statement(statement, :columns), &map_column/1)

        param_types =
          epgsql_statement(statement, :parameter_info)
          |> Stream.map(fn
            {oid, _type_name, _array_oid} -> oid
            {_type_name, oid} -> oid
          end)
          |> Enum.map(&Air.PsqlServer.Protocol.Value.type_from_oid/1)

        {:ok, columns, param_types}

      {:error, epgsql_error} ->
        Logger.error("error parsing shadow db query: #{query}")
        {:error, epgsql_error(epgsql_error, :message)}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp connect(data_source_name, attempts \\ 10) do
    attempt_connect(data_source_name)
  catch
    _, _ ->
      if attempts == 1 do
        raise "can't connect to the shadow database for `#{data_source_name}`"
      else
        Process.sleep(:timer.seconds(1))
        connect(data_source_name, attempts - 1)
      end
  end

  defp attempt_connect(data_source_name) do
    :epgsql.connect(
      '127.0.0.1',
      'postgres',
      '',
      %{database: to_charlist(Air.PsqlServer.ShadowDb.db_name(data_source_name))}
    )
  end

  defp map_column(epgsql_column) do
    %{
      name: epgsql_column(epgsql_column, :name),
      type: epgsql_column |> epgsql_column(:oid) |> Air.PsqlServer.Protocol.Value.type_from_oid()
    }
  end

  defp map_row(columns, values) do
    columns
    |> Stream.map(& &1.type)
    |> Stream.zip(values)
    |> Stream.map(&map_value/1)
    |> Enum.map(fn value -> if value == :null, do: nil, else: value end)
  end

  for passthrough <- ~w/
    oid name int2 int4 int8 numeric float4 float8 boolean varchar text bpchar char regproc unknown
    /a do
    defp map_value({unquote(passthrough), value}), do: value
  end

  defp map_value({:date, date}), do: Date.from_erl!(date)

  defp map_value({:oidarray, encoded}) do
    encoded
    |> String.replace(~r/\{|\}/, "")
    |> String.split(",")
    |> Stream.reject(&(&1 == ""))
    |> Enum.map(&String.to_integer/1)
  end

  defp map_value({:timestamptz, datetime}) do
    {:timestamp, datetime}
    |> map_value()
    |> DateTime.from_naive!("Etc/UTC")
  end

  defp map_value({:timestamp, {date, time}}) do
    {time, microseconds} = convert_time(time)
    NaiveDateTime.from_erl!({date, time}, {microseconds, _precision = 6})
  end

  defp map_value({:time, time}) do
    {time, microseconds} = convert_time(time)
    Time.from_erl!(time, {microseconds, _precision = 6})
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

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(data_source_name) do
    # We're using epgsql instead of Postgrex, since epgsql supports lower level functions, such as query parsing, and
    # because it returns proper type information, even for queries which return no rows. Finally, Postgrex suffers from
    # a potential information loss for OID types (see https://hexdocs.pm/postgrex/readme.html#oid-type-encoding).
    %{
      id: __MODULE__,
      start: fn -> connect(data_source_name) end
    }
  end
end
