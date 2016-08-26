defmodule Cloak.DataSource.ODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.SqlBuilder
  alias Cloak.Aql.Column

  defmodule Worker do
    @moduledoc false

    use GenServer

    def init(parameters) do
      options = [auto_commit: :on, binary_strings: :on, tuple_row: :off]
      parameters[:connect] |> to_char_list() |> :odbc.connect(options)
    end
    def start_link(parameters), do: :gen_server.start_link(__MODULE__, parameters, [])
    def handle_call({:execute, fun}, _from, conn), do: {:reply, fun.(conn), conn}
  end


  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def child_spec(source_id, parameters) do
    pool_options = [
      name: proc_name(source_id),
      worker_module: Cloak.DataSource.ODBC.Worker,
      size: parameters[:pool_size],
      max_overflow: 0
    ]
    :poolboy.child_spec({__MODULE__, source_id}, pool_options, parameters)
  end

  @doc false
  def get_columns(source_id, full_table_name) do
    {:ok, columns} = execute(source_id, fn (conn) ->
      :odbc.describe_table(conn, to_char_list(full_table_name), 15_000)
    end)
    for {name, type} <- columns, do: {to_string(name), parse_type(type)}
  end

  @doc false
  def select(source_id, sql_query, result_processor) do
    statement = sql_query |> SqlBuilder.build() |> to_char_list()
    field_mappers = for column <- sql_query.db_columns, do: column_to_field_mapper(column)
    execute(source_id, fn (conn) ->
      case :odbc.select_count(conn, statement, 4 * 60 * 60_000) do
        {:ok, _count} ->
          data_stream = Stream.resource(fn () -> conn end, fn (conn) ->
            case :odbc.select(conn, :next, 25_000, 5 * 60_000) do
              {:selected, _columns, []} -> {:halt, conn}
              {:selected, _columns, rows} -> {Enum.map(rows, &map_fields(&1, field_mappers)), conn}
              {:error, reason} -> raise to_string(reason)
            end
          end, fn (_conn) -> :ok end)
          {:ok, result_processor.(data_stream)}
        {:error, reason} ->
          {:error, to_string(reason)}
      end
    end)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp proc_name(source_id), do: {:via, :gproc, {:n, :l, {Cloak.DataSource, source_id}}}

  defp execute(source_id, fun) do
    # This copies data between processes, so it might be faster to just
    # open a new connection each time and execute the query inline.
    :poolboy.transaction(proc_name(source_id), fn(worker) ->
      :gen_server.call(worker, {:execute, fun}, :infinity)
    end)
  end

  defp parse_type(:sql_integer), do: :integer
  defp parse_type(:sql_smallint), do: :integer
  defp parse_type(:sql_bit), do: :boolean
  defp parse_type(:sql_real), do: :real
  defp parse_type(:sql_float), do: :real
  defp parse_type(:sql_double), do: :real
  defp parse_type(:SQL_LONGVARCHAR), do: :text
  defp parse_type({:sql_varchar, _length}), do: :text
  defp parse_type(:sql_timestamp), do: :timestamp
  defp parse_type(:SQL_TYPE_DATE), do: :date
  defp parse_type(:SQL_TYPE_TIME), do: :time
  defp parse_type(type), do: {:unsupported, type}

  defp map_fields([], []), do: []
  defp map_fields([field | rest_fields], [mapper | rest_mappers]), do:
    [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp column_to_field_mapper(%Column{type: :timestamp}), do: &timestamp_field_mapper/1
  defp column_to_field_mapper(%Column{type: :time}), do: &time_field_mapper/1
  defp column_to_field_mapper(%Column{type: :date}), do: &date_field_mapper/1
  defp column_to_field_mapper(%Column{}), do: &generic_field_mapper/1

  defp generic_field_mapper(:null), do: nil
  defp generic_field_mapper(value), do: value

  defp timestamp_field_mapper(:null), do: nil
  defp timestamp_field_mapper({{year, month, day}, {hour, min, sec}}) when is_integer(sec), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, {0, 6}) |> error_to_nil()
  defp timestamp_field_mapper({{year, month, day}, {hour, min, fsec}}) when is_float(fsec) do
    sec = trunc(fsec)
    usec = {trunc((fsec - sec) * 1_000_000), 6}
    NaiveDateTime.new(year, month, day, hour, min, sec, usec) |> error_to_nil()
  end

  defp date_field_mapper(:null), do: nil
  defp date_field_mapper(string) when is_binary(string), do: Cloak.Time.parse_date(string) |> error_to_nil()

  defp time_field_mapper(:null), do: nil
  defp time_field_mapper(string) when is_binary(string), do: Cloak.Time.parse_time(string) |> error_to_nil()

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil
end
