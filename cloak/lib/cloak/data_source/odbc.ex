defmodule Cloak.DataSource.ODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.SqlBuilder

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
    {query_string, params} = SqlBuilder.build(sql_query)
    query_string = query_string |> to_string() |> to_char_list()
    params = Enum.map(params, &convert_param/1)
    execute(source_id, fn (conn) ->
      case :odbc.param_query(conn, query_string, params, 4 * 60 * 60_000) do
        {:selected, _columns, rows} ->
          result =
            rows
            |> Stream.map(fn (row) -> Enum.map(row, &field_mapper/1) end)
            |> result_processor.()
          {:ok, result}
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
  defp parse_type(type), do: {:unsupported, type}

  defp field_mapper({{year, month, day}, {hour, min, sec}}) when is_integer(sec), do:
    NaiveDateTime.new(year, month, day, hour, min, sec) |> error_to_nil()
  defp field_mapper({{year, month, day}, {hour, min, fsec}}) when is_float(fsec) do
    sec = trunc(fsec)
    usec = {trunc((fsec - sec) * 1_000_000), 6}
    NaiveDateTime.new(year, month, day, hour, min, sec, usec) |> error_to_nil()
  end
  defp field_mapper(value), do: value

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  defp convert_param(%NaiveDateTime{} = value), do:
    {:sql_timestamp, [{{value.year, value.month, value.day}, {value.hour, value.minute, value.second}}]}
  defp convert_param(value) when is_binary(value), do: {{:sql_varchar, byte_size(value)}, [value]}
  defp convert_param(value) when is_integer(value), do: {:sql_integer, [value]}
  defp convert_param(value) when is_float(value), do: {:sql_real, [value]}
end
