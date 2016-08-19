defmodule Cloak.DataSource.PostgreSQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for PostgreSQL.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.SqlBuilder

  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @pool_name DBConnection.Poolboy

  @doc false
  def child_spec(source_id, parameters) do
    spec = Postgrex.child_spec(parameters ++ [types: true, name: proc_name(source_id), pool: @pool_name])
    {_child_id, start_fun, restart, shutdown, worker, modules} = spec
    {{__MODULE__, source_id}, start_fun, restart, shutdown, worker, modules}
  end

  @doc false
  def get_columns(source_id, full_table_name) do
    {schema_name, table_name} = case String.split(full_table_name, ".") do
      [full_table_name] -> {"public", full_table_name}
      [schema_name, table_name] -> {schema_name, table_name}
    end
    query = "SELECT column_name, udt_name FROM information_schema.columns " <>
      "WHERE table_name = '#{table_name}' AND table_schema = '#{schema_name}'"
    row_mapper = fn [name, type_name] -> {name, parse_type(type_name)} end
    {:ok, columns_list} = run_query(source_id, query, row_mapper, &Enum.to_list/1)
    columns_list
  end

  @doc false
  def select(source_id, sql_query, result_processor) do
    {query_string, params} = SqlBuilder.build(sql_query)
    params = Enum.map(params, &convert_param/1)
    run_query(source_id, query_string, params, &row_mapper/1, result_processor)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(source_id, statement, params \\ [], decode_mapper, result_processor) do
    options = [timeout: 4 * 60 * 60_000, pool_timeout: 5 * 60_000, pool: @pool_name]
    Postgrex.transaction(proc_name(source_id), fn(conn) ->
      with {:ok, query} <- Postgrex.prepare(conn, "data select", statement, []) do
        try do
          Postgrex.stream(conn, query, params, [decode_mapper: decode_mapper, max_rows: 25_000])
          |> Stream.flat_map(fn (%Postgrex.Result{rows: rows}) -> rows end)
          |> result_processor.()
        after
          Postgrex.close(conn, query)
        end
      end
    end, options)
  end

  defp proc_name(source_id), do: {:via, :gproc, {:n, :l, {Cloak.DataSource, source_id}}}

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bool"), do: :boolean
  defp parse_type("int2"), do: :integer
  defp parse_type("int4"), do: :integer
  defp parse_type("int8"), do: :integer
  defp parse_type("float4"), do: :real
  defp parse_type("float8"), do: :real
  defp parse_type("money"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("timestamp"), do: :timestamp
  defp parse_type("timestamptz"), do: :timestamp
  defp parse_type("time"), do: :time
  defp parse_type("timetz"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}

  defp convert_param(%NaiveDateTime{} = time), do:
    %Postgrex.Timestamp{
      year: time.year, month: time.month, day: time.day, hour: time.hour, min: time.minute, sec: time.second
    }
  defp convert_param(%Time{hour: hour, minute: min, second: sec, microsecond: {usec, _precision}}), do:
    %Postgrex.Time{hour: hour, min: min, sec: sec, usec: usec}
  defp convert_param(%Date{year: year, month: month, day: day}), do:
    %Postgrex.Date{year: year, month: month, day: day}
  defp convert_param(param), do: param


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp row_mapper(row), do: for field <- row, do: field_mapper(field)

  defp field_mapper(%Postgrex.Timestamp{year: year, month: month, day: day, hour: hour, min: min, sec: sec, usec: usec}), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, usec) |> error_to_nil()
  defp field_mapper(%Postgrex.Date{year: year, month: month, day: day}), do:
    Date.new(year, month, day) |> error_to_nil()
  defp field_mapper(%Postgrex.Time{hour: hour, min: min, sec: sec, usec: usec}), do:
    Time.new(hour, min, sec, usec) |> error_to_nil()
  defp field_mapper(field), do: field

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil


  #-----------------------------------------------------------------------------------------------------------
  # Test functions
  #-----------------------------------------------------------------------------------------------------------

  if Mix.env == :test do
    @doc false
    def execute(statement, parameters \\ []) do
      options = [timeout: 2 * 60 * 1000, pool_timeout: 10 * 1000, pool: @pool_name]
      Postgrex.query(proc_name(:local), statement, parameters, options)
    end
  end
end
