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

  @doc false
  def connect(parameters) do
    parameters = Enum.to_list(parameters) ++ [types: true, sync_connect: true, pool: DBConnection.Connection]
    with {:ok, connection} = Postgrex.start_link(parameters) do
      {:ok, %Postgrex.Result{}} = Postgrex.query(connection, "SET standard_conforming_strings = ON", [])
      {:ok, connection}
    end
  end
  @doc false
  def disconnect(connection) do
    GenServer.stop(connection)
  end

  @doc false
  def describe_table(connection, full_table_name) do
    {schema_name, table_name} = case String.split(full_table_name, ".") do
      [full_table_name] -> {"public", full_table_name}
      [schema_name, table_name] -> {schema_name, table_name}
    end
    query = "SELECT column_name, udt_name FROM information_schema.columns " <>
      "WHERE table_name = '#{table_name}' AND table_schema = '#{schema_name}'"
    row_mapper = fn [name, type_name] -> {name, parse_type(type_name)} end
    {:ok, columns_list} = run_query(connection, query, row_mapper, &Enum.to_list/1)
    columns_list
  end

  @doc false
  def select(connection, aql_query, result_processor) do
    statement = SqlBuilder.build(aql_query, :postgresql)
    run_query(connection, statement, &row_mapper/1, result_processor)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(pool, statement, decode_mapper, result_processor) do
    Postgrex.transaction(pool, fn(connection) ->
      with {:ok, query} <- Postgrex.prepare(connection, "data select", statement, []) do
        try do
          Postgrex.stream(connection, query, [], [decode_mapper: decode_mapper, max_rows: 25_000])
          |> Stream.flat_map(fn (%Postgrex.Result{rows: rows}) -> rows end)
          |> result_processor.()
        after
          Postgrex.close(connection, query)
        end
      end
    end, [timeout: :timer.hours(4)])
  end

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("bpchar"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bool"), do: :boolean
  defp parse_type("int2"), do: :integer
  defp parse_type("int4"), do: :integer
  defp parse_type("int8"), do: :integer
  defp parse_type("float4"), do: :real
  defp parse_type("float8"), do: :real
  defp parse_type("money"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("timestamp"), do: :datetime
  defp parse_type("timestamptz"), do: :datetime
  defp parse_type("time"), do: :time
  defp parse_type("timetz"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}


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
  @decimal_precision :math.pow(10, 15)
  defp field_mapper(%Decimal{} = value), do:
    (value |> Decimal.mult(Decimal.new(@decimal_precision)) |> Decimal.to_integer()) / @decimal_precision
  defp field_mapper(field), do: field

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil


  #-----------------------------------------------------------------------------------------------------------
  # Test functions
  #-----------------------------------------------------------------------------------------------------------

  if Mix.env == :test do
    defp parameter_mapper(%NaiveDateTime{} = dt), do:
      %Postgrex.Timestamp{year: dt.year, month: dt.month, day: dt.day,
        hour: dt.hour, min: dt.minute, sec: dt.second, usec: 0}
    defp parameter_mapper(%Date{} = d), do: %Postgrex.Date{year: d.year, month: d.month, day: d.day}
    defp parameter_mapper(%Time{} = t), do: %Postgrex.Time{hour: t.hour, min: t.minute, sec: t.second, usec: 0}
    defp parameter_mapper(value), do: value
    @doc false
    def execute(connection, statement, parameters) do
      parameters = Enum.map(parameters, &parameter_mapper/1)
      Postgrex.query(connection, statement, parameters, [timeout: :timer.minutes(2)])
    end
  end
end
