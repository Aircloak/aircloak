defmodule Cloak.DataSource.PostgreSQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for PostgreSQL.
  For more information, see `DataSource`.
  """

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
    {:ok, {_, _, columns_list}} = run_query(source_id, query, row_mapper)
    columns_list
  end

  @doc false
  def query(source_id, query) do
    run_query(source_id, query_to_string(query))
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(source_id, statement, row_mapper \\ fn x -> x end) do
    options = [timeout: 15 * 60 * 1000, pool_timeout: 2 * 60 * 1000, decode_mapper: row_mapper, pool: @pool_name]
    with {:ok, result} <- Postgrex.query(proc_name(source_id), statement, [], options) do
      %Postgrex.Result{command: :select, num_rows: count, columns: columns, rows: rows} = result
      {:ok, {count, columns, rows}}
    end
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
  defp parse_type("interval"), do: :interval
  defp parse_type(type), do: {:unsupported, type}

  defp query_to_string(query) do
    fields = Enum.map_join(query.select, ",", &("(" <> &1 <> ")::text"))
    "SELECT #{fields} FROM #{query.from}"
  end


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
