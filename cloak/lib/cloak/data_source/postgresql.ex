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
    {_count, columns} = run_query(source_id, query, [], row_mapper)
    columns
  end

  @doc false
  def get_metadata(source_id, table, filter, row_limit) do
    {filter_string, filter_parameters} = filter
    query = build_metadata_query(table, filter_string, row_limit)
    row_mapper = fn [user_id, min_row_id, max_row_id, row_count] -> {user_id, min_row_id, max_row_id, row_count} end
    {_count, rows} = run_query(source_id, query, filter_parameters, row_mapper)
    rows
  end

  @doc false
  def get_data_batch(source_id, table, user_id_value, min_row_id, max_row_id, batch_size, columns, filter) do
    table_name = table[:name]
    user_id = table[:user_id]
    row_id = table[:row_id]
    {filter_string, filter_parameters} = filter
    quoted_columns = for column <- columns, do: ~s("#{column}")
    columns_string = Enum.join([row_id | quoted_columns], ",")
    query = "SELECT #{columns_string} FROM #{table_name} WHERE (#{filter_string}) " <>
      "AND #{user_id} = '#{user_id_value}' AND #{row_id} BETWEEN '#{min_row_id}' AND '#{max_row_id}' " <>
      "ORDER BY #{row_id} ASC LIMIT #{batch_size}"
    run_query(source_id, query, filter_parameters)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(source_id, statement, parameters, row_mapper \\ fn x -> x end) do
    options = [timeout: 15 * 60 * 1000, pool_timeout: 2 * 60 * 1000, decode_mapper: row_mapper, pool: @pool_name]
    result = Postgrex.query!(proc_name(source_id), statement, parameters, options)
    %Postgrex.Result{command: :select, num_rows: count, rows: rows} = result
    {count, rows}
  end

  defp proc_name(source_id), do: {:via, :gproc, {:n, :l, {Cloak.DataSource, source_id}}}

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bool"), do: :boolean
  defp parse_type("int2"), do: :integer
  defp parse_type("int4"), do: :integer
  defp parse_type("int8"), do: :integer
  defp parse_type("float4"), do: :number
  defp parse_type("float8"), do: :number
  defp parse_type("money"), do: :number
  defp parse_type("numeric"), do: :number
  defp parse_type(type), do: {:unsupported, type}

  defp build_metadata_query(table, filter, 0) do
    table_name = table[:name]
    user_id = table[:user_id]
    row_id = table[:row_id]
    "SELECT #{user_id}, MIN(#{row_id}), MAX(#{row_id}), COUNT(#{row_id}) FROM #{table_name} " <>
      "WHERE #{filter} GROUP BY #{user_id}"
  end
  defp build_metadata_query(table, filter, row_limit) do
    table_name = table[:name]
    user_id = table[:user_id]
    row_id = table[:row_id]
    "SELECT #{user_id}, MIN(#{row_id}), MAX(#{row_id}), COUNT(#{row_id}) FROM " <>
      "(SELECT #{user_id}, #{row_id}, ROW_NUMBER() OVER (PARTITION BY #{user_id} ORDER BY #{row_id} DESC) " <>
      "AS index FROM #{table_name} WHERE #{filter}) numbered WHERE index <= #{row_limit} GROUP BY #{user_id}"
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
