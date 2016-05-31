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
  def select(source_id, sql_query) do
    run_query(source_id, select_query_to_string(sql_query))
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

  defp select_query_to_string(%{columns: fields_list, from: table} = query) do
    fields_str = Enum.map_join(fields_list, ",", &select_column_to_string/1)
    "SELECT #{fields_str} FROM #{table} #{where_sql(Map.get(query, :where))}"
  end

  defp select_column_to_string({:count, :star}), do: "'*' as \"count(*)\""
  defp select_column_to_string(column), do: "(" <> column <> ")::text"

  defp where_sql(nil), do: ""
  defp where_sql(clauses) do
    "WHERE #{construct_where_clause(clauses, [])}"
  end

  defp construct_where_clause(_clauses = [], acc), do: acc |> Enum.reverse |> Enum.join(" AND ")
  defp construct_where_clause([{:comparison, what, comparator, value} | clauses], acc) do
    clause = "#{what} #{comparator} #{value}"
    construct_where_clause(clauses, [clause|acc])
  end
  defp construct_where_clause([{:in, what, values} | clauses], acc) do
    clause = "#{what} IN (#{values |> Enum.join(", ")})"
    construct_where_clause(clauses, [clause|acc])
  end
  defp construct_where_clause([{:like, what, match} | clauses], acc) do
    clause = "#{what} LIKE #{match}"
    construct_where_clause(clauses, [clause|acc])
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
