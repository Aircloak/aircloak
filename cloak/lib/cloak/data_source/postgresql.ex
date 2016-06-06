defmodule Cloak.DataSource.PostgreSQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for PostgreSQL.
  For more information, see `DataSource`.
  """

  alias Cloak.SqlQuery.Parsers.Token


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
    {query_string, params} = select_query_spec(sql_query)
    run_query(source_id, query_string, params, &row_mapper/1)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(source_id, statement, params \\ [], decode_mapper) do
    options = [timeout: 15 * 60 * 1000, pool_timeout: 2 * 60 * 1000, decode_mapper: decode_mapper, pool: @pool_name]
    with {:ok, result} <- Postgrex.query(proc_name(source_id), statement, params, options) do
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
  defp parse_type(type), do: {:unsupported, type}


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp row_mapper(row), do: for field <- row, do: field_mapper(field)

  defp field_mapper(%Postgrex.Timestamp{year: year, month: month, day: day,
      hour: hour, min: min, sec: sec, usec: 0}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [year, month, day, hour, min, sec])
    |> to_string()
  end
  defp field_mapper(%Postgrex.Timestamp{year: year, month: month, day: day,
      hour: hour, min: min, sec: sec, usec: usec}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~6..0B", [year, month, day, hour, min, sec, usec])
    |> to_string()
  end
  defp field_mapper(%Postgrex.Date{year: year, month: month, day: day}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B", [year, month, day])
    |> to_string()
  end
  defp field_mapper(%Postgrex.Timestamp{year: year, month: month, day: day,
      hour: hour, min: min, sec: sec, usec: 0}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [year, month, day, hour, min, sec])
    |> to_string()
  end
  defp field_mapper(%Postgrex.Time{hour: hour, min: min, sec: sec, usec: 0}) do
    :io_lib.format("~2..0B:~2..0B:~2..0B", [hour, min, sec])
    |> to_string()
  end
  defp field_mapper(%Postgrex.Time{hour: hour, min: min, sec: sec, usec: usec}) do
    :io_lib.format("~2..0B:~2..0B:~2..0B.~6..0B", [hour, min, sec, usec])
    |> to_string()
  end
  defp field_mapper(field), do: field


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  @typep query_spec :: {statement, [constant]}
  @typep constant :: String.t | number | boolean
  @typep statement :: iodata
  @typep fragment :: String.t | {:param, constant} | [fragment]

  @spec select_query_spec(Cloak.SqlQuery.t) :: query_spec
  defp select_query_spec(%{from: table} = query) do
    fragments_to_query_spec([
      "SELECT ", Enum.map_join(ordered_selected_columns(query), ",", &select_column_to_string/1), " ",
      "FROM ", table, " ",
      where_fragments(query[:where])
    ])
  end

  @spec fragments_to_query_spec([fragment]) :: query_spec
  defp fragments_to_query_spec(fragments) do
    {query_string(fragments), params(fragments)}
  end

  defp query_string(fragments) do
    fragments
    |> List.flatten()
    |> Enum.reduce(%{query_string: [], param_index: 1}, &parse_fragment(&2, &1))
    |> Map.fetch!(:query_string)
  end

  defp parse_fragment(query_builder, string) when is_binary(string) do
    %{query_builder | query_string: [query_builder.query_string, string]}
  end
  defp parse_fragment(query_builder, {:param, _value}) do
    %{query_builder |
      query_string: [query_builder.query_string, "$#{query_builder.param_index}"],
      param_index: query_builder.param_index + 1
    }
  end

  defp params(fragments) do
    fragments
    |> List.flatten()
    |> Stream.filter(&match?({:param, _}, &1))
    |> Enum.map(fn({:param, value}) -> value end)
  end

  defp ordered_selected_columns(%{columns: columns} = query) do
    unselected_group_by_columns = Map.get(query, :group_by, []) -- columns
    columns ++ unselected_group_by_columns
  end

  defp select_column_to_string({:count, :star}), do: "'*' as \"count(*)\""
  defp select_column_to_string(column), do: column

  defp where_fragments(nil), do: []
  defp where_fragments(where_clause) do
    ["WHERE ", where_clause_to_fragments(where_clause)]
  end

  defp where_clause_to_fragments([_|_] = and_clauses) do
    ["(", and_clauses |> Enum.map(&where_clause_to_fragments/1) |> join(" AND "), ")"]
  end
  defp where_clause_to_fragments({:comparison, what, comparator, value}) do
    [to_fragment(what), to_fragment(comparator), to_fragment(value)]
  end
  defp where_clause_to_fragments({:in, what, values}) do
    [to_fragment(what), " IN (", values |> Enum.map(&to_fragment/1) |> join(","), ")"]
  end
  Enum.each([
    {:like, " LIKE "},
    {:ilike, " ILIKE "},
  ], fn({keyword, fragment}) ->
    defp where_clause_to_fragments({unquote(keyword), what, match}) do
      [to_fragment(what), unquote(fragment), to_fragment(match)]
    end
  end)

  defp to_fragment(string) when is_binary(string), do: string
  defp to_fragment(atom) when is_atom(atom), do: to_string(atom)
  defp to_fragment(%Token{category: :constant, value: value}), do: {:param, value.value}

  defp join([first | [_|_] = rest], joiner), do: [first, joiner, join(rest, joiner)]
  defp join([el], _joiner), do: [el]
  defp join([], _joiner), do: []


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
