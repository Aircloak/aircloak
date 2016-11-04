defmodule Cloak.DataSource.MongoDB do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MongoDB.
  For more information, see `DataSource`.

  Sample configuration for a MongoDB data source:
  {
    "driver": "mongodb",
    "marker": "mongodb",
    "parameters": {
      "host": "...",
      "login": "...",
      "password": "...",
      "database": "..."
    },
    "tables": {
      "test": {
        "db_name": "...",
        "user_id": "_id"
      }
    }
  }

  Array handling:
    Since SQL has no native support for arrays, we need to expose arrays as virtual columns and tables.
    For each array, a virtual field is created for the array size in the base table, the name of the
    column being the array name suffixed with `#`. In addition, a virtual table is created that unwinds
    the array data by duplicating the base fields and adds an extra row for each array item, with the
    name of the table being the base table name suffixed with the array name, separated by `_`.
  """

  alias Cloak.Aql.{Query, Column, Comparison}
  alias Cloak.Query.Runner.RuntimeError


  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def connect(parameters), do:
    parameters
    |> Enum.to_list()
    |> Keyword.update!(:host, &to_char_list/1)
    |> :mc_worker_api.connect()
  @doc false
  def disconnect(connection), do:
    :mc_worker_api.disconnect(connection)

  @doc false
  def load_tables(connection, table) do
    map_code = """
      function() {
        m_sub = function(base, object) {
          if (object instanceof Date) {
            emit(base, "date");
          } else if (Array.isArray(object)) {
            m_sub(base + "[]", object[0]);
          } else if (typeof object == "object") {
            for(var key in object) {
              m_sub(base + "." + key, object[key]);
            }
          } else {
            emit(base, typeof object);
          }
        };
        for(var key in this) {
          if (key != "_id") {
            m_sub(key, this[key]);
          } else {
            emit("_id", "object_id");
          }
        }
      }
    """
    reduce_code = """
      function(key, types) {
        return types.every(function (type) { return type === types[0]; }) ? types[0] : "mixed";
      }
    """
    connection
    |> execute!({:mapreduce, table.db_name, :map, map_code, :reduce, reduce_code, :out, {:inline, 1}})
    |> Enum.map(fn (%{"_id" => name, "value" => type}) ->
      {String.split(name, "[]"), parse_type(type)}
    end)
    |> Enum.reduce(%{}, &build_schema/2)
    |> build_tables(Map.put(table, :array_path, []))
  end

  @doc false
  def select(connection, query, result_processor) do
    {collection, columns, pipeline} = parse_query(query)
    columns = Enum.map(columns, &String.split(&1, "."))

    result =
      connection
      |> execute!({:aggregate, collection, :pipeline, pipeline})
      |> Stream.map(&extract_fields(&1, columns))
      |> result_processor.()
    {:ok, result}
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp parse_type("object_id"), do: :text
  defp parse_type("number"), do: :real
  defp parse_type("boolean"), do: :boolean
  defp parse_type("string"), do: :text
  defp parse_type("date"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}

  @dialyzer [:no_fail_call, :no_return, :no_unused] # `mc_worker_api.command` has incorrect type spec.
  defp execute!(conn, command) do
    case :mc_worker_api.command(conn, command) do
      {true, %{"results" => results}} -> results
      {true, %{"result" => result}} -> result
      {false, %{"errmsg" => error}} -> raise RuntimeError, "MongoDB execute command error: #{error}"
    end
  end

  defp parse_query(%Query{from: name} = query) when is_binary(name) do
    [%Column{table: table} | _] = query.db_columns
    columns = for %Column{name: name} <- query.db_columns, do: name
    {base_conditions, array_conditions, array_size_conditions} = split_conditions(table.array_path, query.where)
    pipeline =
      parse_where_conditions(base_conditions) ++
      unwind_arrays(table.array_path) ++
      parse_where_conditions(array_conditions) ++
      project_array_sizes(table) ++
      parse_where_conditions(array_size_conditions) ++
      project_columns(columns)
    {table.db_name, columns, pipeline}
  end
  defp parse_query(%Query{}), do:
    raise RuntimeError, message: "Table joins and inner selects are not supported on 'mongodb' data sources."

  defp extract_fields(object, columns), do:
    for column <- columns, do: object |> extract_field(column) |> map_field()

  defp is_array_size?(name), do: String.contains?(name, "#")

  defp extract_field(nil, _), do: nil
  defp extract_field(%{"_id" => {value}}, ["_id"]), do: value
  defp extract_field(value, []), do: value
  defp extract_field(%{} = object, [key | rest]), do: extract_field(object[key], rest)

  defp map_field({mega_sec, sec, micro_sec}), do:
    {mega_sec, sec, 0}
    |> :calendar.now_to_datetime()
    |> NaiveDateTime.from_erl({micro_sec, 6})
    |> error_to_nil()
  defp map_field(value), do: value

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  defp parse_where_conditions([]), do: []
  defp parse_where_conditions([condition]), do: [%{'$match': parse_where_condition(condition)}]
  defp parse_where_conditions(conditions), do: [%{'$match': %{'$and': Enum.map(conditions, &parse_where_condition/1)}}]

  defp parse_operator(:=), do: :'$eq'
  defp parse_operator(:>), do: :'$gt'
  defp parse_operator(:>=), do: :'$gte'
  defp parse_operator(:<), do: :'$lt'
  defp parse_operator(:<=), do: :'$lte'
  defp parse_operator(:<>), do: :'$neq'

  defp parse_where_condition({:comparison, %Column{name: field}, operator, value}), do:
    %{field => %{parse_operator(operator) => map_parameter(value)}}
  defp parse_where_condition({:not, {:comparison, %Column{name: field}, :=, value}}), do:
    %{field => %{'$neq': map_parameter(value)}}
  defp parse_where_condition({:is, %Column{name: field}, :null}), do: %{field => nil}
  defp parse_where_condition({:not, {:is, %Column{name: field}, :null}}), do: %{field => %{'$exists': true}}
  defp parse_where_condition({:in, %Column{name: field}, values}), do:
    %{field => %{'$in': Enum.map(values, &map_parameter/1)}}
  defp parse_where_condition({:not, {:in, %Column{name: field}, values}}), do:
    %{field => %{'$nin': Enum.map(values, &map_parameter/1)}}
  defp parse_where_condition({:like, %Column{name: field}, %Column{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}
  defp parse_where_condition({:ilike, %Column{name: field}, %Column{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}
  defp parse_where_condition({:not, {:like, %Column{name: field}, %Column{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}}
  defp parse_where_condition({:not, {:ilike, %Column{name: field}, %Column{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}}

  defp split_conditions([], conditions) do
    {array_size_conditions, non_array_size_conditions} =
      Enum.partition(conditions, &Comparison.column(&1) |> is_array_size?())
    {non_array_size_conditions, [], array_size_conditions}
  end
  defp split_conditions([array | _], conditions) do
    {array_size_conditions, non_array_size_conditions} =
      Enum.partition(conditions, &Comparison.column(&1) |> is_array_size?())
    {array_conditions, base_conditions} =
      Enum.partition(non_array_size_conditions, &Comparison.column(&1) |> String.starts_with?(array <> "."))
    {base_conditions, array_conditions, array_size_conditions}
  end

  defp project_array_sizes(table) do
    columns = for {name, _type} <- table.columns, do: name
    case Enum.partition(columns, &is_array_size?/1) do
      {_, []} -> []
      {array_sizes, regular_columns} ->
        projected_columns =
          Enum.map(regular_columns, &{&1, true}) ++
          Enum.map(array_sizes, &{&1, %{'$size': "$" <> String.replace(&1, "#", "")}})
        [%{'$project': Enum.into(projected_columns, %{})}]
    end
  end

  defp project_columns(columns), do:
    [%{'$project': (for name <- columns, into: %{"_id" => false}, do: {name, true})}]

  defp map_parameter(%NaiveDateTime{} = datetime), do:
    datetime |> NaiveDateTime.to_erl() |> erlang_datetime_to_timestamp(datetime.microsecond)
  defp map_parameter(%Date{} = date), do: erlang_datetime_to_timestamp({Date.to_erl(date), {0, 0, 0}})
  defp map_parameter(%Column{value: value}), do: value

  @epoch :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
  defp erlang_datetime_to_timestamp(datetime, {microsecond, _precision} \\ {0, 0}) do
    seconds = :calendar.datetime_to_gregorian_seconds(datetime) - @epoch
    {seconds |> div(1_000_000), seconds |> rem(1_000_000), microsecond}
  end

  defp schema_append(schema, key, value), do: Map.update(schema, key, [value], & &1 ++ [value])

  defp build_schema({[name], type}, schema), do: schema_append(schema, :base, {name, type})
  defp build_schema({[array, ""], type}, schema), do:
    schema
    |> schema_append(:base, {array <> "#", :integer})
    |> Map.put(array, %{})
    |> Map.put(array, type)
  defp build_schema({[array | name], type}, schema) do
    schema = case schema[array] do
      nil ->
        schema
        |> schema_append(:base, {array <> "#", :integer})
        |> Map.put(array, %{})
      _ ->
        schema
    end
    array_schema = build_schema({name, type}, schema[array])
    Map.put(schema, array, array_schema)
  end

  defp build_tables(_schema, _table, _parent_columns \\ [])
  defp build_tables(%{} = schema, table, parent_columns) do
    columns =
      Enum.reject(parent_columns, fn ({name, _type}) -> is_array_size?(name) end) ++
      Enum.map(schema.base, fn ({name, type}) -> {to_string(table.array_path) <> name, type} end)
    array_tables =
      for array <- Map.keys(schema) -- [:base] do
        array_table = %{table | name: build_table_name(table.name, array), array_path: table.array_path ++ [array]}
        build_tables(schema[array], array_table, columns)
      end
    [%{table | columns: columns} | List.flatten(array_tables)]
  end
  defp build_tables(type, table, parent_columns) do
    columns =
      Enum.reject(parent_columns, fn ({name, _type}) -> is_array_size?(name) end) ++
      [{to_string(table.array_path), type}]
    [%{table | columns: columns}]
  end

  defp build_table_name(parent, "." <> array), do: build_table_name(parent, array)
  defp build_table_name(parent, array), do: "#{parent}_#{array}"

  defp unwind_arrays(_path, _path \\ "")
  defp unwind_arrays([], _path), do: []
  defp unwind_arrays([array | rest], path) do
    path = path <> array
    [%{'$unwind': "$" <> path} | unwind_arrays(rest, path)]
  end
end
