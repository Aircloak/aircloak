defmodule Cloak.DataSource.MongoDB do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MongoDB.
  For more information, see `DataSource`.

  Sample configuration for a MongoDB data source:
  {
    "driver": "mongodb",
    "marker": "mongodb",      - optional
    "parameters": {
      "hostname": "...",
      "username": "...",
      "password": "...",
      "database": "..."
    },
    "tables": {
      "test": {
        "db_name": "...",     - optional
        "user_id": "...",
        "sample_rate": ...    - optional
      }
    }
  }

  During table loading, a map-reduce job is executed over documents in the collection in order to map out
  the schema of the data. By default, all documents are looked at. This operation can be pretty slow, so,
  for big tables, only a random sample of the documents could be taken into account by setting the optional
  parameters `sample_rate`, which has to be an integer value between 1 and 100.

  Array handling:
    Since SQL has no native support for arrays, we need to expose arrays as virtual columns and tables.
    For each array, a virtual field is created for the array size in the base table, the name of the
    column being the array name suffixed with `#`. In addition, a virtual table is created that unwinds
    the array data by duplicating the base fields and adds an extra row for each array item, with the
    name of the table being the base table name suffixed with the array name, separated by `_`.
  """

  alias Cloak.Sql.{Query, Expression}
  alias Cloak.DataSource
  alias Cloak.DataSource.MongoDB.{Schema, Pipeline}
  alias Cloak.Query.DataDecoder


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @timeout :timer.hours(1)

  @doc false
  def sql_dialect_module(_parameters), do: nil

  @doc false
  def connect!(parameters) do
    self = self()
    parameters = Enum.to_list(parameters) ++ [types: true, sync_connect: true, timeout: @timeout,
      pool: DBConnection.Connection, pool_timeout: @timeout, after_connect: fn (_) -> send self, :connected end]
    {:ok, connection} = Mongo.start_link(parameters)
    receive do
      :connected -> connection
    after :timer.seconds(5)
      ->
        GenServer.stop(connection)
        DataSource.raise_error("Unknown failure during database connection process")
    end
  end

  @doc false
  def disconnect(connection), do:
    GenServer.stop(connection)

  @doc false
  def load_tables(connection, table) do
    table =
      table
      |> Map.put(:mongo_version, get_server_version(connection))
      |> Map.put(:sharded?, is_sharded?(connection, table.db_name))
    sample_rate = table[:sample_rate] || 100
    unless is_integer(sample_rate) and sample_rate >= 1 and sample_rate <= 100, do:
      DataSource.raise_error("Sample rate for schema detection has to be an integer between 1 and 100.")
    map_code = """
      function() {
        if (Math.random() * 100 > #{sample_rate}) return;
        map_subfield = function(base, object) {
          if (object === undefined || object == null) {
            return;
          } else if (object instanceof Date) {
            emit(base, "date");
          } else if (object instanceof ObjectId) {
            emit(base, "object_id");
          } else if (object instanceof BinData) {
            emit(base, "string");
          } else if (Array.isArray(object)) {
            emit(base, "array");
            map_subfield(base + "[]", object[0]);
          } else if (typeof object == "object") {
            emit(base, "object");
            for(var key in object) {
              map_subfield(base + "." + key, object[key]);
            }
          } else {
            emit(base, typeof object);
          }
        };
        for(var key in this) {
          map_subfield(key, this[key]);
        }
      }
    """
    reduce_code = """
      function(key, types) {
        return types.every(function (type) { return type === types[0]; }) ? types[0] : "mixed";
      }
    """
    connection
    |> execute!([{:mapreduce, table.db_name}, {:map, map_code}, {:reduce, reduce_code}, {:out, %{inline: 1}}])
    # 'array' and 'object' type values are only used for detection of 'mixed' type fields
    |> Enum.reject(fn (%{"value" => type}) -> type == "array" or type == "object" end)
    |> Enum.map(fn (%{"_id" => name, "value" => type}) -> {name, parse_type(type)} end)
    |> drop_unknown_subfields()
    |> Schema.build(table)
  end

  @doc false
  def select(connection, query, result_processor) do
    {collection, pipeline} = Pipeline.build(query)
    options = [max_time: @timeout, timeout: @timeout, pool_timeout: @timeout, batch_size: 25_000, allow_disk_use: true]
    mappers =
      query.db_columns
      |> Enum.map(& &1 |> DataDecoder.encoded_type() |> type_to_field_mapper())
      |> Enum.with_index()
    result =
      connection
      |> Mongo.aggregate(collection, pipeline, options)
      |> Stream.map(&map_fields(&1, mappers))
      |> result_processor.()
    {:ok, result}
  end

  @doc false
  def supports_query?(query), do:
    supports_used_functions?(query) and
    supports_used_functions_in_having?(query) and
    supports_joins?(query)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parse_type("object_id"), do: :text
  defp parse_type("number"), do: :real
  defp parse_type("boolean"), do: :boolean
  defp parse_type("string"), do: :text
  defp parse_type("mixed"), do: :unknown
  defp parse_type("date"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}

  defp execute!(conn, command) do
    case Mongo.command(conn, command, timeout: @timeout) do
      {:ok, %{"results" => results}} -> results
      {:ok, %{"result" => result}} -> result
      {:error, %Mongo.Error{message: error}} -> DataSource.raise_error("MongoDB execute command error: #{error}")
    end
  end

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(_), do: &generic_field_mapper/1

  defp integer_field_mapper(nil), do: nil
  defp integer_field_mapper(value) when is_integer(value), do: value
  defp integer_field_mapper(value) when is_float(value), do: round(value)

  defp map_fields(row, mappers), do:
    Enum.map(mappers, fn ({mapper, index}) -> mapper.(row["f#{index}"]) end)

  defp generic_field_mapper(%BSON.ObjectId{value: value}), do: value
  defp generic_field_mapper(%BSON.Binary{binary: value}), do: value
  defp generic_field_mapper(%BSON.DateTime{} = value) do
    {{year, month, day}, {hour, minute, second, usec}} = BSON.DateTime.to_datetime(value)
    NaiveDateTime.new(year, month, day, hour, minute, second, {usec, 6}) |> error_to_nil()
  end
  defp generic_field_mapper(value), do: value

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  # In the case of arrays or objects with mixed types, we need to drop all their subfields.
  defp drop_unknown_subfields(fields) do
    (for {name, :unknown} <- fields, do: name)
    |> Enum.reduce(fields, fn (unknown_field, filtered_fields) ->
        array_prefix = unknown_field <> "[]"
        object_prefix = unknown_field <> "."
        Enum.reject(filtered_fields, fn ({name, _type}) ->
          String.starts_with?(name, array_prefix) or String.starts_with?(name, object_prefix)
        end)
    end)
  end

  defp get_server_version(connection) do
    {:ok, %{"version" => version}} = Mongo.command(connection, [{:buildInfo, true}])
    version
  end

  defp get_mongo_version(data_source) do
    {_name, %{mongo_version: version}} = Enum.at(data_source.tables, 0)
    version
  end

  defp is_sharded?(connection, collection) do
    {:ok, stats} = Mongo.command(connection, [{:collStats, collection}])
    stats["sharded"] == true
  end

  @supported_functions_3_0 ~w(+ - * ^ / % mod count sum min max avg
    || concat lower upper lcase ucase year month day weekday hour minute second
    cast_integer_to_real cast_real_to_text cast_integer_to_text cast_datetime_to_text cast_boolean_to_text)
  @supported_functions_3_2 @supported_functions_3_0 ++ 
    ~w(abs ceil floor round sqrt trunc quarter div cast_real_to_integer)
  @supported_functions_3_4 @supported_functions_3_2 ++ ~w(length left right substring)
  defp supported_functions(version) do
    cond do
      Version.compare(version, "3.0.0") == :lt ->
        DataSource.raise_error("Unsupported MongoDB version: #{version}. At least 3.0 required.")
      Version.compare(version, "3.2.0") == :lt -> @supported_functions_3_0
      Version.compare(version, "3.4.0") == :lt -> @supported_functions_3_2
      true -> @supported_functions_3_4
    end
  end

  defp supports_used_functions_in_having?(%Query{subquery?: true} = query) do
    Query.Lenses.conditions()
    |> Query.Lenses.operands()
    |> Lens.satisfy(& &1.function? and not &1.aggregate?)
    |> Lens.to_list(query.having) == []
  end
  defp supports_used_functions_in_having?(_query), do: true

  defp supports_used_functions?(query) do
    used_functions = Query.Lenses.db_needed_functions() |> Lens.to_list(query) |> Enum.map(&function_signature/1)
    supported_functions = query.data_source |> get_mongo_version() |> supported_functions()
    Enum.reject(used_functions, & &1 in supported_functions) == []
  end

  defp function_signature(%Expression{function: {:cast, type}, function_args: [value]}), do:
    "cast_#{value.type}_to_#{type}"
  defp function_signature(%Expression{function?: true, function: name}), do: name

  defp supports_joins?(%Query{from: {:join, join}} = query) do
    # join support was added in 3.2
    (query.data_source |> get_mongo_version() |> Version.compare("3.2.0") != :lt) and
    join.type == :inner_join and
    supported_join_conditions?(join.conditions) and
    supported_join_branches?(query.selected_tables, join.lhs, join.rhs)
  end
  defp supports_joins?(_query), do: true

  defp supported_join_conditions?({:comparison, lhs, :=, rhs}), do: lhs.name != nil and rhs.name != nil
  defp supported_join_conditions?(_conditions), do: false

  defp supported_join_branches?(selected_tables, lhs, rhs), do:
    (is_binary(lhs) or is_binary(rhs)) and
    not sharded_table?(selected_tables, lhs) and
    not sharded_table?(selected_tables, rhs)

  defp sharded_table?(selected_tables, table) when is_binary(table) do
    table = Enum.find(selected_tables, & &1.name == table)
    table != nil and table.sharded?
  end
  defp sharded_table?(_selected_tables, _table), do: false
end
