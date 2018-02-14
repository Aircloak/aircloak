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
  alias Cloak.DataSource.{Driver, MongoDB.Schema, MongoDB.Pipeline}


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Driver

  @impl Driver
  def sql_dialect_module(_parameters), do: nil

  @impl Driver
  def connect!(parameters) do
    self = self()
    timeout = Driver.timeout()
    parameters = Enum.to_list(parameters) ++ [types: true, sync_connect: true, timeout: timeout, cursor_timeout: false,
      pool: DBConnection.Connection, pool_timeout: timeout, after_connect: fn (_) -> send self, :connected end]
    {:ok, connection} = Mongo.start_link(parameters)
    receive do
      :connected -> connection
    after Driver.connect_timeout()
      ->
        GenServer.stop(connection, :normal, :timer.seconds(5))
        DataSource.raise_error("Unknown failure during database connection process")
    end
  end

  @impl Driver
  def disconnect(connection), do:
    GenServer.stop(connection, :normal, :timer.seconds(5))

  @impl Driver
  def load_tables(connection, table) do
    table =
      table
      |> Map.put(:sharded?, is_sharded?(connection, table.db_name))
      # db_name is used for translating projections and decoders, so it needs to be adjusted in order to support
      # fake tables representing arrays, this means we need to save the original value under a different field
      |> Map.put(:collection, table.db_name)
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
    |> execute!([{:mapreduce, table.collection}, {:map, map_code}, {:reduce, reduce_code}, {:out, %{inline: 1}}])
    # 'array' and 'object' type values are only used for detection of 'mixed' type fields
    |> Enum.reject(fn (%{"value" => type}) -> type == "array" or type == "object" end)
    |> Enum.map(fn (%{"_id" => name, "value" => type}) -> {name, parse_type(type)} end)
    |> drop_unknown_subfields()
    |> Schema.build(table)
  end

  @impl Driver
  def select(connection, query, result_processor) do
    {collection, pipeline} = Pipeline.build(query)
    timeout = Driver.timeout()
    options = [max_time: timeout, timeout: timeout, pool_timeout: timeout,
      batch_size: Driver.batch_size(), allow_disk_use: true]
    mappers =
      query.db_columns
      |> Enum.map(&type_to_field_mapper(&1.type))
      |> Enum.with_index()
    result =
      connection
      |> Mongo.aggregate(collection, pipeline, options)
      |> Stream.map(&map_fields(&1, mappers))
      |> Stream.chunk_every(Driver.batch_size())
      |> result_processor.()
    {:ok, result}
  end

  @impl Driver
  def supports_query?(query), do: supports_joins?(query) and supports_order_by?(query)

  @impl Driver
  def supports_function?(expression, data_source), do:
    function_signature(expression) in (data_source |> mongo_version() |> supported_functions())

  @impl Driver
  def driver_info(connection) do
    {:ok, %{"version" => version}} = Mongo.command(connection, [{:buildInfo, true}])
    version
  end

  @impl Driver
  def supports_connection_sharing?(), do: true


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
    case Mongo.command(conn, command, timeout: Driver.timeout()) do
      {:ok, %{"results" => results}} -> results
      {:ok, %{"result" => result}} -> result
      {:error, %Mongo.Error{message: error}} -> DataSource.raise_error("MongoDB execute command error: #{error}")
    end
  end

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:interval), do: &interval_field_mapper/1
  defp type_to_field_mapper(_), do: &generic_field_mapper/1

  defp integer_field_mapper(nil), do: nil
  defp integer_field_mapper(value) when is_integer(value), do: value
  defp integer_field_mapper(value) when is_float(value), do: round(value)

  defp interval_field_mapper(nil), do: nil
  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)

  defp map_fields(row, mappers), do:
    Enum.map(mappers, fn ({mapper, index}) -> mapper.(row["f#{index}"]) end)

  defp generic_field_mapper(%BSON.ObjectId{value: value}), do: value
  defp generic_field_mapper(%BSON.Binary{binary: value}), do: value
  defp generic_field_mapper(%DateTime{} = datetime), do:
    datetime |> DateTime.to_naive() |> Cloak.Time.max_precision()
  defp generic_field_mapper(value), do: value

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

  defp mongo_version(%{driver_info: version}) when is_binary(version), do: version

  defp is_sharded?(connection, collection) do
    {:ok, stats} = Mongo.command(connection, [{:collStats, collection}])
    stats["sharded"] == true
  end

  @supported_functions_3_0 ~w(+ - * / % mod count sum
    || concat lower upper lcase ucase year month day weekday hour minute second
    cast_integer_to_boolean cast_real_to_boolean cast_boolean_to_integer cast_boolean_to_real
    cast_boolean_to_text cast_text_to_boolean cast_integer_to_real cast_datetime_to_text)
  @supported_functions_3_2 @supported_functions_3_0 ++
    ~w(^ abs ceil floor round sqrt trunc quarter div cast_real_to_integer min max avg pow)
  @supported_functions_3_4 @supported_functions_3_2 ++
    ~w(length left right substring cast_real_to_text cast_integer_to_text)
  defp supported_functions(version) do
    cond do
      Version.compare(version, "3.0.0") == :lt ->
        DataSource.raise_error("Unsupported MongoDB version: #{version}. At least 3.0 required.")
      Version.compare(version, "3.2.0") == :lt -> @supported_functions_3_0
      Version.compare(version, "3.4.0") == :lt -> @supported_functions_3_2
      true -> @supported_functions_3_4
    end
  end

  defp function_signature(%Expression{function: {:cast, type}, function_args: [value]}), do:
    "cast_#{value.type}_to_#{type}"
  defp function_signature(%Expression{function?: true, function: name}), do: name

  defp supports_joins?(%Query{from: {:join, join}} = query) do
    mongo_version_supports_joins?(query) and
    join.type == :inner_join and
    supports_join_conditions?(join.conditions) and
    supports_join_branches?(query.selected_tables, join.lhs, join.rhs)
  end
  defp supports_joins?(_query), do: true

  defp supports_order_by?(%{subquery?: false}), do: true
  defp supports_order_by?(%{subquery?: true, order_by: order_by}), do:
    Enum.all?(order_by, fn
      {_, _, :nulls_natural} -> true
      {_, :asc, :nulls_first} -> true
      {_, :desc, :nulls_last} -> true
      _ -> false
    end)

  defp mongo_version_supports_joins?(%{data_source: data_source}), do:
    data_source |> mongo_version() |> Version.compare("3.2.0") != :lt

  defp supports_join_conditions?({:comparison, lhs, :=, rhs}), do: lhs.name != nil and rhs.name != nil
  defp supports_join_conditions?(_conditions), do: false

  defp supports_join_branches?(selected_tables, lhs, rhs), do:
    (is_binary(lhs) or is_binary(rhs)) and
    simple_branch?(lhs) and simple_branch?(rhs) and
    not sharded_table?(selected_tables, lhs) and not sharded_table?(selected_tables, rhs)

  defp simple_branch?({:join, _}), do: false
  defp simple_branch?(_), do: true

  defp sharded_table?(selected_tables, table) when is_binary(table) do
    table = Enum.find(selected_tables, & &1.name == table)
    table != nil and table.sharded?
  end
  defp sharded_table?(_selected_tables, _table), do: false
end
