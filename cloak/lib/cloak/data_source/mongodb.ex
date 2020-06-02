defmodule Cloak.DataSource.MongoDB do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MongoDB.
  For more information, see `DataSource`.

  Sample configuration for a MongoDB data source:
  {
    "driver": "mongodb",
    "parameters": {
      "hostname": "...",
      "username": "...",
      "password": "...",
      "database": "..."
    },
    "tables": {
      "test": {
        "db_name": "...",     - optional
        "query": "...",       - optional
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
  alias Cloak.Query.ExecutionError
  alias Cloak.DataSource.{Driver, MongoDB.Schema, MongoDB.Pipeline}

  require Logger

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Driver

  @impl Driver
  def sql_dialect_module(), do: nil

  @impl Driver
  def connect(parameters) do
    parameters =
      Enum.to_list(parameters) ++
        [
          types: true,
          sync_connect: true,
          backoff_type: :stop,
          timeout: Driver.timeout(),
          cursor_timeout: false
        ]

    case Mongo.start_link(parameters) do
      {:ok, connection} -> {:ok, connection}
      {:error, {%Mongo.Error{} = error, _stacktrace}} -> {:error, error.message}
    end
  end

  @impl Driver
  def health_check(_connection), do: :ok

  @impl Driver
  def disconnect(connection), do: GenServer.stop(connection, :normal, :timer.seconds(5))

  @impl Driver
  def load_tables(connection, table) do
    table =
      table
      |> Map.put(:sharded?, is_sharded?(connection, table.db_name))
      # db_name needs to be adjusted in order to support fake tables representing arrays,
      # this means we need to save the original value under a different field
      |> Map.put(:collection, table.db_name)

    sample_rate = table[:sample_rate] || 100

    unless is_integer(sample_rate) and sample_rate >= 1 and sample_rate <= 100,
      do: raise(ExecutionError, message: "Sample rate for schema detection has to be an integer between 1 and 100.")

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
    |> execute!([
      {:mapreduce, table.collection},
      {:map, map_code},
      {:reduce, reduce_code},
      {:out, %{inline: 1}}
    ])
    # 'array' and 'object' type values are only used for detection of 'mixed' type fields
    |> Enum.reject(fn %{"value" => type} -> type == "array" or type == "object" end)
    |> Enum.map(fn %{"_id" => name, "value" => type} -> {name, parse_type(type)} end)
    |> drop_unknown_subfields()
    |> Schema.build(table)
  end

  @impl Driver
  def select(connection, query, result_processor) do
    {collection, pipeline} = Pipeline.build(query)
    Logger.debug(fn -> "Executing MongoDB pipeline on collection #{inspect(collection)}: #{inspect(pipeline)}" end)
    timeout = Driver.timeout()

    options = [
      max_time: timeout,
      timeout: timeout,
      pool_timeout: timeout,
      batch_size: Driver.batch_size(),
      allow_disk_use: true
    ]

    mappers = Enum.map(query.db_columns, &type_to_field_mapper(&1.type))

    result =
      connection
      |> Mongo.aggregate(collection, pipeline, options)
      |> Stream.map(&map_fields(&1, mappers))
      |> Stream.chunk_every(Driver.batch_size())
      |> result_processor.()

    {:ok, result}
  end

  @impl Driver
  def supports_query?(query) do
    Enum.all?(query.selected_tables, &(&1[:sharded?] != true)) and supports_order_by?(query) and
      supports_aggregators?(query) and supports_grouping?(query)
  end

  @impl Driver
  def supports_function?(expression, data_source),
    do: function_signature(expression) in (data_source |> mongo_version() |> supported_functions())

  @impl Driver
  def driver_info(connection) do
    {:ok, %{"version" => version}} = Mongo.command(connection, [{:buildInfo, true}])
    version
  end

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
      {:error, %Mongo.Error{message: error}} -> raise "MongoDB execute command error: #{error}"
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

  defp map_fields(%{"row" => row}, mappers),
    do: Enum.zip(row, mappers) |> Enum.map(fn {field, mapper} -> mapper.(field) end)

  defp generic_field_mapper(%BSON.ObjectId{value: value}), do: value
  defp generic_field_mapper(%BSON.Binary{binary: value}), do: value

  defp generic_field_mapper(%DateTime{} = datetime), do: datetime |> DateTime.to_naive() |> Cloak.Time.max_precision()

  defp generic_field_mapper(value), do: value

  # In the case of arrays or objects with mixed types, we need to drop all their subfields.
  defp drop_unknown_subfields(fields) do
    for({name, :unknown} <- fields, do: name)
    |> Enum.reduce(fields, fn unknown_field, filtered_fields ->
      array_prefix = unknown_field <> "[]"
      object_prefix = unknown_field <> "."

      Enum.reject(filtered_fields, fn {name, _type} ->
        String.starts_with?(name, array_prefix) or String.starts_with?(name, object_prefix)
      end)
    end)
  end

  defp mongo_version(%{driver_info: version}) when is_binary(version), do: version

  defp is_sharded?(connection, collection) do
    case Mongo.command(connection, [{:collStats, collection}]) do
      {:ok, stats} ->
        stats["sharded"] == true

      {:error, %Mongo.Error{message: error}} ->
        raise ExecutionError, message: "Error getting sharding status for collection `#{collection}`: #{error}."
    end
  end

  @supported_functions_36 ~w(
    + - * / % unsafe_add unsafe_sub unsafe_mul unsafe_div unsafe_mod checked_div checked_mod
    count sum || concat lower upper year month day weekday hour minute second
    cast_integer_to_boolean cast_real_to_boolean cast_boolean_to_integer cast_boolean_to_real
    cast_boolean_to_text cast_text_to_boolean cast_integer_to_real cast_datetime_to_text
    ^ abs ceil floor round sqrt trunc quarter div cast_real_to_integer min max avg
    length left right substring cast_real_to_text cast_integer_to_text coalesce case
    < > <= >= = <> and or not in is_null !<> stddev variance
  )

  @supported_functions_42 @supported_functions_36 ++ ~w(like ilike)

  defp supported_functions(version) do
    cond do
      Version.compare(version, "3.6.0") == :lt ->
        raise ExecutionError, message: "Unsupported MongoDB version: #{version}. At least 3.6 required."

      Version.compare(version, "4.2.0") == :lt ->
        @supported_functions_36

      true ->
        @supported_functions_42
    end
  end

  defp function_signature(%Expression{kind: :function, name: {:cast, type}, args: [value]}),
    do: "cast_#{value.type}_to_#{type}"

  defp function_signature(%Expression{kind: :function, name: name}), do: name

  defp supports_order_by?(%{type: :anonymized}), do: true

  defp supports_order_by?(%{order_by: order_by}),
    do:
      Enum.all?(order_by, fn
        {_, _, :nulls_natural} -> true
        {_, :asc, :nulls_first} -> true
        {_, :desc, :nulls_last} -> true
        _ -> false
      end)

  # Offloaded global aggregators do not work properly as the `$group` operator return an empty output on empty input.
  defp supports_aggregators?(%Query{aggregators: [_ | _], implicit_count?: false, type: :standard}), do: false
  defp supports_aggregators?(_query), do: true

  defp supports_grouping?(query), do: query.type == :anonymized or length(query.grouping_sets) <= 1
end
