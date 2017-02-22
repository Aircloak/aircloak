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

  alias Cloak.Sql.Expression
  alias Cloak.Query.Runner.RuntimeError
  alias Cloak.DataSource.MongoDB.{Schema, Pipeline}


  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @timeout :timer.hours(1)

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
        raise RuntimeError, message: "Could not connect to the MongoDB server!"
    end
  end

  @doc false
  def disconnect(connection), do:
    GenServer.stop(connection)

  @doc false
  def load_tables(connection, table) do
    sample_rate = table[:sample_rate] || 100
    unless is_integer(sample_rate) and sample_rate >= 1 and sample_rate <= 100, do:
      raise RuntimeError, "Sample rate for schema detection has to be an integer between 1 and 100."
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
    columns = for %Expression{name: name, alias: alias} <- query.db_columns, do: String.split(alias || name, ".")
    options = [max_time: @timeout, timeout: @timeout, pool_timeout: @timeout, batch_size: 25_000, allow_disk_use: true]
    result =
      connection
      |> Mongo.aggregate(collection, pipeline, options)
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
  defp parse_type("mixed"), do: :unknown
  defp parse_type("date"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}

  defp execute!(conn, command) do
    case Mongo.command(conn, command, timeout: @timeout) do
      {:ok, %{"results" => results}} -> results
      {:ok, %{"result" => result}} -> result
      {:error, %Mongo.Error{message: error}} -> raise RuntimeError, "MongoDB execute command error: #{error}"
    end
  end

  defp extract_fields(object, columns), do:
    for column <- columns, do: object |> extract_field(column) |> map_field()

  defp extract_field(nil, _), do: nil
  defp extract_field(value, []), do: value
  defp extract_field(%{} = object, [key | rest]), do: extract_field(object[key], rest)

  defp map_field(%BSON.ObjectId{value: value}), do: value
  defp map_field(%BSON.Binary{binary: value}), do: value
  defp map_field(%BSON.DateTime{} = value) do
    {{year, month, day}, {hour, minute, second, usec}} = BSON.DateTime.to_datetime(value)
    NaiveDateTime.new(year, month, day, hour, minute, second, {usec, 6}) |> error_to_nil()
  end
  defp map_field(value), do: value

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
end
