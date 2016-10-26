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
  def describe_table(connection, table_name) do
    # We assume that all fields have a single, consistent type (no mixing of values).
    map_code = """
      function() {
        m_sub = function(base, object) {
          if (object instanceof Date) {
            emit(base, "date");
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
            emit("_id", "id");
          }
        }
      }
    """
    reduce_code = """
      function(key, types) {
        return types[0];
      }
    """
    connection
    |> mapreduce(table_name, map_code, reduce_code)
    |> Enum.map(fn (%{"_id" => name, "value" => type}) -> {name, parse_type(type)} end)
  end

  @doc false
  def select(connection, query, result_processor) do
    {collection, columns, selector, projector} = parse_query(query)
    columns = Enum.map(columns, &String.split(&1, "."))

    result =
      connection
      |> :mc_worker_api.find(collection, selector, %{projector: projector})
      |> cursor_to_stream()
      |> Stream.map(&extract_fields(&1, columns))
      |> result_processor.()
    {:ok, result}
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp parse_type("id"), do: :text
  defp parse_type("number"), do: :real
  defp parse_type("boolean"), do: :boolean
  defp parse_type("string"), do: :text
  defp parse_type("date"), do: :datetime

  @dialyzer [:no_fail_call, :no_return] # `mc_worker_api.command` has incorrect type spec.
  defp mapreduce(conn, collection, map_code, reduce_code) do
    {true, %{"results" => results}} = :mc_worker_api.command(
      conn,
      {
        :mapreduce, collection,
        :map, map_code,
        :reduce, reduce_code,
        :out, {:inline, 1}
      }
    )
    results
  end

  defp parse_query(%Query{from: name} = query) when is_binary(name) do
    columns = for %Column{name: name} <- query.db_columns, do: name
    projector = for column <- columns, into: %{"_id" => false}, do: {column, true}
    [%Column{table: %{db_name: collection}} | _] = query.db_columns
    selector = parse_where_clause(query.where)
    {collection, columns, selector, projector}
  end
  defp parse_query(%Query{}), do:
    raise RuntimeError, message: "Table joins and inner selects are not supported on 'mongodb' data sources."

  defp extract_fields(object, columns), do:
    for column <- columns, do: object |> extract_field(column) |> map_field()

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

  defp cursor_to_stream(cursor) do
    Stream.resource(
      fn () -> cursor end,
      fn (cursor) ->
        case :mc_cursor.take(cursor, 25_000, :timer.minutes(5)) do
          :error -> {:halt, cursor}
          objects -> {objects, cursor}
        end
      end,
      &:mc_cursor.close/1
    )
  end

  defp parse_where_clause([]), do: %{}
  defp parse_where_clause([condition]), do: parse_where_condition(condition)
  defp parse_where_clause(conditions), do: %{'$and': Enum.map(conditions, &parse_where_condition/1)}

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

  defp map_parameter(%NaiveDateTime{} = datetime), do:
    datetime |> NaiveDateTime.to_erl() |> erlang_datetime_to_timestamp(datetime.microsecond)
  defp map_parameter(%Date{} = date), do:
    date |> Date.to_erl() |> erlang_datetime_to_timestamp()
  defp map_parameter(%Column{value: value}), do: value

  @epoch :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
  defp erlang_datetime_to_timestamp(datetime, {microsecond, _precision} \\ {0, 0}) do
    seconds = :calendar.datetime_to_gregorian_seconds(datetime) - @epoch
    {seconds |> div(1_000_000), seconds |> rem(1_000_000), microsecond}
  end
end
