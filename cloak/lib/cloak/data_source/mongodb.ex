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
          for(var key in object) {
            const value = object[key];
            emit(base + "." + key, typeof value);
            if(typeof value == 'object') {
              m_sub(base + "." + key, value);
            }
          }
        };
        for(var key in this) {
          const value = this[key];
          if(key != "_id" && typeof value == 'object') {
            m_sub(key, value);
          } else {
            emit(key, typeof value);
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

  defp parse_type("object"), do: :text
  defp parse_type("number"), do: :real
  defp parse_type("boolean"), do: :boolean
  defp parse_type("string"), do: :text
  defp parse_type("datetime"), do: :datetime

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
    for column <- columns, do: extract_field(object, column)

  defp extract_field(nil, _), do: nil
  defp extract_field(%{"_id" => {value}}, ["_id"]), do: value
  defp extract_field(value, []), do: value
  defp extract_field(%{} = object, [key | rest]), do: extract_field(object[key], rest)

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

  defp parse_where_condition({:comparison, %Column{name: field}, operator, %Column{value: value}}), do:
    %{field => %{parse_operator(operator) => value}}
  defp parse_where_condition({:not, {:comparison, %Column{name: field}, :=, %Column{value: value}}}), do:
    %{field => %{'$neq': value}}
  defp parse_where_condition({:is, %Column{name: field}, :null}), do: %{field => nil}
  defp parse_where_condition({:not, {:is, %Column{name: field}, :null}}), do: %{field => %{'$exists': true}}
  defp parse_where_condition({:in, %Column{name: field}, values}), do:
    %{field => %{'$in': (for %Column{value: value} <- values, do: value)}}
  defp parse_where_condition({:not, {:in, %Column{name: field}, values}}), do:
    %{field => %{'$nin': (for %Column{value: value} <- values, do: value)}}
  defp parse_where_condition({:like, %Column{name: field}, %Column{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}
  defp parse_where_condition({:ilike, %Column{name: field}, %Column{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}
  defp parse_where_condition({:not, {:like, %Column{name: field}, %Column{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}}
  defp parse_where_condition({:not, {:ilike, %Column{name: field}, %Column{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}}
end
