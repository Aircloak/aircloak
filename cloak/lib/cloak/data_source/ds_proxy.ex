defmodule Cloak.DataSource.DsProxy do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Paul's Anonymizing Query Language backend.
  For more information, see `DataSource`.

  To use this data-source you need to add it to the list of data sources that should
  be served by your cloak and declare where the dsproxy backend can be found.

  A sample deploy configuration could look like this:

      "data_sources": {
        "dsproxy": {
          driver: "dsproxy",
          "parameters": {"url": "http://localhost:8000"},
          "tables": {
            "<table-display-name>": {
              "name": "<actual-table-name>",
              "user_id": "<user-id-column-name>",
              "ignore_unsupported_types": false
            }
          }
        }
      }
  """

  alias Cloak.DataSource.SqlBuilder


  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def connect(parameters), do: {:ok, Enum.to_list(parameters)}
  @doc false
  def disconnect(_connection), do: :ok

  @doc false
  def describe_table(connection, table_name) do
    load_column_definitions(connection, table_name)
  end

  @doc false
  def select(connection, sql_query, result_processor) do
    run_query(connection, sql_query, result_processor)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp load_column_definitions(connection, table_name) do
    response = %{"success" => true} = post!(connection, "show_columns", %{table: table_name})
    Enum.map(response["columns"], &({&1["name"], parse_type(String.downcase(&1["type"]))}))
  end

  defp run_query(connection, query, result_processor) do
    case post!(connection, "query", request(query)) do
      %{"success" => true} = response ->
        rows =
          case response["rows"] do
            [[]] ->
              # dsproxy returns a `[[]]` when there are no results, so we'll normalize it to
              # an empty list
              []

            [_|_] = some_rows ->
              some_rows
          end
        {:ok, result_processor.(rows)}

      %{"success" => false, "error" => error_message} ->
        {:error, error_message}
    end
  end

  defp request(query) do
    maybe_include_columns(%{statement: sql_statement(query)}, query)
  end

  defp maybe_include_columns(request, %{mode: :unparsed}), do: request
  defp maybe_include_columns(request, query) do
    Map.put(request, :columns, needed_columns(query))
  end

  defp needed_columns(query) do
    Enum.map(query.db_columns, &SqlBuilder.column_name(&1, :ansi))
  end

  defp sql_statement(sql_query) do
    %{
      type: query_type(sql_query),
      params: [],
      val: SqlBuilder.build(sql_query, :ansi)
    }
  end

  defp query_type(%{mode: :unparsed}), do: "unsafe"
  defp query_type(_query), do: "parsed"

  defp post!(connection, operation, payload) do
    %HTTPoison.Response{status_code: 200, body: body} = HTTPoison.post!(
      "#{Keyword.fetch!(connection, :url)}/#{operation}",
      Poison.encode!(payload),
      [{"Content-Type", "application/json"}],
      recv_timeout: :timer.hours(4)
    )

    Poison.decode!(body)
  end

  defp parse_type("uniqueidentifier"), do: :uuid
  defp parse_type("nvarchar"), do: :text
  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("character"), do: :text
  defp parse_type("character varying"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("ntext"), do: :text
  defp parse_type("bool"), do: :boolean
  defp parse_type("bit"), do: :boolean
  defp parse_type("integer"), do: :integer
  defp parse_type("int"), do: :integer
  defp parse_type("int2"), do: :integer
  defp parse_type("int4"), do: :integer
  defp parse_type("int8"), do: :integer
  defp parse_type("tinyint"), do: :integer
  defp parse_type("smallint"), do: :integer
  defp parse_type("bigint"), do: :integer
  defp parse_type("real"), do: :real
  defp parse_type("float"), do: :real
  defp parse_type("float4"), do: :real
  defp parse_type("float8"), do: :real
  defp parse_type("decimal"), do: :real
  defp parse_type("money"), do: :real
  defp parse_type("smallmoney"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("double"), do: :real
  defp parse_type("timestamp"), do: :datetime
  defp parse_type("timestamptz"), do: :datetime
  defp parse_type("time"), do: :time
  defp parse_type("timetz"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("datetime"), do: :datetime
  defp parse_type("datetime2"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
