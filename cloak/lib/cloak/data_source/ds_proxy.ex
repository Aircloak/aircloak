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

  import Supervisor.Spec
  alias Cloak.SqlQuery.Builder


  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @pool_name DBConnection.Poolboy

  @doc false
  def child_spec(source_id, parameters) do
    worker(__MODULE__, [source_id, parameters], id: proc_name(source_id), restart: :permanent)
  end

  @doc false
  def start_link(source_id, parameters) do
    Agent.start_link(fn() -> parameters end, name: proc_name(source_id))
  end

  @doc false
  def get_columns(source_id, full_table_name) do
    load_column_definitions(params(source_id), full_table_name)
  end

  @doc false
  def select(source_id, sql_query) do
    run_query(params(source_id), sql_query)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp params(source_id) do
    Agent.get(proc_name(source_id), &(&1))
  end

  defp load_column_definitions(params, full_table_name) do
    response = %{"success" => true} = post!(params, "show_columns", %{table: full_table_name})
    Enum.map(response["columns"], &({&1["name"], parse_type(String.downcase(&1["type"]))}))
  end

  defp run_query(params, query) do
    case post!(params, "query", request(query)) do
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
        {:ok, {length(response["rows"]), response["columns"], rows}}

      %{"success" => false, "error" => error_message} ->
        {:error, error_message}
    end
  end

  defp request(query) do
    %{statement: sql_statement(query)}
    |> maybe_include_columns(query)
    |> maybe_include_where_clause(query)
  end

  defp maybe_include_columns(request, %{from: {:subquery, _}}), do: request
  defp maybe_include_columns(request, query) do
    Map.put(request, :columns, needed_columns(query))
  end

  defp maybe_include_where_clause(request, %{from: {:subquery, _}, where: where, where_not: where_not}) do
    request
    |> Map.put(:where, Enum.map(where, &encode_where/1))
    |> Map.put(:where_not, Enum.map(where_not, &encode_where/1))
  end
  defp maybe_include_where_clause(request, _) do
    request
  end

  defp encode_where({:comparison, lhs, comparator, rhs}) do
    %{
      type: :comparison,
      lhs: lhs,
      comparator: comparator,
      rhs: encode_rhs(rhs)
    }
  end
  defp encode_where({:ilike, lhs, rhs}) do
    %{
      type: :ilike,
      lhs: lhs,
      rhs: encode_rhs(rhs)
    }
  end
  defp encode_where({:like, lhs, rhs}) do
    %{
      type: :like,
      lhs: lhs,
      rhs: encode_rhs(rhs)
    }
  end
  defp encode_where({:not, {:ilike, lhs, rhs}}) do
    %{
      type: :not_ilike,
      lhs: lhs,
      rhs: encode_rhs(rhs)
    }
  end
  defp encode_where({:not, {:like, lhs, rhs}}) do
    %{
      type: :not_like,
      lhs: lhs,
      rhs: encode_rhs(rhs)
    }
  end
  defp encode_where({:not, {:comparison, lhs, :=, rhs}}) do
    %{
      type: :not_comparison,
      lhs: lhs,
      comparator: :=,
      rhs: encode_rhs(rhs)
    }
  end
  defp encode_where({:in, lhs, rhss}) do
    %{
      type: :in,
      lhs: lhs,
      rhss: Enum.map(rhss, &encode_rhs/1)
    }
  end
  defp encode_where({:is, lhs, :null}) do
    %{
      type: :is_null,
      lhs: lhs,
    }
  end
  defp encode_where({:not, {:is, lhs, :null}}) do
    %{
      type: :is_not_null,
      lhs: lhs,
    }
  end

  defp encode_rhs(%{value: value}) do
    value
  end

  defp needed_columns(query) do
    Cloak.SqlQuery.db_columns(query) -- [Cloak.SqlQuery.user_id_column(query)]
  end

  defp sql_statement(%{from: {:subquery, unsafe_select}}) do
    %{
      type: "unsafe",
      val: unsafe_select
    }
  end
  defp sql_statement(sql_query) do
    {query_string, params} = Builder.build(sql_query)
    %{
      type: "parsed",
      params: params,
      val: query_string |> List.flatten |> Enum.join
    }
  end

  defp post!(params, operation, payload) do
    %HTTPoison.Response{status_code: 200, body: body} = HTTPoison.post!(
      "#{Keyword.fetch!(params, :url)}/#{operation}",
      Poison.encode!(payload),
      [{"Content-Type", "application/json"}]
    )

    Poison.decode!(body)
  end

  defp proc_name(source_id), do: {:via, :gproc, {:n, :l, {Cloak.DataSource, source_id}}}

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
  defp parse_type("timestamp"), do: :timestamp
  defp parse_type("timestamptz"), do: :timestamp
  defp parse_type("time"), do: :time
  defp parse_type("timetz"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("datetime"), do: :datetime
  defp parse_type("datetime2"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
