defmodule Cloak.DataSource.Acl do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Paul's Anonymizing Query Language backend.
  For more information, see `DataSource`.

  To use this data-source you need to add it to the list of data sources that should
  be served by your cloak and declare where the ACL backend can be found.

  A sample configuration could look like this:

      config :cloak, data_sources: [
        acl: [
          driver: Cloak.DataSource.Acl,
          parameters: [
            hostname: "localhost",
            port: 8000
          ],
          tables: [
            <table-display-name>: [
              name: "<actual-table-name>",
              user_id: "<user-id-column-name>",
              ignore_unsupported_types: false
            ]
          ]
        ]
      }
  """

  import Supervisor.Spec

  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @pool_name DBConnection.Poolboy

  @doc false
  def child_spec(source_id, parameters) do
    worker(__MODULE__, [[source_id | parameters]], id: proc_name(source_id), restart: :permanent)
  end

  @doc false
  def start_link([source_id | parameters]) do
    Agent.start_link(fn() -> parameters end, name: proc_name(source_id))
  end

  @doc false
  def get_columns(source_id, full_table_name) do
    with {:ok, params} <- Agent.get(proc_name(source_id), &({:ok, &1})) do
      load_column_definitions(params, full_table_name)
    end
  end

  @doc false
  def select(source_id, sql_query) do
    with {:ok, params} <- Agent.get(proc_name(source_id), &({:ok, &1})) do
      run_query(params, sql_query)
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp load_column_definitions(params, full_table_name) do
    request = %{
      type: "show:columns",
      table: full_table_name,
    }
    with {:ok, response} <- tcp_send_receive(params, request) do
      case response["success"] do
        true ->
          Enum.map(response["columns"], &({&1["name"], parse_type(&1["type"])}))
        false ->
          raise "Couldn't determine columns for table #{full_table_name}"
      end
    end
  end

  defp run_query(params, %{columns: columns} = query) do
    request = %{
      type: "query",
      columns: columns |> Enum.map(&Cloak.SqlQuery.Builder.select_column_to_string/1),
      statement: sql_statement(query)
    }
    with {:ok, response} <- tcp_send_receive(params, request) do
      case response["success"] do
        true ->
          {:ok, {length(response["rows"]), response["columns"], response["rows"]}}
        false ->
          {:error, response["error"]}
      end
    end
  end

  defp sql_statement(%{from: {:verbatim, unsafe_select}}) do
    %{
      type: "unsafe",
      val: unsafe_select
    }
  end
  defp sql_statement(sql_query) do
    {query_string, params} = Cloak.SqlQuery.Builder.build(sql_query)
    %{
      type: "parsed",
      params: params,
      val: query_string |> List.flatten |> Enum.join
    }
  end

  defp tcp_send_receive(params, message) do
    case :gen_tcp.connect(String.to_char_list(params[:hostname]), params[:port],
        [:binary, {:packet, 0}, {:active, true}]) do
      {:ok, socket} ->
        :ok = :gen_tcp.send(socket, Poison.encode!(message))
        :ok = :gen_tcp.shutdown(socket, :write)
        response = case receive_data([]) do
          {:ok, data} ->
            Poison.decode(data)
          {:error, _error} ->
            {:error, "Communication with ACL backend failed"}
        end
        :gen_tcp.close(socket)
        response
      {:error, reason} ->
        {:error, "Unable to connect to ACL backend: #{inspect(reason)}"}
    end
  end

  defp receive_data(acc) do
    receive do
      {:tcp, _socket, packet} ->
        receive_data([packet | acc])
      {:tcp_closed, _socket} ->
        data = acc
        |> Enum.reverse()
        |> Enum.join()
        {:ok, data}
      _other ->
        {:error, "Unexpected response from ACL backend"}
    end
  end

  defp proc_name(source_id), do: {:via, :gproc, {:n, :l, {Cloak.DataSource, source_id}}}

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bool"), do: :boolean
  defp parse_type("integer"), do: :integer
  defp parse_type("int2"), do: :integer
  defp parse_type("int4"), do: :integer
  defp parse_type("int8"), do: :integer
  defp parse_type("float"), do: :real
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
end
