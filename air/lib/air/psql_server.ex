defmodule Air.PsqlServer do
  @moduledoc "Server for PostgreSQL protocol which allows PostgreSQL clients to query cloaks."

  alias Air.PsqlServer.RanchServer
  alias Air.Service.{User, DataSource, Version}
  require Logger

  @behaviour RanchServer


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns the supervisor specification for the server."
  @spec child_spec() :: Supervisor.child_spec
  def child_spec(), do:
    RanchServer.child_spec(
      Application.fetch_env!(:air, Air.PsqlServer)[:port],
      __MODULE__,
      nil,
      ranch_opts()
    )


  #-----------------------------------------------------------------------------------------------------------
  # Air.PsqlServer.RanchServer callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def init(conn, nil), do:
    {:ok, RanchServer.assign(conn, :async_jobs, %{})}

  @doc false
  def login(conn, password) do
    with data_source_id <- {:global_id, conn.login_params["database"]},
         {:ok, user} <- User.login(conn.login_params["user"], password),
         {:ok, _} <- DataSource.fetch_as_user(data_source_id, user)
    do
      # We're not storing data source, since access permissions have to be checked on every query.
      # Otherwise, revoking permissions on a data source would have no effects on currently connected
      # cloak.
      # However, we're also checking access permissions now, so we can report error immediately.
      {:ok,
        conn
        |> RanchServer.assign(:user, user)
        |> RanchServer.assign(:data_source_id, data_source_id)
      }
    else
      _ -> :error
    end
  end

  @doc false
  def run_query(conn, query, params, _max_rows) do
    case handle_special_query(conn, String.downcase(query)) do
      {true, conn} ->
        conn
      false ->
        start_async_query(conn, query, params, &RanchServer.set_query_result(&1, parse_response(&2)))
    end
  end

  @doc false
  def describe_statement(conn, query, params) do
    user = conn.assigns.user
    data_source_id = conn.assigns.data_source_id
    converted_params = convert_params(params)
    run_async(
      conn,
      fn -> DataSource.describe_query(data_source_id, user, query, converted_params) end,
      fn(conn, describe_result) ->
        result =
        case parse_response(describe_result) do
          {:error, _} = error -> error
          parsed_response -> Keyword.take(parsed_response, [:columns, :param_types])
        end
        RanchServer.set_describe_result(conn, result)
      end
    )
  end

  @doc false
  def handle_message(conn, {ref, query_result}) do
    case Map.fetch(conn.assigns.async_jobs, ref) do
      :error ->
        conn
      {:ok, job_descriptor} ->
        async_jobs = Map.delete(conn.assigns.async_jobs, ref)
        conn = RanchServer.assign(conn, :async_jobs, async_jobs)
        job_descriptor.on_finished.(conn, query_result)
    end
  end
  def handle_message(conn, _message), do:
    conn


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_async(conn, job_fun, on_finished) do
    task = Task.async(job_fun)
    async_jobs = Map.put(conn.assigns.async_jobs, task.ref, %{task: task, on_finished: on_finished})
    RanchServer.assign(conn, :async_jobs, async_jobs)
  end

  defp start_async_query(conn, query, params, on_finished) do
    user = conn.assigns.user
    data_source_id = conn.assigns.data_source_id
    converted_params = convert_params(params)
    run_async(
      conn,
      fn -> DataSource.run_query(data_source_id, user, query, converted_params) end,
      on_finished
    )
  end

  defp ranch_opts(), do:
    Application.get_env(:air, Air.PsqlServer, [])
    |> Keyword.get(:ranch_opts, [])
    |> Keyword.merge(ssl: [
        certfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_cert.pem"]),
        keyfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_key.pem"])
      ])

  defp handle_special_query(conn, "set " <> _), do:
    # we're ignoring set for now
    {true, RanchServer.set_query_result(conn, [command: :set])}
  defp handle_special_query(conn, query) do
    cond do
      query =~ ~r/^select.+from pg_type/s ->
        {true, RanchServer.set_query_result(conn, special_query(query))}
      query =~ ~r/begin;declare.* for select relname, nspname, relkind from.*fetch.*/ ->
        # tables list for tableau
        {true,
          conn
          |> RanchServer.set_query_result([command: :begin, intermediate: true])
          |> RanchServer.set_query_result([command: :"declare cursor", intermediate: true])
          |> RanchServer.set_query_result(tables_list_for_tableau([]))
        }
      true ->
        false
    end
  end

  defp parse_response({:error, :not_connected}), do:
    {:error, "Data source is not available!"}
  defp parse_response({:error, :expired}), do:
    %{
      error: "Your Aircloak installation is running version #{Air.SharedView.version()} " <>
        "which expired on #{Version.expiry_date()}."
    }
  defp parse_response({:ok, %{"error" => error}}), do:
    {:error, error}
  defp parse_response({:ok, query_result}), do:
    [
      columns:
        Enum.zip(
          Map.fetch!(query_result, "columns"),
          query_result |> Map.fetch!("features") |> Map.fetch!("selected_types")
        )
        |> Enum.map(fn({name, sql_type}) -> %{name: name, type: psql_type(sql_type)} end),
      rows:
        query_result
        |> Map.get("rows", [])
        |> Enum.flat_map(&List.duplicate(Map.fetch!(&1, "row"), Map.fetch!(&1, "occurrences"))),
      param_types:
        query_result
        |> Map.fetch!("features")
        |> Map.fetch!("parameter_types")
        |> Enum.map(&psql_type/1)
    ]
  defp parse_response(other) do
    Logger.error("Error running a query: #{inspect other}")
    {:error, "System error!"}
  end

  defp convert_params(nil), do: nil
  defp convert_params(params), do:
    Enum.map(params, fn({type, value}) -> %{type: sql_type(type, value), value: value} end)


  #-----------------------------------------------------------------------------------------------------------
  # Type conversions
  #-----------------------------------------------------------------------------------------------------------

  for {psql_type, sql_type} <- %{
    boolean: :boolean,
    int2: :integer,
    int4: :integer,
    int8: :integer,
    float4: :real,
    float8: :real,
    numeric: :real,
    date: :date,
    time: :time,
    timestamp: :datetime,
    text: :text
  } do
    defp sql_type(unquote(psql_type), _value), do: unquote(sql_type)
  end
  defp sql_type(:unknown, value), do: sql_type_from_value(value)

  defp sql_type_from_value(value) when is_boolean(value), do: :boolean
  defp sql_type_from_value(value) when is_integer(value), do: :integer
  defp sql_type_from_value(value) when is_float(value), do: :real
  defp sql_type_from_value(value) when is_binary(value), do: :text
  defp sql_type_from_value(%Date{}), do: :date
  defp sql_type_from_value(%Time{}), do: :time
  defp sql_type_from_value(%NaiveDateTime{}), do: :datetime

  for {sql_type, psql_type} <- %{
    "boolean" => :boolean,
    "integer" => :int8,
    "real" => :float8,
    "text" => :text,
    "date" => :date,
    "time" => :time,
    "datetime" => :timestamp,
  } do
    defp psql_type(unquote(sql_type)), do: unquote(psql_type)
  end
  defp psql_type(_other), do: :unknown


  #-----------------------------------------------------------------------------------------------------------
  # Handling of special queries
  #-----------------------------------------------------------------------------------------------------------

  # These queries are issued by clients to query `pg_type` and associated tables. Currently, we don't parse
  # queries, so we have to hardcode results for each client we want to support.

  # postgrex pg_type query
  defp special_query("select t.oid, t.typname, t.typsend, t.typreceive, t.typoutput, t.typinput,\n       t.typelem, 0, array (\n  select a.atttypid\n  from pg_attribute as a\n  where a.attrelid = t.typrelid and a.attnum > 0 and not a.attisdropped\n  order by a.attnum\n)\nfrom pg_type as t\n\n\n") do
    [
      columns:
        ~w(oid typname typsend typreceive typoutput typinput typelem coalesce array)
        |> Enum.map(&%{name: &1, type: :text}),
      rows:
        [
          ~w(16 bool boolsend boolrecv boolout boolin 0 0 {}),
          ~w(21 int2 int2send int2recv int2out int2in 0 0 {}),
          ~w(23 int4 int4send int4recv int4out int4in 0 0 {}),
          ~w(20 int8 int8send int8recv int8out int8in 0 0 {}),
          ~w(25 text textsend textrecv textout textin 0 0 {}),
          ~w(700 float4 float4send float4recv float4out float4in 0 0 {}),
          ~w(701 float8 float8send float8recv float8out float8in 0 0 {}),
          ~w(705 unknown unknownsend unknownrecv unknownout unknownin 0 0 {}),
          ~w(1082 date date_send date_recv date_out date_in 0 0 {}),
          ~w(1083 time time_send time_recv time_out time_in 0 0 {}),
          ~w(1114 timestamp timestamp_send timestamp_recv timestamp_out timestamp_in 0 0 {}),
          ~w(1700 numeric numeric_send numeric_recv numeric_out numeric_in 0 0 {})
        ]
    ]
  end
  defp special_query(_), do:
    [columns: [], rows: []]

  defp tables_list_for_tableau(table_names), do:
    [
      command: :fetch,
      columns:
        [
          %{name: "relname", type: :name},
          %{name: "nspname", type: :name},
          %{name: "relkind", type: :char},
        ],
      rows: Enum.map(table_names, &[&1, "public", ?r])
    ]
end
