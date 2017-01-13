defmodule Air.PsqlServer do
  @moduledoc "Server for PostgreSQL protocol which allows PostgreSQL clients to query cloaks."

  alias Air.PsqlServer.RanchServer
  alias Air.Service.{User, DataSource}
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
      ssl: [
        certfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_cert.pem"]),
        keyfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_key.pem"])
      ]
    )


  #-----------------------------------------------------------------------------------------------------------
  # Air.PsqlServer.RanchServer callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def init(conn, nil), do:
    {:ok, conn}

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
      {true, conn} -> conn
      false ->
        RanchServer.assign(
          conn,
          :query_runner,
          Task.async(fn ->
            DataSource.run_query(conn.assigns.data_source_id, conn.assigns.user, query, params)
          end)
        )
    end
  end

  @doc false
  def describe_statement(conn, query, params), do:
    RanchServer.assign(
      conn,
      :query_describer,
      Task.async(fn ->
        DataSource.describe_query(conn.assigns.data_source_id, conn.assigns.user, query, params)
      end)
    )

  @doc false
  def handle_message(%{assigns: %{query_runner: %Task{ref: ref}}} = conn, {ref, query_result}), do:
    RanchServer.set_query_result(conn, parse_response(query_result))
  def handle_message(%{assigns: %{query_describer: %Task{ref: ref}}} = conn, {ref, query_result}), do:
    RanchServer.set_describe_result(
      conn,
      Map.take(parse_response(query_result), [:columns, :param_types, :error])
    )
  def handle_message(conn, _message), do:
    conn


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp handle_special_query(conn, "set " <> _), do:
    # we're ignoring set for now
    {true, RanchServer.set_query_result(conn, nil)}
  defp handle_special_query(conn, query) do
    if query =~ ~r/^select.+from pg_type/s do
      # select ... from pg_type ...
      {true, RanchServer.set_query_result(conn, special_query(query))}
    else
      false
    end
  end

  defp parse_response({:error, :not_connected}), do:
    %{error: "Data source is not available!"}
  defp parse_response({:ok, %{"error" => error}}), do:
    %{error: error}
  defp parse_response({:ok, query_result}), do:
    %{
      columns:
        Enum.zip(
          Map.fetch!(query_result, "columns"),
          query_result |> Map.fetch!("features") |> Map.fetch!("selected_types")
        )
        |> Enum.map(fn({name, type}) -> %{name: name, type: type_atom(type)} end),
      rows:
        query_result
        |> Map.get("rows", [])
        |> Enum.flat_map(&List.duplicate(Map.fetch!(&1, "row"), Map.fetch!(&1, "occurrences"))),
      param_types:
        query_result
        |> Map.fetch!("features")
        |> Map.fetch!("parameter_types")
        |> Enum.map(&type_atom/1)
    }
  defp parse_response(other) do
    Logger.error("Error running a query: #{inspect other}")
    %{error: "System error!"}
  end

  for {aql_type, psql_type} <- %{
    "boolean" => :boolean,
    "integer" => :int8,
    "real" => :float8,
    "text" => :text
  } do
    defp type_atom(unquote(aql_type)), do: unquote(psql_type)
  end
  defp type_atom(_other), do: :unknown


  #-----------------------------------------------------------------------------------------------------------
  # Handling of special queries
  #-----------------------------------------------------------------------------------------------------------

  # These queries are issued by clients to query `pg_type` and associated tables. Currently, we don't parse
  # queries, so we have to hardcode results for each client we want to support.

  # postgrex pg_type query
  defp special_query("select t.oid, t.typname, t.typsend, t.typreceive, t.typoutput, t.typinput,\n       t.typelem, 0, array (\n  select a.atttypid\n  from pg_attribute as a\n  where a.attrelid = t.typrelid and a.attnum > 0 and not a.attisdropped\n  order by a.attnum\n)\nfrom pg_type as t\n\n\n") do
    %{
      columns:
        ~w(oid typname typsend typreceive typoutput typinput typelem coalesce array)
        |> Enum.map(&%{name: &1, type: :text}),
      rows:
        [
          ~w(16 bool boolsend boolrecv boolout boolin 0 0 {}),
          ~w(20 int8 int8send int8recv int8out int8in 0 0 {}),
          ~w(23 int4 int4send int4recv int4out int4in 0 0 {}),
          ~w(25 text textsend textrecv textout textin 0 0 {}),
          ~w(700 float4 float4send float4recv float4out float4in 0 0 {}),
          ~w(701 float8 float8send float8recv float8out float8in 0 0 {}),
          ~w(705 unknown unknownsend unknownrecv unknownout unknownin 0 0 {}),
        ]
    }
  end
  defp special_query(_), do:
    %{columns: [], rows: []}
end
