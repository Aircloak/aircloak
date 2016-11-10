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
  def run_query(conn, query, _params, _max_rows) do
    RanchServer.assign(
      conn,
      :query_runner,
      Task.async(fn -> DataSource.run_query(conn.assigns.data_source_id, conn.assigns.user, query) end)
    )
  end

  @doc false
  def describe_statement(_conn, _query, _params), do:
    raise "Prepared statements are not supported!"

  @doc false
  def handle_message(%{assigns: %{query_runner: %Task{ref: ref}}} = conn, {ref, query_result}), do:
    RanchServer.set_query_result(conn, parse_response(query_result))
  def handle_message(conn, _message), do:
    conn


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

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
        |> Map.fetch!("rows")
        |> Enum.flat_map(&List.duplicate(Map.fetch!(&1, "row"), Map.fetch!(&1, "occurrences")))
    }
  defp parse_response(other) do
    Logger.error("Error running a query: #{inspect other}")
    %{error: "System error!"}
  end

  for {aql_type, psql_type} <- %{
    "integer" => :int8,
    "text" => :text
  } do
    defp type_atom(unquote(aql_type)), do: unquote(psql_type)
  end
  defp type_atom(_other), do: :unknown
end
