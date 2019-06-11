defmodule Air.PsqlServer do
  @moduledoc """
  Server for PostgreSQL protocol which allows PostgreSQL clients to query cloaks.

  ## Modules

  The PostgreSQL functionality is broken into multiple different modules inside the `Air.PsqlServer` namespace:

    - `Air.PsqlServer.RanchServer` implements a generic PostgreSQL SSL server which is agnostic of Aircloak specifics.
        This module can be thought of as a behaviour, while `Air.PsqlServer` is a concrete callback module which
        provides Aircloak specific details.

    - `Air.PsqlServer.Protocol` is the module which contains the generic implementation of the PostgreSQL protocol.

    - `Air.PsqlServer.QueryExecution` contains the implementation of the query execution in the context of the Aircloak
       system.

    - `Air.PsqlServer.ShadowDb` module implements the shadow db functionality. A shadow db is a local database which
       corresponds in structure to a cloak datasource. There is one shadow db per each known datasource. These databases
       is used by `Air.PsqlServer.QueryExecution` to execute meta queries (statements which are querying the database
       structure).

    - `Air.PsqlServer.ConnectionRegistry` is responsible for tracking connection processes, mostly for the purpose of
       query cancellation.


  ## Process structure

  The functionality is supported by various processes. All the processes reside under the `Air.PsqlServer` supervisor.
  At the high-level there are three different services: the shadow db (powered by the Air.PsqlServer.ShadowDb), the
  connection registry (powered by `Air.PsqlServer.ConnectionRegistry`), and the TCP server (powered by
  `Air.PsqlServer.RanchServer`).
  """

  alias Air.PsqlServer.{RanchServer, ConnectionRegistry, QueryExecution}
  alias Air.Service.{User, DataSource}
  require Logger
  require Aircloak.DeployConfig

  @behaviour RanchServer

  @type configuration :: %{
          require_ssl: boolean,
          certfile: String.t() | nil,
          keyfile: String.t() | nil,
          max_connections: pos_integer
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the postgresql server configuration."
  @spec configuration() :: configuration
  def configuration() do
    default_config = %{require_ssl: true, certfile: nil, keyfile: nil, max_connections: 1024}

    user_config =
      case Aircloak.DeployConfig.fetch("psql_server") do
        {:ok, configuration} ->
          configuration

        :error ->
          %{}
      end

    normalized_user_config =
      user_config
      |> Map.take(["require_ssl", "certfile", "keyfile", "max_connections"])
      |> Enum.map(fn {key, value} -> {String.to_atom(key), value} end)
      |> Enum.into(%{})

    Map.merge(default_config, normalized_user_config)
  end

  @doc "Verifies if SSL configuration is valid."
  @spec validate_ssl_config() :: :ok | {:error, String.t()}
  def validate_ssl_config() do
    [:certfile, :keyfile]
    |> Enum.map(&verify_ssl_file/1)
    |> Enum.filter(&match?({:error, _}, &1))
    |> Enum.map(fn {:error, error} -> error end)
    |> case do
      [] ->
        :ok

      errors ->
        {:error,
         to_string([
           "the system can't accept SSL connections over the PostgreSQL protocol for the following reasons: ",
           Enum.join(errors, ", ")
         ])}
    end
  end

  # -------------------------------------------------------------------
  # Air.PsqlServer.RanchServer callback functions
  # -------------------------------------------------------------------

  @impl RanchServer
  def init(conn, nil), do: {:ok, conn}

  @impl RanchServer
  def login(conn, password) do
    with data_source_name = conn.login_params["database"],
         data_source_id <- {:name, data_source_name},
         {:ok, user} <- User.login_psql(conn.login_params["user"], password),
         {:ok, _} <- DataSource.fetch_as_user(data_source_id, user) do
      # We're not storing data source, since access permissions have to be checked on every query.
      # Otherwise, revoking permissions on a data source would have no effects on currently connected
      # cloak.
      # However, we're also checking access permissions now, so we can report error immediately.
      {:ok,
       conn
       |> RanchServer.assign(:user, user)
       |> RanchServer.assign(:data_source_id, data_source_id)
       |> RanchServer.assign(:data_source_name, data_source_name)}
    else
      _ -> :error
    end
  end

  @impl RanchServer
  def run_query(conn, query, params, _max_rows), do: QueryExecution.run_query(conn, query, params)

  @impl RanchServer
  def cancel_query(conn, key_data) do
    ConnectionRegistry.cancel_query(key_data)
    conn
  end

  @impl RanchServer
  def describe_statement(conn, query, params), do: QueryExecution.describe_query(conn, query, params)

  @impl RanchServer
  def handle_message(conn, _message), do: conn

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp verify_ssl_file(key) do
    cond do
      Map.fetch!(configuration(), key) == nil ->
        {:error, "missing `#{key}` setting under the `psql_server` key in the `config.json` file"}

      not File.exists?(Path.join([Application.app_dir(:air, "priv"), "config", Map.fetch!(configuration(), key)])) ->
        {:error, "the file `#{Map.fetch!(configuration(), key)}` is missing"}

      true ->
        :ok
    end
  end

  defp ranch_opts(),
    do: Keyword.merge(ssl_settings(), max_connections: configuration().max_connections, num_acceptors: 10, backlog: 100)

  defp ssl_settings() do
    case validate_ssl_config() do
      :ok ->
        [
          ssl: [
            certfile: Path.join([Application.app_dir(:air, "priv"), "config", configuration().certfile]),
            keyfile: Path.join([Application.app_dir(:air, "priv"), "config", configuration().keyfile])
          ]
        ]

      {:error, error} ->
        Logger.warn(error)
        []
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    Aircloak.ChildSpec.supervisor(
      [
        Air.PsqlServer.ShadowDb,
        Air.PsqlServer.ConnectionRegistry,
        tcp_interface()
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  defp tcp_interface() do
    Supervisor.child_spec(
      {RanchServer,
       {
         Application.fetch_env!(:air, Air.PsqlServer)[:port],
         __MODULE__,
         nil,
         ranch_opts()
       }},
      id: __MODULE__
    )
  end
end
