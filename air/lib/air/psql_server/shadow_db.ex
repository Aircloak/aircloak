defmodule Air.PsqlServer.ShadowDb do
  @moduledoc """
  Service for managing shadow databases.

  A shadow database is a local PostgreSQL database which corresponds in structure to data source. We maintain these
  databases so we can offload PostgreSQL specific queries issued over the psql interface.
  """

  use Supervisor
  require Logger
  require Aircloak.DeployConfig
  alias Aircloak.ChildSpec
  alias Air.PsqlServer.ShadowDb.{Connection, ConnectionPool, Database, Manager}
  alias Air.Schemas.User
  import Aircloak, only: [in_env: 1]

  @database_supervisor __MODULE__.Databases
  @registry __MODULE__.Registry

  @type connection_params :: %{
          host: String.t(),
          port: non_neg_integer,
          ssl: boolean,
          user: String.t(),
          password: String.t(),
          name: String.t()
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns connection parameters to the shadow database server."
  @spec connection_params() :: connection_params
  def connection_params() do
    default_connection_params()
    |> Map.merge(Aircloak.DeployConfig.get("shadow_database", %{}))
    |> Map.take(~w(host port ssl user password name))
    |> Aircloak.atomize_keys()
  end

  @doc "Executes the query on the given data source."
  @spec query(User.t(), String.t(), String.t(), [term]) :: Connection.query_result()
  def query(user, data_source_name, query, params) do
    ensure_database!(user, data_source_name)
    Manager.wait_until_initialized(user, data_source_name)
    ConnectionPool.query(user, data_source_name, query, params)
  end

  @doc "Parses the query on the given data source."
  @spec parse(User.t(), String.t(), String.t()) :: Connection.parse_result()
  def parse(user, data_source_name, query) do
    ensure_database!(user, data_source_name)
    Manager.wait_until_initialized(user, data_source_name)
    ConnectionPool.parse(user, data_source_name, query)
  end

  @doc "Updates the shadow database for the given data source."
  @spec update(User.t(), String.t()) :: :ok
  def update(user, data_source_name) do
    ensure_database!(user, data_source_name)
    Manager.update_definition(user, data_source_name)
  end

  @doc "Drops the given shadow database."
  @spec drop(User.t(), String.t()) :: :ok
  def drop(user, data_source_name) do
    with pid when is_pid(pid) <- Database.whereis(user, data_source_name),
         do: DynamicSupervisor.terminate_child(@database_supervisor, pid)

    # Manager.drop_database creates a connection and closes it, and closing a connection requires the client process to
    # trap exits. Since we don't want to implicitly start trapping exit in the caller of drop/1 we're doing this in a
    # separate task.
    Task.start_link(fn ->
      Process.flag(:trap_exit, true)
      Manager.drop_database(user, data_source_name)
    end)

    :ok
  end

  @doc "Returns the name of the shadow database for the given data source."
  @spec db_name(User.t(), String.t()) :: String.t()
  defdelegate db_name(user, data_source), to: Manager

  @doc "Returns the registered name for the process related to the given data source in a given role."
  @spec registered_name(User.t(), String.t(), term()) :: {:via, module, {atom, term}}
  def registered_name(user, data_source_name, role),
    do: {:via, Registry, {@registry, {user.id, data_source_name, role}}}

  # -------------------------------------------------------------------
  # Supervisor callbacks
  # -------------------------------------------------------------------

  @impl Supervisor
  def init(_arg) do
    wait_local_postgresql()

    Supervisor.init(
      [
        ChildSpec.registry(:unique, @registry),
        ChildSpec.dynamic_supervisor(name: @database_supervisor),
        in_env(test: nil, else: Air.PsqlServer.ShadowDb.SchemaSynchronizer)
      ]
      |> Enum.reject(&is_nil/1),
      strategy: :one_for_one
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp default_connection_params() do
    %{
      "host" => "127.0.0.1",
      "port" => 5432,
      "ssl" => false,
      "user" => "postgres",
      "password" => "",
      "name" => "postgres"
    }
  end

  defp ensure_database!(user, data_source_name) do
    with nil <- Database.whereis(user, data_source_name) do
      case DynamicSupervisor.start_child(@database_supervisor, {Database, {user, data_source_name}}) do
        {:ok, _pid} -> :ok
        {:error, {:already_started, _pid}} -> :ok
      end
    end
  end

  defp wait_local_postgresql() do
    Logger.info("waiting for local PostgreSQL instance")

    task =
      Task.async(fn ->
        Process.flag(:trap_exit, true)

        Stream.repeatedly(&Manager.db_server_available?/0)
        |> Stream.intersperse(:sleep)
        |> Stream.map(fn
          :sleep -> Process.sleep(:timer.seconds(5))
          el -> el
        end)
        |> Stream.drop_while(&(&1 != true))
        |> Enum.take(1)
      end)

    case Task.yield(task, :timer.minutes(1)) do
      nil -> raise "local PostgreSQL is not available"
      _ -> :ok
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: Supervisor.start_link(__MODULE__, nil, name: __MODULE__)
end
