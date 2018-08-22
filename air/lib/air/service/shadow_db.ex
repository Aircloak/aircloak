defmodule Air.Service.ShadowDb do
  @moduledoc """
  Service for managing shadow databases.

  A shadow database is a local PostgreSQL database which corresponds in structure to data source. We maintain these
  databases so we can offload PostgreSQL specific queries issued over the psql interface.
  """

  use Supervisor
  require Logger
  alias Aircloak.ChildSpec
  alias Air.Service.ShadowDb.Server

  @server_supervisor __MODULE__.Servers
  @server_registry __MODULE__.Registry

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Updates the shadow database according to the data source definition."
  @spec update(map) :: :ok
  def update(data_source), do: Server.update_definition(server_pid(data_source))

  @doc "Drops the given shadow database."
  @spec drop(String.t()) :: :ok
  def drop(data_source_name) do
    with pid when is_pid(pid) <- Server.whereis(data_source_name),
         do: DynamicSupervisor.terminate_child(@server_supervisor, pid)

    # Server.drop_database creates a connection and closes it, and closing a connection requires the client process to
    # trap exits. Since we don't want to implicitly start trapping exit in the caller of drop/1 we're doing this in a
    # separate task.
    Task.start_link(fn ->
      Process.flag(:trap_exit, true)
      Server.drop_database(data_source_name)
    end)

    :ok
  end

  @doc "Returns the name of the shadow database for the given data source."
  @spec db_name(String.t()) :: String.t()
  defdelegate db_name(data_source), to: Server

  @doc "Returns the registered name for the process related to the given data source in a given role."
  @spec registered_name(String.t(), term()) :: {:via, module, {atom, term}}
  def registered_name(data_source_name, role), do: {:via, Registry, {@server_registry, {data_source_name, role}}}

  # -------------------------------------------------------------------
  # Supervisor callbacks
  # -------------------------------------------------------------------

  @impl Supervisor
  def init(_arg) do
    if Application.get_env(:air, :shadow_db?, true), do: wait_local_postgresql()

    Supervisor.init(
      [
        ChildSpec.registry(:unique, @server_registry),
        ChildSpec.dynamic_supervisor(name: @server_supervisor)
      ],
      strategy: :one_for_one
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp server_pid(data_source) do
    with nil <- Server.whereis(data_source.name) do
      case DynamicSupervisor.start_child(@server_supervisor, {Server, data_source.name}) do
        {:ok, pid} -> pid
        {:error, {:already_started, pid}} -> pid
      end
    end
  end

  defp wait_local_postgresql() do
    Logger.info("waiting for local PostgreSQL instance")

    task =
      Task.async(fn ->
        Process.flag(:trap_exit, true)

        Stream.repeatedly(&Server.db_server_available?/0)
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
