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
    case DynamicSupervisor.start_child(
           @server_supervisor,
           {Server, {data_source.name, server_name(data_source.name)}}
         ) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  defp server_name(data_source_name), do: {:via, Registry, {@server_registry, data_source_name}}

  defp wait_local_postgresql() do
    Logger.info("waiting for local PostgreSQL instance")

    task =
      Task.async(fn ->
        Process.flag(:trap_exit, true)

        Stream.repeatedly(&postgrex_availabe?/0)
        |> Stream.drop_while(&(&1 == false))
        |> Enum.take(1)
      end)

    case Task.yield(task, :timer.minutes(1)) do
      nil -> raise "local PostgreSQL is not available"
      _ -> :ok
    end
  end

  defp postgrex_availabe?() do
    Task.async(fn ->
      Postgrex.start_link(
        hostname: "127.0.0.1",
        username: "postgres",
        database: "postgres",
        sync_connect: true,
        backoff_type: :stop
      )
    end)
    |> Task.yield()
    |> case do
      {:ok, {:ok, _pid}} ->
        true

      _ ->
        Process.sleep(:timer.seconds(5))
        false
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: Supervisor.start_link(__MODULE__, nil, name: __MODULE__)
end
