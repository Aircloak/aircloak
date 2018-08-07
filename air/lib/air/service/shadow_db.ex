defmodule Air.Service.ShadowDb do
  @moduledoc """
  Service for managing shadow databases.

  A shadow database is a local PostgreSQL database which corresponds in structure to data source. We maintain these
  databases so we can offload PostgreSQL specific queries issued over the psql interface.
  """

  alias Aircloak.ChildSpec
  alias Air.Service.ShadowDb.Server

  @server_supervisor __MODULE__.Servers
  @server_registry __MODULE__.Registry

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Updates the shadow database according to the data source definition."
  @spec update(map) :: :ok
  def update(data_source), do: Server.update_definition(server_pid(data_source), data_source)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp server_pid(data_source) do
    case DynamicSupervisor.start_child(@server_supervisor, {Server, server_name(data_source.name)}) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  defp server_name(data_source_name), do: {:via, Registry, {@server_registry, data_source_name}}

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        ChildSpec.registry(:unique, @server_registry),
        ChildSpec.dynamic_supervisor(name: @server_supervisor)
      ],
      strategy: :rest_for_one,
      name: __MODULE__
    )
  end
end
