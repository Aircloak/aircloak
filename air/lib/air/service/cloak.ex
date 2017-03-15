defmodule Air.Service.Cloak do
  @moduledoc """
  The Cloak service holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  use GenServer

  alias Air.Service.DataSource

  @serializer_name __MODULE__.Serializer
  @main_registry_name __MODULE__.MainRegistry
  @memory_registry_name __MODULE__.MemoryRegistry
  @all_cloak_registry_name __MODULE__.AllCloakRegistry


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the supervisor specification for this service."
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    import Supervisor.Spec

    children = [
      worker(__MODULE__, []),
      worker(Registry, [:duplicate, @main_registry_name], id: @main_registry_name),
      worker(Registry, [:unique, @memory_registry_name], id: @memory_registry_name),
      worker(Registry, [:duplicate, @all_cloak_registry_name], id: @all_cloak_registry_name),
    ]

    supervisor(Supervisor, [children, [strategy: :one_for_one, name: __MODULE__]])
  end

  @doc "Starts the data source serializer process."
  @spec start_link() :: GenServer.on_start
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: @serializer_name)

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register(Map.t, Map.t) :: :ok
  def register(cloak_info, data_sources) do
    {data_source_ids, cloak_info} = GenServer.call(@serializer_name, {:register, cloak_info, data_sources})

    Registry.register(@all_cloak_registry_name, :all_cloaks, cloak_info)
    for global_id <- data_source_ids do
      Registry.register(@main_registry_name, global_id, cloak_info)
    end

    :ok
  end

  @doc "Records cloak memory readings"
  @spec record_memory(Map.t) :: :ok
  def record_memory(reading) do
    Registry.register(@memory_registry_name, self(), reading)
    :ok
  end

  @doc "Returns a list of cloak channels for a given data source. The list consists of pairs `{pid, cloak_info}`."
  @spec channel_pids(String.t) :: [{pid(), Map.t}]
  def channel_pids(global_id), do: Registry.lookup(@main_registry_name, global_id)

  @doc """
  Returns a list of the connected cloaks. The element returned for each cloak
  corresponds to the cloak info that was used to register the cloak, but is
  additionally augmented with a list of the IDs of the data sources served by the cloak
  """
  @spec all_cloak_infos() :: [Map.t]
  def all_cloak_infos(), do:
    for {pid, info} <- Registry.lookup(@all_cloak_registry_name, :all_cloaks), do: lookup_memory(pid, info)

  @doc "Returns the cloak info of cloaks serving a data source"
  @spec cloak_infos_for_data_source(String.t) :: [Map.t]
  def cloak_infos_for_data_source(global_id), do:
    for {pid, cloak_info} <- Registry.lookup(@main_registry_name, global_id), do: lookup_memory(pid, cloak_info)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_), do: {:ok, nil}

  def handle_call({:register, cloak_info, data_sources}, _from, state) do
    data_source_ids = register_data_sources(data_sources)
    cloak_info = Map.merge(cloak_info, %{
      data_source_ids: data_source_ids,
      memory: %{},
    })

    {:reply, {data_source_ids, cloak_info}, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp register_data_sources(data_sources) do
    for data_source <- data_sources do
      global_id = Map.fetch!(data_source, "global_id")
      tables = Map.fetch!(data_source, "tables")
      errors = Map.get(data_source, "errors", [])

      DataSource.create_or_update_data_source(global_id, tables, errors)

      global_id
    end
  end

  defp lookup_memory(pid, cloak_info) do
    case Registry.lookup(@memory_registry_name, pid) do
      [{_, memory}] -> cloak_info |> Map.put(:memory, memory)
      _ -> cloak_info |> Map.put(:memory, %{})
    end
  end
end
