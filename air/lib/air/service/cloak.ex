defmodule Air.Service.Cloak do
  @moduledoc """
  The Cloak service holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  use GenServer

  alias Air.Service.DataSource

  @serializer_name __MODULE__.Serializer
  @data_source_registry_name __MODULE__.DataSourceRegistry
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
      worker(GenServer, [__MODULE__, [], [name: @serializer_name]], id: @serializer_name),
      worker(Registry, [:duplicate, @data_source_registry_name], id: @data_source_registry_name),
      worker(Registry, [:unique, @memory_registry_name], id: @memory_registry_name),
      worker(Registry, [:duplicate, @all_cloak_registry_name], id: @all_cloak_registry_name),
    ]

    supervisor(Supervisor, [children, [strategy: :one_for_one, name: __MODULE__]])
  end

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register(Map.t, Map.t) :: :ok
  def register(cloak_info, data_sources) do
    {data_source_names, cloak_info} = GenServer.call(@serializer_name, {:register, cloak_info, data_sources})

    Registry.register(@all_cloak_registry_name, :all_cloaks, cloak_info)
    for data_source_name <- data_source_names do
      Registry.register(@data_source_registry_name, data_source_name, cloak_info)
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
  def channel_pids(name), do: Registry.lookup(@data_source_registry_name, name)

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
  def cloak_infos_for_data_source(name), do:
    for {pid, cloak_info} <- Registry.lookup(@data_source_registry_name, name), do: lookup_memory(pid, cloak_info)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_), do: {:ok, nil}

  def handle_call({:register, cloak_info, data_sources}, _from, state) do
    data_sources_by_name = data_sources
    |> Enum.map(&({Map.fetch!(&1, "name"), &1}))
    |> Enum.into(%{})

    data_sources
    |> add_error_on_conflicting_data_source_definitions()
    |> combined_data_source_errors(cloak_info)
    |> register_data_sources()

    cloak_info = Map.merge(cloak_info, %{
      data_sources: data_sources_by_name,
      memory: %{},
    })

    {:reply, {Map.keys(data_sources_by_name), cloak_info}, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp add_error_on_conflicting_data_source_definitions(data_sources) do
    for data_source <- data_sources do
      name = Map.fetch!(data_source, "name")
      tables = Map.fetch!(data_source, "tables")

      existing_definitions_for_data_source_by_cloak(name)
      |> Enum.map(fn({_cloak_name, data_source}) -> data_source end)
      |> Enum.all?(&(tables == Map.fetch!(&1, "tables")))
      |> unless do
        existing_errors = Map.get(data_source, "errors", [])
        error = "The data source definition for data source `#{name}` differs between the different cloaks. " <>
          "Please ensure the configurations for the data source are identical, across all the cloaks configured " <>
          "to serve the dataset."
        Map.put(data_source, "errors", [error | existing_errors])
      else
        data_source
      end
    end
  end

  defp combined_data_source_errors(data_sources, cloak_info) do
    for data_source <- data_sources do
      data_source_name = Map.fetch!(data_source, "name")

      combined_errors = [{cloak_info.name, data_source}]
      |> Enum.concat(existing_definitions_for_data_source_by_cloak(data_source_name))
      |> Enum.flat_map(fn({cloak_name, data_source}) ->
        Enum.map(Map.get(data_source, "errors", []), &("On cloak #{cloak_name}: #{&1}"))
      end)
      |> Enum.uniq()

      Map.put(data_source, "errors", combined_errors)
    end
  end

  defp existing_definitions_for_data_source_by_cloak(name), do:
    all_cloak_infos()
    |> Enum.map(&({&1.name, Map.get(&1.data_sources, name)}))
    |> Enum.reject(&is_nil(&1))

  defp register_data_sources(data_sources), do:
    Enum.each(data_sources, fn(data_source) ->
      name = Map.fetch!(data_source, "name")
      # Deprecated: global_id is a remnant of Aircloak pre-version 17.3.0.
      # It has to remain for compatibility with older versions
      # (hidden from the sight of users) until version 18.1.0.
      global_id = Map.fetch!(data_source, "global_id")
      tables = Map.fetch!(data_source, "tables")
      errors = Map.get(data_source, "errors", [])

      DataSource.create_or_update_data_source(name, global_id, tables, errors)
    end)

  defp lookup_memory(pid, cloak_info) do
    case Registry.lookup(@memory_registry_name, pid) do
      [{_, memory}] -> cloak_info |> Map.put(:memory, memory)
      [] -> cloak_info |> Map.put(:memory, %{})
    end
  end
end
