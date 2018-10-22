defmodule Air.Service.Cloak do
  @moduledoc """
  The Cloak service holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  use GenServer

  alias Aircloak.ChildSpec
  alias Air.Service.DataSource

  @serializer_name __MODULE__.Serializer
  @data_source_registry_name __MODULE__.DataSourceRegistry
  @memory_registry_name __MODULE__.MemoryRegistry
  @all_cloak_registry_name __MODULE__.AllCloakRegistry
  @memory_readings_to_keep 250

  @type memory_reading :: %{
          total_memory: number,
          available_memory: %{
            current: number,
            last_5_seconds: number,
            last_1_minute: number,
            last_5_minutes: number,
            last_15_minutes: number,
            last_1_hour: number
          }
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Registers a data source (if needed), and associates the calling cloak with the data source."
  @spec register(Map.t(), Map.t()) :: [Air.Schemas.DataSource.t()]
  def register(cloak_info, data_sources) do
    {data_source_names, cloak_info, data_source_schemas} =
      GenServer.call(@serializer_name, {:register, cloak_info, data_sources})

    Enum.each(data_sources, &Air.PsqlServer.ShadowDb.update(&1.name))

    Registry.register(@all_cloak_registry_name, :all_cloaks, cloak_info)
    Registry.register(@memory_registry_name, self(), initial_memory_readings())

    for data_source_name <- data_source_names do
      Registry.register(@data_source_registry_name, data_source_name, cloak_info)
    end

    data_source_schemas
  end

  @doc "Updates the data sources configuration for the calling cloak."
  @spec update(Map.t(), Map.t()) :: [Air.Schemas.DataSource.t()]
  def update(cloak_info, data_sources) do
    unregister_cloak()
    register(cloak_info, data_sources)
  end

  @doc "Records cloak memory readings"
  @spec record_memory(String.t(), memory_reading) :: :ok
  def record_memory(cloak_id, reading) do
    result =
      Registry.update_value(@memory_registry_name, self(), fn state ->
        total_memory = reading[:total_memory]
        current_available = reading[:available_memory][:current]
        currently_in_use = total_memory - current_available
        percent_used = currently_in_use * 100 / total_memory

        %{
          total: total_memory,
          currently_in_use: currently_in_use,
          in_use_percent: currently_in_use * 100 / total_memory,
          readings: Enum.take([percent_used | state[:readings]], @memory_readings_to_keep)
        }
      end)

    unless result == :error do
      AirWeb.Socket.Frontend.MemoryChannel.broadcast_memory_reading(cloak_info(cloak_id))
    end

    :ok
  end

  @doc "Returns cloak info for given a cloak id"
  @spec cloak_info(String.t()) :: Map.t()
  def cloak_info(cloak_id),
    do:
      Air.Service.Cloak.all_cloak_infos()
      |> Enum.find(&(&1[:id] == cloak_id))

  @doc "Returns a list of cloak channels for a given data source. The list consists of pairs `{pid, cloak_info}`."
  @spec channel_pids(String.t()) :: [{pid(), Map.t()}]
  def channel_pids(name), do: Registry.lookup(@data_source_registry_name, name)

  @doc """
  Returns a list of the connected cloaks. The element returned for each cloak
  corresponds to the cloak info that was used to register the cloak, but is
  additionally augmented with a list of the IDs of the data sources served by the cloak
  """
  @spec all_cloak_infos() :: [Map.t()]
  def all_cloak_infos(),
    do:
      for(
        {pid, info} <- Registry.lookup(@all_cloak_registry_name, :all_cloaks),
        do: add_memory_readings(pid, info)
      )

  @doc "Returns the cloak info of cloaks serving a data source"
  @spec cloak_infos_for_data_source(String.t()) :: [Map.t()]
  def cloak_infos_for_data_source(name),
    do:
      for(
        {pid, cloak_info} <- Registry.lookup(@data_source_registry_name, name),
        do: add_memory_readings(pid, cloak_info)
      )

  @doc "Returns the list of queries running on all connected cloaks."
  @spec running_queries() :: [String.t()]
  def running_queries(),
    do:
      Registry.lookup(@all_cloak_registry_name, :all_cloaks)
      |> Stream.map(fn {pid, _} -> pid end)
      |> Enum.map(&Task.async(fn -> AirWeb.Socket.Cloak.MainChannel.running_queries(&1) end))
      |> Stream.map(&Task.await/1)
      |> Stream.filter(&match?({:ok, _}, &1))
      |> Enum.flat_map(fn {:ok, query_ids} -> query_ids end)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_call({:register, cloak_info, data_sources}, _from, state) do
    data_sources_by_name =
      data_sources
      |> Enum.map(&{&1.name, &1})
      |> Enum.into(%{})

    data_source_schemas =
      data_sources
      |> add_error_on_conflicting_data_source_definitions()
      |> add_error_on_different_salts(cloak_info)
      |> combined_data_source_errors(cloak_info)
      |> register_data_sources()

    cloak_info =
      Map.merge(cloak_info, %{
        data_sources: data_sources_by_name,
        memory: %{}
      })

    {:reply, {Map.keys(data_sources_by_name), cloak_info, data_source_schemas}, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp unregister_cloak() do
    Registry.unregister(@all_cloak_registry_name, :all_cloaks)

    for data_source_name <- Registry.keys(@data_source_registry_name, self()),
        do: Registry.unregister(@data_source_registry_name, data_source_name)
  end

  defp add_error(error, data_source) do
    existing_errors = Map.get(data_source, :errors, [])
    Map.put(data_source, :errors, [error | existing_errors])
  end

  defp add_error_on_conflicting_data_source_definitions(data_sources) do
    for data_source <- data_sources do
      name = data_source.name
      tables = data_source.tables

      existing_definitions_for_data_source_by_cloak(name)
      |> Enum.map(fn {_cloak_name, data_source} -> data_source end)
      |> Enum.reject(&is_nil/1)
      |> Enum.all?(&(tables == &1.tables))
      |> if do
        data_source
      else
        ("The data source definition for data source `#{name}` differs between the different cloaks. Please ensure " <>
           "the configurations for the data source are identical, across all the cloaks configured to serve the dataset.")
        |> add_error(data_source)
      end
    end
  end

  defp add_error_on_different_salts(data_sources, cloak_info) do
    for data_source <- data_sources do
      data_source.name
      |> cloak_infos_for_data_source()
      |> Enum.map(& &1[:salt_hash])
      |> Enum.any?(&(&1 != cloak_info[:salt_hash]))
      |> if do
        ("The data source `#{data_source.name}` is served by multiple cloaks that have different salts configured. " <>
           "In order to ensure consistent results, please ensure that the same salt is set for cloaks serving " <>
           "identical data sources.")
        |> add_error(data_source)
      else
        data_source
      end
    end
  end

  defp combined_data_source_errors(data_sources, cloak_info) do
    for data_source <- data_sources do
      data_source_name = data_source.name

      combined_errors =
        [{cloak_info.name, data_source}]
        |> Enum.concat(existing_definitions_for_data_source_by_cloak(data_source_name))
        |> Enum.flat_map(&named_errors(&1))

      Map.put(data_source, :errors, combined_errors)
    end
  end

  defp named_errors({cloak_name, data_source}),
    do:
      data_source
      |> Map.get(:errors, [])
      |> Enum.map(&"On cloak #{cloak_name}: #{&1}")

  defp existing_definitions_for_data_source_by_cloak(name),
    do:
      all_cloak_infos()
      |> Enum.map(&{&1.name, Map.get(&1.data_sources, name)})
      |> Enum.reject(&match?({_name, nil}, &1))

  defp register_data_sources(data_sources),
    do:
      Enum.map(data_sources, fn data_source ->
        errors = Map.get(data_source, :errors, [])

        DataSource.create_or_update_data_source(data_source.name, data_source.tables, errors)
      end)

  defp add_memory_readings(pid, cloak_info) do
    case Registry.lookup(@memory_registry_name, pid) do
      [{_, memory}] -> Map.put(cloak_info, :memory, memory)
      [] -> Map.put(cloak_info, :memory, [])
    end
  end

  defp initial_memory_readings(),
    do: %{
      total: 0,
      currently_in_use: 0,
      in_use_percent: 0,
      readings: List.duplicate(0, @memory_readings_to_keep)
    }

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        ChildSpec.gen_server(__MODULE__, [], name: @serializer_name),
        ChildSpec.registry(:duplicate, @data_source_registry_name),
        ChildSpec.registry(:unique, @memory_registry_name),
        ChildSpec.registry(:duplicate, @all_cloak_registry_name)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end
end
