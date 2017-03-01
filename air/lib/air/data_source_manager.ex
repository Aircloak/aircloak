defmodule Air.DataSourceManager do
  @moduledoc """
  The DataSourceManager holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  alias Air.{Repo, Service.DataSource}

  @registry_name Module.concat(__MODULE__, Registry)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the data source manager process."
  @spec start_link() :: GenServer.on_start
  def start_link(), do:
    Registry.start_link(:duplicate, @registry_name)

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register_cloak(Map.t, Map.t) :: :ok
  def register_cloak(cloak_info, data_sources) do
    data_source_ids = register_data_sources(cloak_info, data_sources)
    Registry.register(@registry_name, :cloak, Map.put(cloak_info, :data_source_ids, data_source_ids))
    :ok
  end

  @doc "Returns pairs of the form {channel_pid, cloak_info} the cloaks that have the given data source."
  @spec channel_pids(String.t) :: [{pid(), Map.t}]
  def channel_pids(global_id), do: Registry.lookup(@registry_name, {:data_source, global_id})

  @doc "Whether or not a data source is available for querying. True if it has one or more cloaks online"
  @spec available?(String.t) :: boolean
  def available?(data_source_id), do: channel_pids(data_source_id) !== []

  @doc """
  Returns a list of the connected cloaks. The element returned for each cloak
  corresponds to the cloak info that was used to register the cloak, but is
  additionally augmented with a list of the IDs of the data sources served by the cloak
  """
  @spec cloaks() :: [Map.t]
  def cloaks(), do:
    for {_pid, cloak_info} <- Registry.lookup(@registry_name, :cloak), do: cloak_info

  @doc "Returns the cloak info of cloaks serving a data source"
  @spec cloaks_for_data_source(String.t) :: [map]
  def cloaks_for_data_source(data_source_id), do:
    Enum.filter(cloaks(), &Enum.member?(&1.data_source_ids, data_source_id))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp register_data_sources(cloak_info, data_sources) do
    data_source_ids = data_sources
    |> Enum.map(&Task.async(fn ->
      global_id = Map.fetch!(&1, "global_id")
      tables = Map.fetch!(&1, "tables")

      # locking on a local node to prevent two simultaneous db registrations of the same datasource
      :global.trans(
        {{__MODULE__, :create_or_update_datastore, global_id}, self()},
        fn -> DataSource.create_or_update_data_source(global_id, tables) end,
        [node()]
      )

      global_id
    end))
    |> Enum.map(&Task.await/1)

    Enum.each(data_source_ids, &Registry.register(@registry_name, {:data_source, &1}, cloak_info))

    data_source_ids
  end
end
