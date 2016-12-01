defmodule Air.DataSourceManager do
  @moduledoc """
  The DataSourceManager holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  alias Air.{Repo, Schemas.DataSource}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register_cloak(Map.t, Map.t) :: :ok
  def register_cloak(cloak_info, data_sources) do
    data_source_ids = register_data_sources(cloak_info, data_sources)
    :gproc.reg({:p, :l, {__MODULE__, :cloak}}, Map.put(cloak_info, :data_source_ids, data_source_ids))
    :ok
  end


  @doc "Returns the pids of all the phoenix channels of the cloaks that have the data source"
  @spec channel_pids(String.t) :: [pid()]
  def channel_pids(global_id), do:
    :gproc.lookup_pids({:p, :l, {__MODULE__, :data_source, global_id}})

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
    for {_pid, cloak_info} <- :gproc.lookup_values({:p, :l, {__MODULE__, :cloak}}), do: cloak_info


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp register_data_sources(cloak_info, data_sources) do
    data_source_ids =
      data_sources
      |> Enum.map(&Task.async(fn ->
            global_id = Map.fetch!(&1, "global_id")

            # locking on a local node to prevent two simultaneous db registrations of the same datasource
            :global.trans(
              {{__MODULE__, :create_or_update_datastore, global_id}, self()},
              fn -> create_or_update_datastore(global_id, &1) end,
              [node()]
            )

            global_id
          end))
      |> Enum.map(&Task.await/1)

    Enum.each(data_source_ids, &:gproc.reg({:p, :l, {__MODULE__, :data_source, &1}}, cloak_info))

    data_source_ids
  end

  defp create_or_update_datastore(global_id, data) do
    case Repo.get_by(DataSource, global_id: global_id) do
      nil ->
        params = %{
          global_id: global_id,
          name: global_id,
          tables: Poison.encode!(Map.fetch!(data, "tables")),
        }
        %DataSource{}
        |> DataSource.changeset(params)
        |> Repo.insert!()

      data_source ->
        params = %{
          tables: Poison.encode!(Map.fetch!(data, "tables")),
        }
        data_source
        |> DataSource.changeset(params)
        |> Repo.update!()
    end
  end
end
