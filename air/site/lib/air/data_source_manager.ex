defmodule Air.DataSourceManager do
  @moduledoc """
  The DataSourceManager holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  use GenServer
  require Logger

  alias Air.{Repo, DataSource}

  @server {:global, __MODULE__}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the DataSourceManager which allows the air application to, among other things,
  discover whether a datastore is available for querying, and if so where.
  """
  @spec start_link() :: {:ok, pid} | {:error, term}
  def start_link(), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register_cloak(Map.t, Map.t) :: :ok
  def register_cloak(cloak_info, data_sources), do:
    GenServer.call(@server, {:register_cloak, cloak_info, data_sources})

  @doc "Attaches a monitor to the global DataSourceManager from the current process."
  @spec monitor() :: reference()
  def monitor, do:
    global_name() |> :global.whereis_name() |> Process.monitor()

  @doc "Returns the pids of all the phoenix channels of the cloaks that have the data source"
  @spec channel_pids(String.t) :: [pid]
  def channel_pids(data_source_id), do:
    GenServer.call(@server, {:channel_pids, data_source_id})

  @doc "Whether or not a data source is available for querying. True if it has one or more cloaks online"
  @spec available?(String.t) :: boolean
  def available?(data_source_id), do: channel_pids(data_source_id) !== []

  @doc """
  Returns a list of the connected cloaks. The element returned for each cloak
  corresponds to the cloak info that was used to register the cloak, but is
  additionally augmented with a list of the IDs of the data sources served by the cloak
  """
  @spec cloaks() :: [Map.t]
  def cloaks(), do: GenServer.call(@server, :cloaks)


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    state = %{data_source_to_cloak: Map.new(), authority_ref: nil}
    state = take_authority(state)

    {:ok, state}
  end

  @doc false
  def handle_call({:register_cloak, cloak_info, data_sources}, _from, state) do
    Process.monitor(cloak_info.channel_pid)
    state = Enum.reduce(data_sources, state, &register_data_source(&1, cloak_info, &2))
    {:reply, :ok, state}
  end
  def handle_call({:channel_pids, data_source_id}, _from, state) do
    cloak_infos = Map.get(state.data_source_to_cloak, data_source_id, [])
    pids = Enum.map(cloak_infos, &(&1.channel_pid))
    {:reply, pids, state}
  end
  def handle_call(:cloaks, _from, state) do
    {:reply, cloaks_from_state(state), state}
  end

  @doc false
  def handle_info({:DOWN, ref, :process, _pid, _reason}, state = %{authority_ref: ref}) do
    {:noreply, take_authority(state)}
  end
  def handle_info({:DOWN, _ref, :process, channel_pid, _reason}, state) do
    {:noreply, remove_disconnected_cloak(channel_pid, state)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp take_authority(state) do
    case :global.register_name(global_name(), self()) do
      :yes -> state
      :no -> %{state | authority_ref: monitor()}
    end
  end

  defp global_name do
    {:global, name} = @server
    name
  end

  defp register_data_source(data_source_data, cloak_info, %{data_source_to_cloak: data_source_to_cloak} = state) do
    create_or_update_datastore(data_source_data)
    id = data_source_data["id"]
    data_source_to_cloak = Map.update(data_source_to_cloak, id, [cloak_info], &([cloak_info | &1]))
    %{state | data_source_to_cloak: data_source_to_cloak}
  end

  defp create_or_update_datastore(data) do
    params = %{
      unique_id: data["id"],
      name: data["name"] || data["id"],
      tables: Poison.encode!(data["tables"]),
    }

    case Repo.get_by(DataSource, unique_id: data["id"]) do
      nil ->
        %DataSource{}
        |> DataSource.changeset(params)
        |> Repo.insert!()
      data_source ->
        data_source
        |> DataSource.changeset(params)
        |> Repo.update!()
    end
  end

  defp remove_disconnected_cloak(channel_pid, state) do
    filtered_map = state.data_source_to_cloak
    |> Enum.map(&remove_cloak_info(channel_pid, &1))
    |> Enum.into(Map.new())
    %{state | data_source_to_cloak: filtered_map}
  end

  defp remove_cloak_info(channel_pid, {unique_id, cloak_infos}) do
    filtered_cloak_infos = Enum.reject(cloak_infos, &(&1.channel_pid == channel_pid))
    if filtered_cloak_infos === [], do: Logger.info("Data source #{unique_id} is now unavailable")
    {unique_id, filtered_cloak_infos}
  end

  defp cloaks_from_state(state) do
    state.data_source_to_cloak
    |> Enum.flat_map(&invert_data_source_to_cloak/1)
    |> Enum.group_by(fn({cloak_info, _}) -> cloak_info end, fn({_, data_sources}) -> data_sources end)
    |> Enum.map(fn({cloak_info, data_source_ids}) -> Map.merge(cloak_info, %{data_source_ids: data_source_ids}) end)
  end

  defp invert_data_source_to_cloak({data_source_unique_id, cloak_infos}) do
    Enum.map(cloak_infos, &({&1, data_source_unique_id}))
  end
end
