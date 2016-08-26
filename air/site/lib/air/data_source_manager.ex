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
  def start_link(), do: GenServer.start_link(__MODULE__, nil, name: @server)

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register_cloak(Map.t, Map.t) :: :ok
  def register_cloak(cloak_info, data_sources), do:
    GenServer.call(@server, {:register_cloak, cloak_info, data_sources})

  @doc "Returns the pids of all the phoenix channels of the cloaks that have the data source"
  @spec channel_pids(String.t) :: [pid]
  def channel_pids(data_source_id), do:
    GenServer.call(@server, {:channel_pids, data_source_id})

  @doc "Whether or not a data source is available for querying. True if it has one or more cloaks online"
  @spec available?(String.t) :: boolean
  def available?(data_source_id), do: channel_pids(data_source_id) !== []


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    Logger.info("Started the data source manager")
    state = %{
      data_source_to_cloak: Map.new(),
    }
    {:ok, state}
  end

  @doc false
  def handle_call({:register_cloak, cloak_info, data_sources}, _from, state) do
    Process.monitor(cloak_info.channel_pid)
    state = Enum.reduce(data_sources, state, fn(data_source, state_acc) ->
      register_data_source(data_source, cloak_info, state_acc)
    end)
    {:reply, :ok, state}
  end
  def handle_call({:channel_pids, data_source_id}, _from, state) do
    cloak_infos = Map.get(state.data_source_to_cloak, data_source_id, [])
    pids = Enum.map(cloak_infos, &(&1.channel_pid))
    {:reply, pids, state}
  end
  def handle_call(msg, _from, state) do
    raise "Unimplemented call: #{inspect msg}"
    {:reply, {:error, :not_implemented}, state}
  end

  @doc false
  def handle_cast(msg, state) do
    raise "Unimplemented cast: #{inspect msg}"
    {:noreply, state}
  end

  @doc false
  def handle_info({:DOWN, _ref, :process, channel_pid, _reason}, state) do
    {:noreply, remove_disconnected_cloak(channel_pid, state)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp register_data_source(data_source_data, cloak_info, %{data_source_to_cloak: data_source_to_cloak} = state) do
    create_or_update_datastore(data_source_data)
    id = data_source_data["id"]
    data_source_to_cloak = Map.update(data_source_to_cloak, id, [cloak_info],
      fn(cloak_infos) -> [cloak_info | cloak_infos] end)
    %{state | data_source_to_cloak: data_source_to_cloak}
  end

  defp create_or_update_datastore(data) do
    params = %{
      unique_id: data["id"],
      name: data["name"],
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
    filtered_cloak_infos = Enum.reject(cloak_infos, fn(cloak_info) ->
      cloak_info.channel_pid === channel_pid
    end)
    if filtered_cloak_infos === [], do: Logger.info("Data source #{unique_id} is now unavailable")
    {unique_id, filtered_cloak_infos}
  end
end
