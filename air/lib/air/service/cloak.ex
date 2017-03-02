defmodule Air.Service.Cloak do
  @moduledoc """
  The Cloak service holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  use GenServer

  alias Air.Service.DataSource


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the data source manager process."
  @spec start_link() :: GenServer.on_start
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc """
  Registers a data source (if needed), and associates the calling cloak with the data source
  """
  @spec register(Map.t, Map.t) :: :ok
  def register(cloak_info, data_sources) do
    data_source_ids = register_data_sources(data_sources)
    cloak_info = Map.put(cloak_info, :data_source_ids, data_source_ids)
    GenServer.cast(__MODULE__, {:register, self(), cloak_info})
  end

  @doc "Returns pairs of the form {channel_pid, cloak_info} the cloaks that have the given data source."
  @spec channel_pids(String.t) :: [{pid(), Map.t}]
  def channel_pids(global_id), do:
    GenServer.call(__MODULE__, {:lookup, {:data_source, global_id}})

  @doc """
  Returns a list of the connected cloaks. The element returned for each cloak
  corresponds to the cloak info that was used to register the cloak, but is
  additionally augmented with a list of the IDs of the data sources served by the cloak
  """
  @spec cloaks() :: [Map.t]
  def cloaks(), do:
    for {_pid, cloak_info} <- GenServer.call(__MODULE__, {:lookup, :cloaks}), do: cloak_info

  @doc "Returns the cloak info of cloaks serving a data source"
  @spec cloaks_for_data_source(String.t) :: [map]
  def cloaks_for_data_source(data_source_id), do:
    Enum.filter(cloaks(), &Enum.member?(&1.data_source_ids, data_source_id))


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    state = %{
      cloaks: Map.new(),
    }
    {:ok, state}
  end

  def handle_cast({:register, pid, cloak_info}, %{cloaks: cloaks} = state) do
    Process.monitor(pid)
    cloaks = Map.put(cloaks, pid, cloak_info)
    {:noreply, %{state | cloaks: cloaks}}
  end

  def handle_call({:lookup, {:data_source, global_id}}, _from, %{cloaks: cloaks} = state) do
    reply = Enum.filter(cloaks, fn({_pid, %{data_source_ids: ids}}) -> Enum.any?(ids, & &1 == global_id) end)
    {:reply, reply, state}
  end
  def handle_call({:lookup, :cloaks}, _from, %{cloaks: cloaks} = state) do
    cloaks = Enum.to_list(cloaks)
    {:reply, cloaks, state}
  end

  def handle_info({:DOWN, _monitor_ref, :process, pid, _info}, %{cloaks: cloaks} = state) do
    cloaks = Map.delete(cloaks, pid)
    {:noreply, %{state | cloaks: cloaks}}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp register_data_sources(data_sources) do
    data_source_ids = data_sources
    |> Enum.map(&Task.async(fn ->
      global_id = Map.fetch!(&1, "global_id")
      tables = Map.fetch!(&1, "tables")

      # Locking on a local node to prevent two simultaneous db registrations of the same datasource.
      # The database maintains a uniqueness constraint already, but this is in place to avoid
      # unnecessary retries.
      :global.trans(
        {{__MODULE__, :create_or_update_datastore, global_id}, self()},
        fn -> DataSource.create_or_update_data_source(global_id, tables) end,
        [node()]
      )

      global_id
    end))
    |> Enum.map(&Task.await/1)

    data_source_ids
  end
end
