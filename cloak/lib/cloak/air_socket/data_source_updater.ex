defmodule Cloak.AirSocket.DataSourceUpdater do
  @moduledoc """
  The process which contains the datasource information which must be sent to Air over the socket.

  The process keeps the most recent datasource information. The process also listens to changes in datasources,
  recomputes the new information, and notifies the air socket process about the change.
  """
  use GenServer
  import Aircloak, only: [in_env: 1]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the most recent datasource info."
  @spec data_sources() :: map
  def data_sources(), do: GenServer.call(__MODULE__, :data_sources)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(parent_pid) do
    Cloak.DataSource.subscribe_to_changes()
    {:ok, %{data_sources: data_sources_info(Cloak.DataSource.all()), socket_pid: nil, parent_pid: parent_pid}}
  end

  @impl GenServer
  def handle_call(:data_sources, _from, state), do: {:reply, state.data_sources, state}

  def handle_call(:force_refresh, _from, state),
    do: {:reply, :ok, update_data_sources(state, data_sources_info(Cloak.DataSource.all()))}

  @impl GenServer
  def handle_info({:data_sources_changed, new_data_sources}, state),
    do: {:noreply, update_data_sources(state, data_sources_info(new_data_sources))}

  def handle_info(other, state), do: super(other, state)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # We're periodically force refreshing, because datasource info contains other pieces of information, such as
  # isolated status, which are not propagated through the datasource change notification mechanism.
  # Using this periodic refresh, allows us to propagate such changes. This approach also reduces the possible chatter,
  # because we don't need to frequently propagate all datasources for such small changes. For example, if isolated
  # property is computed for 10 columns in a single second, we'll send just one update to air.
  defp force_refresh(), do: GenServer.call(__MODULE__, :force_refresh, round(0.9 * force_refresh_interval()))

  defp force_refresh_interval(), do: in_env(dev: :timer.seconds(10), else: :timer.seconds(30))

  defp data_sources_info(data_sources), do: Enum.map(data_sources, &data_source_info/1)

  defp data_source_info(data_source) do
    %{
      name: data_source.name,
      tables: Enum.map(data_source.tables, &table_info(data_source, &1)),
      errors: data_source.errors
    }
  end

  defp table_info(data_source, {id, table}),
    do: %{id: id, columns: Enum.map(table.columns, &column_info(data_source, table, &1))}

  defp column_info(data_source, table, column) do
    %{
      name: column.name,
      type: column.type,
      user_id: column.name == table.user_id,
      isolated_computed?: Cloak.DataSource.Isolators.computed?(data_source, table.name, column.name)
    }
  end

  defp update_data_sources(%{data_sources: data_sources} = state, data_sources), do: state

  defp update_data_sources(state, data_sources) do
    # We're fetching the sibling socket process directly from the parent supervisor. This simplifies the implementation,
    # and it's chosen in this case, since this process is tightly coupled with its sibling.
    for {Cloak.AirSocket, socket_pid, _, _} <- Supervisor.which_children(state.parent_pid),
        do: send(socket_pid, {:data_sources_changed, data_sources})

    %{state | data_sources: data_sources}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: GenServer.start_link(__MODULE__, self(), name: __MODULE__)

  @doc false
  def child_spec(arg) do
    Aircloak.ChildSpec.supervisor(
      [
        super(arg),
        {Periodic, id: :force_refresh, run: &force_refresh/0, every: force_refresh_interval(), overlapped?: false}
      ],
      strategy: :one_for_one
    )
  end
end
