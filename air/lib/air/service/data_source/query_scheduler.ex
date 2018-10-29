defmodule Air.Service.DataSource.QueryScheduler do
  @moduledoc "Synchronous starting of queries"

  use Parent.GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Notifies the scheduler that some queries are pending to be started."
  @spec notify() :: :ok
  def notify(), do: GenServer.cast(__MODULE__, :notify)

  # -------------------------------------------------------------------
  # GenServer and Parent.GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil), do: {:ok, %{changed?: false}}

  @impl GenServer
  def handle_cast(:notify, state), do: {:noreply, maybe_start_queries(%{state | changed?: true})}

  @impl GenServer
  def handle_call(:sync, _from, state) do
    with {:ok, query_starter_pid} <- Parent.GenServer.child_pid(:query_starter) do
      mref = Process.monitor(query_starter_pid)

      receive do
        {:DOWN, ^mref, _, _, _} -> :ok
      end
    end

    {:reply, :ok, state}
  end

  @impl GenServer
  # this clause handles normal exits of parallel tasks which are started when ecto preloads associations
  def handle_info({:EXIT, _pid, :normal}, state), do: {:noreply, :ok, state}

  @impl Parent.GenServer
  def handle_child_terminated(:query_starter, _meta, _pid, _reason, state), do: {:noreply, maybe_start_queries(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @doc false
  # Needed in tests to ensure synchronism
  def sync(), do: GenServer.call(__MODULE__, :sync)

  defp maybe_start_queries(state) do
    if state.changed? and not Parent.GenServer.child?(:query_starter) do
      Parent.GenServer.start_child(%{id: :query_starter, start: {Task, :start_link, [&start_pending_queries/0]}})
      %{state | changed?: false}
    else
      state
    end
  end

  # This function tries to distribute currently pending queries over the available cloaks. The function takes the
  # snapshot of the available cloaks immediately, and doesn't handle cloak connects/disconnects which might happen in
  # the meantime. If another cloak joins while this function is running, it's ignored. If a cloak disconnects, we'll
  # still try to start a query on it. In this case, the query will fail due to a disconnect.
  defp start_pending_queries() do
    Enum.reduce(
      Air.Service.Query.awaiting_start(),
      # shuffling the cloak infos to improve distribution
      Enum.shuffle(Air.Service.Cloak.all_cloak_infos()),
      &try_query_start(&2, &1)
    )
  end

  defp try_query_start(cloak_infos, query) do
    case pop_cloak(cloak_infos, query.data_source) do
      nil ->
        cloak_infos

      {cloak_info, remaining_cloak_infos} ->
        query
        |> start_query(cloak_info)
        |> case do
          :ok ->
            # put the cloak at the end of the list to ensure fair spread of queries over available cloaks
            remaining_cloak_infos ++ [cloak_info]

          {:error, :too_many_queries} ->
            # this cloak is at max capacity, so we won't try it again in this iteration
            try_query_start(remaining_cloak_infos, query)

          {:error, :timeout} ->
            # timeout (likely a disconnect) -> we'll report an error, and won't try this cloak again in this iteration
            report_start_timeout(query)
            remaining_cloak_infos
        end
    end
  end

  defp pop_cloak([], _data_source), do: nil

  defp pop_cloak([cloak_info | rest_infos], data_source) do
    if has_data_source?(cloak_info, data_source),
      do: {cloak_info, rest_infos},
      else: with({found, rest_infos} <- pop_cloak(rest_infos, data_source), do: {found, [cloak_info | rest_infos]})
  end

  defp has_data_source?(cloak_info, data_source), do: Map.has_key?(cloak_info.data_sources, data_source.name)

  defp start_query(query, cloak_info) do
    with :ok <- AirWeb.Socket.Cloak.MainChannel.run_query(cloak_info.main_channel_pid, cloak_query_map(query)) do
      Air.Service.Cloak.Stats.record_query(cloak_info.id)
      query = add_cloak_info_to_query(query, cloak_info.id)
      AirWeb.Socket.Frontend.UserChannel.broadcast_state_change(query)
      Air.Service.AuditLog.log(query.user, "Executed query", Air.Schemas.Query.audit_meta(query))
      :ok
    end
  end

  defp add_cloak_info_to_query(query, cloak_id) do
    query
    |> Air.Schemas.Query.changeset(%{cloak_id: cloak_id, query_state: :started})
    |> Air.Repo.update!()
  end

  defp cloak_query_map(query) do
    %{
      id: query.id,
      statement: query.statement,
      data_source: query.data_source.name,
      parameters: query.parameters["values"],
      views: Air.Service.View.user_views_map(query.user, query.data_source.id)
    }
  end

  defp report_start_timeout(query) do
    Air.Service.Query.Events.trigger_result(%{
      query_id: query.id,
      error: "The query could not be started due to a communication timeout."
    })

    Air.Service.DataSource.stop_query_async(query)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
