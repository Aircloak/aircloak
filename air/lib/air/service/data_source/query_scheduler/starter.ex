defmodule Air.Service.DataSource.QueryScheduler.Starter do
  @moduledoc "Starter of pending queries."

  alias Air.Service.{Cloak, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the pending queries on the connected cloaks.

  This function tries to distribute currently pending queries over the connected cloaks. The function takes the
  snapshot of the connected cloaks immediately, and doesn't handle cloak connects/disconnects which might happen in
  the meantime. If another cloak joins while this function is running, it's ignored. If a cloak disconnects, we'll
  still try to start a query on it. In this case, the query will fail due to a disconnect.
  """
  @spec run() :: :ok
  def run() do
    {expired, pending} = Enum.split_with(Query.awaiting_start(), &expired?/1)

    pending
    # shuffling the cloak infos to improve distribution
    |> Stream.scan(Enum.shuffle(Cloak.all_cloak_infos()), &try_query_start(&2, &1))
    |> Stream.run()

    Enum.each(
      expired,
      &Query.Lifecycle.query_died(&1.id, "The query could not be started because there was no cloak available.")
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp expired?(query) do
    expiry_time = NaiveDateTime.add(NaiveDateTime.utc_now(), -:timer.hours(24), :millisecond)
    NaiveDateTime.compare(query.inserted_at, expiry_time) != :gt
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
            Air.Service.DataSource.stop_query_on_cloak(query)
            Query.Lifecycle.query_died(query.id, "The query could not be started due to a communication timeout.")
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
    with :ok <-
           AirWeb.Socket.Cloak.MainChannel.run_query(
             cloak_info.main_channel_pid,
             query.id,
             query.user.id,
             query.statement,
             query.data_source.name,
             query.parameters["values"],
             Air.Service.View.user_views_map(query.user, query.data_source.id),
             %{
               analyst_tables: Air.Service.AnalystTable.user_analyst_tables_metadata(query.user, query.data_source_id),
               views: Air.Service.View.user_views_metadata(query.user, query.data_source_id)
             }
           ) do
      Cloak.Stats.record_query(cloak_info.id)
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
end
