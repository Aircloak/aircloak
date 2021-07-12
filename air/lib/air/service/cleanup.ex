defmodule Air.Service.Cleanup do
  @moduledoc "Services for cleaning up old data."

  import Ecto.Query

  @timeout :timer.hours(1)

  @doc "Removes old queries according to the retention in the settings."
  @spec cleanup_old_queries(Air.Settings.t(), NaiveDateTime.t()) :: :ok
  def cleanup_old_queries(settings \\ Air.Service.Settings.read(), now \\ NaiveDateTime.utc_now())
  def cleanup_old_queries(%Air.Settings{query_retention_days: :unlimited}, _now), do: :ok

  def cleanup_old_queries(%Air.Settings{query_retention_days: retention}, now) do
    keep_since = Timex.shift(now, days: -retention)

    Air.Schemas.Query
    |> where([q], q.inserted_at < ^keep_since)
    |> Air.Repo.delete_all(timeout: @timeout)

    :ok
  end

  @doc "Triggers marking queries for which processing stopped for unknown reasons as errored."
  @spec cleanup_dead_queries() :: :ok
  def cleanup_dead_queries() do
    # We need to first fetch pending queries on air, to avoid possible race conditions.
    started_on_cloak_as_seen_by_air = Air.Service.Query.started_on_cloak()
    currently_running_on_connected_cloaks = MapSet.new(Air.Service.Cloak.running_queries())

    started_on_cloak_as_seen_by_air
    |> Enum.reject(&MapSet.member?(currently_running_on_connected_cloaks, &1.id))
    |> Enum.each(&Air.Service.Query.Lifecycle.query_died(&1.id, "Query died."))

    :ok
  end

  @doc "Removes explorer analysis for tables that no longer exist"
  @spec cleanup_old_explorer_analyses() :: :ok
  def cleanup_old_explorer_analyses() do
    Air.Repo.delete_all(
      from(explorer_analysis in Air.Schemas.ExplorerAnalysis,
        where: explorer_analysis.soft_delete
      ),
      timeout: @timeout
    )

    :ok
  end

  @doc "Removes old logs."
  @spec cleanup_old_logs() :: :ok
  def cleanup_old_logs() do
    retention = Application.get_env(:air, :logs_retention_days)
    keep_since = NaiveDateTime.utc_now() |> Timex.shift(days: -retention)

    Air.Schemas.Log
    |> where([log], log.timestamp < ^keep_since)
    |> Air.Repo.delete_all(timeout: @timeout)

    :ok
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    Aircloak.ChildSpec.supervisor(
      [
        {Periodic,
         run: &cleanup_old_queries/0,
         every: :timer.hours(1),
         overlap?: false,
         initial_delay: :timer.minutes(1),
         id: :cleanup_old_queries},
        {Periodic, run: &cleanup_dead_queries/0, every: :timer.minutes(5), overlap?: false, id: :cleanup_dead_queries},
        {Periodic,
         run: &cleanup_old_logs/0,
         every: :timer.hours(12),
         overlap?: false,
         initial_delay: :timer.minutes(2),
         id: :cleanup_old_logs},
        {Periodic,
         run: &cleanup_old_explorer_analyses/0,
         every: :timer.hours(24 * 10),
         overlap?: false,
         initial_delay: :timer.minutes(3),
         id: :cleanup_old_explorer_analyses}
      ],
      name: __MODULE__,
      strategy: :one_for_one
    )
  end
end
