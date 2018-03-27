defmodule Air.Service.Cleanup do
  @moduledoc "Services for cleaning up old data."

  import Ecto.Query

  @doc "Removes old queries according to the retention in the settings."
  @spec cleanup_old_queries(Air.Settings.t(), NaiveDateTime.t()) :: :ok
  def cleanup_old_queries(settings \\ Air.Service.Settings.read(), now \\ NaiveDateTime.utc_now())
  def cleanup_old_queries(%Air.Settings{query_retention_days: :unlimited}, _now), do: :ok

  def cleanup_old_queries(%Air.Settings{query_retention_days: retention}, now) do
    keep_since = Timex.shift(now, days: -retention)

    Air.Schemas.Query
    |> where([q], q.inserted_at < ^keep_since)
    |> Air.Repo.delete_all()

    :ok
  end

  @doc "Triggers marking queries for which processing stopped for unknown reasons as errored."
  @spec cleanup_dead_queries() :: :ok
  def cleanup_dead_queries() do
    # We need to first fetch pending queries on air, to avoid possible race conditions.
    currently_running_as_seen_by_air = Air.Service.Query.currently_running()
    currently_running_on_connected_cloaks = MapSet.new(Air.Service.Cloak.running_queries())

    currently_running_as_seen_by_air
    |> Enum.reject(&MapSet.member?(currently_running_on_connected_cloaks, &1.id))
    |> Enum.each(&Air.Service.Query.Lifecycle.query_died(&1.id))

    :ok
  end
end
