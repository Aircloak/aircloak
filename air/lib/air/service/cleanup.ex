defmodule Air.Service.Cleanup do
  @moduledoc "Services for cleaning up old data."

  import Ecto.Query

  @doc "Removes old queries according to the retention in the settings."
  @spec cleanup_old_queries(Air.Settings.t, NaiveDateTime.t) :: :ok
  def cleanup_old_queries(settings \\ Air.Service.Settings.read(), now \\ NaiveDateTime.utc_now())
  def cleanup_old_queries(%Air.Settings{query_retention_days: :unlimited}, _now), do: :ok
  def cleanup_old_queries(%Air.Settings{query_retention_days: retention}, now) do
    keep_since = Timex.shift(now, days: -retention)

    Air.Schemas.Query
    |> where([q], q.inserted_at < ^keep_since)
    |> Air.Repo.delete_all()

    :ok
  end
end
