defmodule Air.Service.Cleanup do
  import Ecto.Query

  def cleanup_old_queries(settings, now \\ NaiveDateTime.utc_now())
  def cleanup_old_queries(%Air.Settings{query_retention_days: :unlimited}, _now), do: :ok
  def cleanup_old_queries(%Air.Settings{query_retention_days: retention}, now) do
    keep_since = Timex.shift(now, days: -retention)

    Air.Schemas.Query
    |> where([q], q.inserted_at < ^keep_since)
    |> Air.Repo.delete_all()
  end
end
