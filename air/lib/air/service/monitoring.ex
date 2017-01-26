defmodule Air.Service.Monitoring do
  import Ecto.Query

  alias Air.{Repo, Schemas.Group, Schemas.User, Schemas.DataSource, Schemas.Query, DataSourceManager}

  @miliseconds_in_second 1000

  def assemble_info(now \\ NaiveDateTime.utc_now()) do
    %{
      uptime: fetch_uptime(),
      groups: fetch_group_names(),
      users: fetch_users(),
      data_sources: fetch_data_sources(now),
      cloaks: fetch_cloaks(now),
    }
  end

  defp fetch_group_names(), do:
    Group |> select([g], g.name) |> Repo.all()

  defp fetch_uptime() do
    {total, _since_last_call} = :erlang.statistics(:wall_clock)
    div(total, @miliseconds_in_second)
  end

  defp fetch_users() do
    for user <- User |> preload(:groups) |> Repo.all() do
      %{
        name: user.name,
        email: user.email,
        groups: user.groups |> Enum.map(&(&1.name))
      }
    end
  end

  defp fetch_cloaks(now) do
    for cloak <- DataSourceManager.cloaks() do
      %{
        name: cloak.name,
        uptime: Timex.diff(now, cloak.online_since, :seconds),
        data_sources: cloak.data_source_ids,
        queries: query_stats(Query |> where([q], q.cloak_id == ^cloak.id), now),
      }
    end
  end

  defp fetch_data_sources(now) do
    for data_source <- DataSource |> Repo.all() do
      %{
        id: data_source.global_id,
        name: data_source.name,
        queries: query_stats(Query |> where([q], q.data_source_id == ^data_source.id), now)
      }
    end
  end

  defp query_stats(queries, now) do
    %{
      last_5_minutes: queries |> where([q], q.inserted_at > ^Timex.shift(now, minutes: -5)) |> count(),
      last_15_minutes: queries |> where([q], q.inserted_at > ^Timex.shift(now, minutes: -15)) |> count(),
      last_30_minutes: queries |> where([q], q.inserted_at > ^Timex.shift(now, minutes: -30)) |> count(),
      last_1_hour: queries |> where([q], q.inserted_at > ^Timex.shift(now, hours: -1)) |> count(),
      last_1_day: queries |> where([q], q.inserted_at > ^Timex.shift(now, days: -1)) |> count(),
    }
  end

  defp count(queryable), do: queryable |> select([x], count(x.id)) |> Repo.one!()
end
