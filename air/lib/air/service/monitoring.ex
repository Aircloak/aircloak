defmodule Air.Service.Monitoring do
  @moduledoc "Service gathering information for the monitoring endpoint."

  import Ecto.Query

  alias Air.{Repo, Schemas.Group, Schemas.User, Schemas.Query, Service.Cloak}
  alias Air.{Schemas, Service}

  @miliseconds_in_second 1000

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a structure describing the health of the application. See user_guides/monitoring.md for details."
  @spec assemble_info(NaiveDateTime.t()) :: map()
  def assemble_info(now \\ NaiveDateTime.utc_now()) do
    %{
      version: fetch_version(),
      uptime: fetch_uptime(),
      groups: fetch_group_names(),
      users: fetch_users(),
      data_sources: fetch_data_sources(now),
      cloaks: fetch_cloaks(now)
    }
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp fetch_version(), do: Aircloak.Version.for_app(:air) |> Aircloak.Version.to_string()

  defp fetch_group_names(), do: Group |> select([g], g.name) |> Repo.all()

  defp fetch_uptime() do
    {total, _since_last_call} = :erlang.statistics(:wall_clock)
    div(total, @miliseconds_in_second)
  end

  defp fetch_users() do
    for user <- User |> preload(:groups) |> Repo.all() do
      %{
        name: user.name,
        email: user.email,
        groups: user.groups |> Enum.map(& &1.name)
      }
    end
  end

  defp fetch_cloaks(now) do
    for cloak_info <- Cloak.all_cloak_infos() do
      %{
        name: cloak_info.name,
        version: cloak_info.version,
        uptime: Timex.diff(now, cloak_info.online_since, :seconds),
        data_sources: Map.keys(cloak_info.data_sources),
        queries: query_stats(Query |> where([q], q.cloak_id == ^cloak_info.id), now),
        memory: cloak_info.memory
      }
    end
  end

  defp fetch_data_sources(now) do
    for data_source <- Service.DataSource.all() do
      %{
        id: data_source.id,
        name: data_source.name,
        queries: query_stats(Query |> where([q], q.data_source_id == ^data_source.id), now),
        groups: data_source.groups |> Enum.map(& &1.name),
        errors: Schemas.DataSource.errors(data_source)
      }
    end
  end

  defp query_stats(queries, now) do
    %{
      last_5_minutes: queries |> where([q], q.inserted_at > ^Timex.shift(now, minutes: -5)) |> count(),
      last_15_minutes: queries |> where([q], q.inserted_at > ^Timex.shift(now, minutes: -15)) |> count(),
      last_30_minutes: queries |> where([q], q.inserted_at > ^Timex.shift(now, minutes: -30)) |> count(),
      last_1_hour: queries |> where([q], q.inserted_at > ^Timex.shift(now, hours: -1)) |> count(),
      last_1_day: queries |> where([q], q.inserted_at > ^Timex.shift(now, days: -1)) |> count()
    }
  end

  defp count(queryable), do: queryable |> select([x], count(x.id)) |> Repo.one!()
end
